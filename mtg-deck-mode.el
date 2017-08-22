;;; mtg-deck-mode.el --- Major mode to edit MTG decks -*- lexical-binding: t; -*-

;; Copyright ⓒ 2015 Mattias Bengtsson
;;
;; mtg-deck-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with This program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Author: Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Version: 0.2
;; Keywords: data MTG Magic
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/mattiasb/mtg-deck-mode
;; Doc URL: https://github.com/mattiasb/mtg-deck-mode
;; Compatibility: GNU Emacs: 24.x

;;; Commentary:

;; mtg-deck-mode is a major mode for editing Magic: the Gathering decks that
;; comes with capf-completion, syntax highlighting and a very simple card search
;; via `mtg-deck-show-card'

;;; Note:

;;; Code:

(require 'subr-x)

(declare-function company-doc-buffer "company")

(defvar mtg-deck--font-lock-defaults
  '(("^[[:blank:]]*SB:"
     (0 font-lock-keyword-face))
    ("^[[:blank:]]*\\(SB:\\)?[[:blank:]]*\\([[:digit:]]*\\)"
     (2 font-lock-constant-face))
    ("^[[:blank:]]*//.*$"
     (0 font-lock-comment-face)))
  "Keyword highlighting specification for `mtg-deck-mode'.")

(defgroup mtg-deck-mode nil
  "Major mode to edit MTG decks."
  :prefix "mtg-deck-"
  :group 'wp
  :link '(url-link "https://github.com/mattiasb/mtg-deck-mode"))

(defvar mtg-deck-mode-map (make-sparse-keymap))

(defcustom mtg-deck-mode-hook nil
  "Hook called by `mtg-deck-mode'."
  :type 'hook
  :group 'mtg-deck-mode)

(defcustom mtg-deck-format 'all
  "Default `mtg-deck-mode' format."
  :group 'mtg-deck-mode
  :type '(choice (const :tag "All"      all)
                 (const :tag "Standard" standard)
                 (const :tag "Modern"   modern)
                 (const :tag "Legacy"   legacy)
                 (const :tag "Vintage"  vintage)))

(defvar mtg-deck--formats-directory
  (when load-file-name
    (concat (file-name-directory load-file-name) "formats")))

(defun mtg-deck--format-filename-prefix (format)
  "Get the path prefix to the card files of FORMAT."
  (expand-file-name (symbol-name format)
                    mtg-deck--formats-directory))

(defun mtg-deck--file-to-list (fname sep)
  "Return a list of the contents in FNAME, split by SEP."
  (when (file-exists-p fname)
    (split-string
     (with-temp-buffer
       (insert-file-contents-literally fname)
       (buffer-string))
     sep)))

(defun mtg-deck--card-names-in-format (format)
  "Return a list of all card names in FORMAT."
  (mtg-deck--file-to-list (format "%s.names"
                                  (mtg-deck--format-filename-prefix format))
                          "\n"))

(defun mtg-deck--cards-in-format (format)
  "Return a list of all cards in FORMAT."
  (mtg-deck--file-to-list (format "%s.cards"
                                  (mtg-deck--format-filename-prefix format))
                          "\n\n"))

(defun mtg-deck--name-of-card (card)
  "Get the card name out of a CARD definition."
  (car (split-string card "\n")))

(defvar mtg-deck--cards-table nil "A hash-table of all MTG cards.")
(defvar mtg-deck--num-cards 20000 "The number of available Magic Cards.")

(defun mtg-deck--make-cards-table ()
  "Make a hash-table of all cards."
  (let ((card-table (make-hash-table :size mtg-deck--num-cards
                                     :test 'equal))
        (card-list  (mtg-deck--cards-in-format 'all)))
    (dolist (card card-list card-table)
      (puthash (mtg-deck--name-of-card card) card card-table))))

(defun mtg-deck--get-card-by-name (name)
  "Get card doc info by NAME."
  (unless mtg-deck--cards-table
    (setq mtg-deck--cards-table (mtg-deck--make-cards-table)))
  (gethash name mtg-deck--cards-table))

(defun mtg-deck--company-doc-buffer (card-name)
  "Produce a `company-doc-buffer' for CARD-NAME in FORMAT."
  (company-doc-buffer (mtg-deck--get-card-by-name card-name)))

(defvar mtg-deck--line-prefix-rx
  (rx bol
      (zero-or-more blank)
      (optional (sequence "SB:"
                          (zero-or-more blank)))
      (one-or-more digit)
      (zero-or-more blank)))

(defun mtg-deck--start-of-card-point ()
  "Get the `point' where the card name of the current line start."
  (save-excursion
    (beginning-of-line)
    (when (looking-at mtg-deck--line-prefix-rx)
      (goto-char (match-end 0)))))

(defun mtg-deck--card-complete-at-point ()
  "`completion-at-point-functions' function for MTG cards."
  (let ((start (mtg-deck--start-of-card-point)))
    (when start
      (list start (point) (mtg-deck--card-names-in-format mtg-deck-format)
            :exclusive 'yes
            :company-docsig #'identity
            :company-doc-buffer #'mtg-deck--company-doc-buffer))))

(defun mtg-deck-card-at-point ()
  "The card at point."
  (save-excursion
    (beginning-of-line)
    (when (looking-at mtg-deck--line-prefix-rx)
      (goto-char (match-end 0))
      (string-trim (buffer-substring-no-properties (point)
                                                   (line-end-position))))))

(defun mtg-deck--card-buffer (card)
  "Show a CARD in a new buffer."
  (let ((buf-name (format "*MTG Card: %s*" card)))
    (with-current-buffer (get-buffer-create buf-name)
      (erase-buffer)
      (insert (mtg-deck--get-card-by-name card))
      (view-mode)
      (switch-to-buffer-other-window (current-buffer)))))

;;;###autoload
(defun mtg-deck-sideboard-toggle ()
  "Toggle the current card or region as a sideboard card."
  (interactive)
  (let* ((comment-start "SB: ")
         (region-active (region-active-p))
         (beg (if region-active (region-beginning) (line-beginning-position)))
         (end (if region-active (region-end) (line-end-position))))
    (comment-or-uncomment-region beg end)))

;;;###autoload
(defun mtg-deck-show-card-at-point ()
  "Show card at point in a new buffer."
  (interactive)
  (let ((card (mtg-deck-card-at-point)))
    (when card
      (mtg-deck--card-buffer card))))

;;;###autoload
(defun mtg-deck-show-card ()
  "Choose and show a card in a new buffer."
  (interactive)
  (let ((cards (mtg-deck--card-names-in-format mtg-deck-format)))
    (mtg-deck--card-buffer (completing-read "Card: " cards))))

;;;###autoload
(define-derived-mode mtg-deck-mode fundamental-mode "MTG Deck"
  "Major mode to edit MTG decks."
  (setq font-lock-defaults '(mtg-deck--font-lock-defaults))
  (setq-local comment-start "// ")
  (setq-local completion-ignore-case t)
  (setq-local completion-at-point-functions
              '(mtg-deck--card-complete-at-point)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.(mw)?dec\\'" . mtg-deck-mode))

(provide 'mtg-deck-mode)
;;; mtg-deck-mode.el ends here

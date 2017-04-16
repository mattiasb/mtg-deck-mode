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

;; Version: 0.1
;; Keywords: data MTG Magic
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/mattiasb/mtg-deck-mode
;; Doc URL: https://github.com/mattiasb/mtg-deck-mode
;; Compatibility: GNU Emacs: 24.x

;;; Commentary:

;;; Note:

;;; Code:

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

(defun mtg-deck--make-cards-table ()
  "Make a hash-table of all cards."
  (let ((card-table (make-hash-table :size 16000 :test 'equal))
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

(defun mtg-deck--skip-charsets-from (point charsets)
  "Go to POINT and skip all character-sets in CHARSETS.
Return sum of all skipped chars."
  (save-excursion
    (goto-char point)
    (apply '+ (mapcar #'skip-chars-forward charsets))))

(defun mtg-deck--start-of-card-point ()
  "Get the `point' where the card name of the current line start."
  (let ((line-bounds (bounds-of-thing-at-point 'line)))
    (when line-bounds
      (let* ((line-start (car line-bounds))
             (charsets   (list "[:space:]" "[:digit:]" "[:space:]"))
             (offset     (mtg-deck--skip-charsets-from line-start charsets))
             (start      (+ offset line-start)))
        start))))

(defun mtg-deck--card-complete-at-point ()
  "`completion-at-point-functions' function for MTG cards."
  (let ((start (mtg-deck--start-of-card-point)))
    (when start
      (list start (point) (mtg-deck--card-names-in-format mtg-deck-format)
            :exclusive 'yes
            :company-docsig #'identity
            :company-doc-buffer #'mtg-deck--company-doc-buffer))))

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

;;; mtg-deck-mode.el --- Major mode to edit MTG decks -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2023, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Keywords: data MTG Magic
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/mattiasb/mtg-deck-mode
;; Doc URL: https://github.com/mattiasb/mtg-deck-mode
;; Compatibility: GNU Emacs: 25.x

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

(defvar mtg-deck--card-names-table nil
  "A hash-table from format to a list of cards legal in that format.")
(defvar mtg-deck--num-formats 5
  "The number of constructed formats mtg-deck-mode knows about.")

(defun mtg-deck--card-names-in-format (format)
  "Return a list of all card names in FORMAT."
  (unless mtg-deck--card-names-table
    (setq mtg-deck--card-names-table (make-hash-table :size mtg-deck--num-formats
                                                      :test 'equal)))
  (if-let ((names (gethash format mtg-deck--card-names-table)))
      names
    (let ((names (mtg-deck--read-card-names-in-format format)))
      (puthash format names mtg-deck--card-names-table)
      names)))

(defun mtg-deck--read-card-names-in-format (format)
  "Read a list of all card names in FORMAT from disk."
  (mtg-deck--file-to-list (format "%s.names"
                                  (mtg-deck--format-filename-prefix format))
                          "\n"))

(defun mtg-deck--all-cards ()
  "Return a list of all cards in Magic."
  (mtg-deck--file-to-list (format "%s.cards"
                                  (mtg-deck--format-filename-prefix 'all))
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
        (card-list  (mtg-deck--all-cards)))
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

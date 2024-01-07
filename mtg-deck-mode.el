;;; mtg-deck-mode.el --- Major mode to edit MTG decks -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2023, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Version: 0.3
;; Keywords: data MTG Magic
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/mattiasb/mtg-deck-mode
;; Doc URL: https://github.com/mattiasb/mtg-deck-mode
;; Compatibility: GNU Emacs: 29.x

;;; Commentary:

;; mtg-deck-mode is a major mode for editing Magic: the Gathering decks that
;; comes with capf-completion, syntax highlighting and a very simple card search
;; via `mtg-deck-show-card'

;;; Note:

;;; Code:

(require 'subr-x)
(require 'url)
(require 'mm-util)

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

(defvar mtg-deck--database
  (when load-file-name
    (file-name-concat (file-name-directory load-file-name) "cards.db")))

(defun mtg-deck--query (query &optional values)
  "Run QUERY against the card database, returning the result."
  (let* ((db (sqlite-open mtg-deck--database))
         (result (sqlite-select db query values)))
    (sqlite-close db)
    result))

;;;###autoload
(defun mtg-deck-update-card-database ()
  "Update the card database from mtgjson.com."
  (interactive)
  (url-retrieve "https://mtgjson.com/api/v5/AllPrintings.sqlite.xz"
                (lambda (_)
                  (re-search-forward "\r?\n\r?\n")
                  (delete-region (point) (point-min))
                  (mm-decompress-buffer "cards.db.xz" t t)
                  (write-file mtg-deck--database))))

(defun mtg-deck--card-names-in-format (format)
  "Read a list of all card names in FORMAT from disk."
  (let* ((format-subquery (format "INNER JOIN cardLegalities ON (
                                      cards.uuid = cardLegalities.uuid
                                      AND
                                      cardLegalities.%s = 'Legal'
                                  )" (symbol-name format)))
         (query (format "SELECT DISTINCT name FROM cards
                        %s
                        ORDER BY name ASC" (if (eq format 'all)
                                               ""
                                             format-subquery))))
    (mapcar #'car (mtg-deck--query query))))

(defun mtg-deck--get-card-by-name (name)
  "Get card doc info by NAME."
  (let* ((query "SELECT DISTINCT name,manaCost,types,text FROM cards
                 WHERE name=?
                 ORDER BY name ASC")
         (result (car (mtg-deck--query query (list name)))))
    (string-join result "\n")))

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
            :company-doc-buffer #'mtg-deck--card-buffer))))

(defun mtg-deck-card-at-point ()
  "The card at point."
  (save-excursion
    (beginning-of-line)
    (when (looking-at mtg-deck--line-prefix-rx)
      (goto-char (match-end 0))
      (string-trim (buffer-substring-no-properties (point)
                                                   (line-end-position))))))

(defun mtg-deck--card-buffer (card-name)
  (with-current-buffer (get-buffer-create (format "*MTG Card: %s*" card-name))
    (fundamental-mode)
    (erase-buffer)
    (save-excursion
      (insert (mtg-deck--get-card-by-name card-name))
      (view-mode))
    (current-buffer)))

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
  (if-let ((card-name (mtg-deck-card-at-point)))
      (display-buffer (mtg-deck--card-buffer card-name))))

;;;###autoload
(defun mtg-deck-show-card ()
  "Choose and show a card in a new buffer."
  (interactive)
  (let* ((cards (mtg-deck--card-names-in-format mtg-deck-format))
         (card-name (completing-read "Card: " cards)))
    (display-buffer (mtg-deck--card-buffer card-name))))

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

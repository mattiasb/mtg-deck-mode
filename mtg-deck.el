;;; mtg-deck.el --- Major mode to edit MTG decks -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2023, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Version          : 0.3
;; Keywords         : data MTG Magic
;; Package-Requires : ((emacs "29.1"))
;; URL              : https://github.com/mattiasb/mtg-deck-mode
;; Doc URL          : https://github.com/mattiasb/mtg-deck-mode
;; Compatibility    : GNU Emacs: 29.x

;;; Commentary:

;; `mtg-deck-mode' is a major mode for editing Magic: the Gathering decks that
;; comes with capf-completion, syntax highlighting and a very simple card search
;; via `mtg-deck-show-card'

;;; Note:

;;; Code:

(require 'subr-x)
(require 'url-handlers)
(require 'mm-util)

(defvar mtg-deck--font-lock-defaults
  '(("^[[:blank:]]*SB:"
     (0 font-lock-keyword-face))
    ("^[[:blank:]]*\\(SB:\\)?[[:blank:]]*\\([[:digit:]]*\\)"
     (2 font-lock-constant-face))
    ("^[[:blank:]]*//.*$"
     (0 font-lock-comment-face)))
  "Keyword highlighting specification for `mtg-deck-mode'.")

(defgroup mtg-deck nil
  "Major mode to edit MTG decks."
  :prefix "mtg-deck-"
  :group 'wp
  :link '(url-link "https://github.com/mattiasb/mtg-deck-mode"))

(defcustom mtg-deck-mode-hook nil
  "Hook called by `mtg-deck-mode'."
  :type 'hook
  :group 'mtg-deck-mode)

(defcustom mtg-deck-card-mode-hook '(view-mode visual-line-mode)
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

(defcustom mtg-deck-database-path
  (file-name-concat user-emacs-directory "mtg-deck-cards.sqlite")
  "Where to store the card database."
  :type 'file)

(defvar mtg-deck--database-url
  "https://mtgjson.com/api/v5/AllPrintings.sqlite.xz")

(defun mtg-deck--query (query &optional values)
  "Run QUERY against the card database, returning the result.
VALUES (if non-nil) is a list or vector to be interpolated into a
parameterized statement."
  (let* ((db (sqlite-open mtg-deck-database-path))
         (result (sqlite-select db query values)))
    (sqlite-close db)
    result))

;;;###autoload
(defun mtg-deck-open-db ()
  "Open the card database with `sqlite-mode-open-file'."
  (interactive)
  (sqlite-mode-open-file mtg-deck-database-path))

;;;###autoload
(defun mtg-deck-update-card-database (&optional force)
  "Update the card database from mtgjson.com if it doesn't exist.
When called with a FORCE prefix argument forcibly update the database."
  (interactive "P")
  (when (or force (not (file-exists-p mtg-deck-database-path)))
    (let ((magic-mode-alist nil))
      (with-temp-buffer
        (url-insert-file-contents mtg-deck--database-url)
        (mm-decompress-buffer "cards.db.xz" t t)
        (write-file mtg-deck-database-path)))))

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
         (result (car (mtg-deck--query query (list name))) ))
    (string-replace "\\n" "\n"
                    (string-join (seq-filter #'identity result)
                                 "\n"))))

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

(defun mtg-deck-completion-at-point ()
  "CAPF for MTG cards."
  (let ((start (mtg-deck--start-of-card-point)))
    (when start
      (list start (point) (mtg-deck--card-names-in-format mtg-deck-format)
            :exclusive 'yes
            :company-docsig #'identity
            :company-doc-buffer #'mtg-deck--card-buffer
            :exit-function #'mtg-deck--capf-exit-function))))

(defun mtg-deck--capf-exit-function (_ status)
  "What to do after completion with STATUS.
See: `completion-extra-properties' for more information."
  (if  (forward-char) (newline)))

(defun mtg-deck-card-at-point ()
  "The card at point."
  (save-excursion
    (beginning-of-line)
    (when (looking-at mtg-deck--line-prefix-rx)
      (goto-char (match-end 0))
      (string-trim (buffer-substring-no-properties (point)
                                                   (line-end-position))))))

(defun mtg-deck-num-cards ()
  "Sum number of cards in region or buffer."
  (let* ((start        (if (region-active-p) (region-beginning) (point-min)))
         (end          (if (region-active-p) (region-end)       (point-max)))
         (tokens       (split-string (buffer-substring start end)))
         (numbers      (seq-map #'string-to-number tokens)))
    (seq-reduce #'+ numbers 0)))

(defun mtg-deck-count-cards ()
  "Count the number of cards in the region from START to END."
  (interactive)
  (message (format "%d" (mtg-deck-num-cards))))

(defun mtg-deck-insert-card-count ()
  "Insert card count for region or buffer."
  (interactive)
  (let ((cards (mtg-deck-num-cards)))
    (save-excursion
      (goto-char (if (region-active-p) (region-end) (point-max)))
      (insert (format "// %d" cards)))))

(defun mtg-deck--card-buffer (card-name)
  "Create a buffer showing CARD-NAME."
  (with-current-buffer (get-buffer-create (format "*MTG Card: %s*" card-name))
    (fundamental-mode)
    (erase-buffer)
    (save-excursion
      (insert (mtg-deck--get-card-by-name card-name))
      (mtg-deck-card-mode))
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
(defun mtg-deck-show-card (card-name)
  "Choose and show CARD-NAME in a new buffer."
  (interactive
   (list (completing-read "Card: "
                          (mtg-deck--card-names-in-format mtg-deck-format))))
  (display-buffer (mtg-deck--card-buffer card-name)))

;;;###autoload
(defun mtg-deck-sort-by-name (p1 p2)
  "Sort region (from P1 to P2) by card name."
  (interactive "r")
  (let ((beg (save-excursion (goto-char (min p1 p2))
                             (line-beginning-position)))
        (end (save-excursion (goto-char (max p1 p2))
                             (line-end-position))))
    (call-process-region beg end "sort" t t nil "-f" "-k2")))

;;;###autoload
(define-derived-mode mtg-deck-card-mode fundamental-mode "MTG Deck Card")

;;;###autoload
(define-derived-mode mtg-deck-mode fundamental-mode "MTG Deck"
  "Major mode to edit MTG decks."
  (setq font-lock-defaults '(mtg-deck--font-lock-defaults))
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+ *")
  (setq-local completion-ignore-case t)
  (setq-local completion-at-point-functions
              '(mtg-deck-completion-at-point))
  (unless (file-exists-p mtg-deck-database-path)
    (message
     "Run `M-x mtg-deck-update-card-database' to retrieve a card database!")))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.(mw)?dec\\'" . mtg-deck-mode))

(provide 'mtg-deck)
;;; mtg-deck.el ends here

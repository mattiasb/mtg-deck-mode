;;; mtg-deck.el --- Edit Magic: the Gathering decks -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2023, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 0.3
;; Keywords         : data MTG Magic
;; Package-Requires : ((emacs "29.4"))
;; URL              : https://github.com/mattiasb/mtg-deck-mode
;; Doc URL          : https://github.com/mattiasb/mtg-deck-mode
;; Compatibility    : GNU Emacs: 29.x

;;; Commentary:

;; This package contains a major mode for editing Magic: the Gathering decks.
;; `mtg-deck-mode' comes with a completion at point implementation for cards,
;; syntax highlighting and card search and view via the `mtg-card' package.

;;; Note:

;;; Code:

(require 'subr-x)
(require 'url-handlers)

(require 'mtg-card)

(defvar mtg-deck--font-lock-defaults
  '(("^[[:blank:]]*SB:"
     (0 font-lock-keyword-face))
    ("^[[:blank:]]*\\(SB:\\)?[[:blank:]]*\\([[:digit:]]*\\)"
     (2 font-lock-constant-face))
    ("^[[:blank:]]*//.*$"
     (0 font-lock-comment-face)))
  "Keyword highlighting specification for `mtg-deck-mode'.")

(defgroup mtg-deck nil
  "Edit Magic: the Gathering decks."
  :prefix "mtg-deck-"
  :group 'wp
  :link '(url-link "https://github.com/mattiasb/mtg-deck-mode"))

(defcustom mtg-deck-mode-hook nil
  "Hook called by `mtg-deck-mode'."
  :type 'hook
  :group 'mtg-deck)

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
      (list start (point) (mtg-card--names-in-format mtg-format)
            :exclusive 'yes
            :company-docsig #'identity
            :company-doc-buffer #'mtg-card--create-buffer))))

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
      (display-buffer (mtg-card--create-buffer card-name))))

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
(define-derived-mode mtg-deck-mode fundamental-mode "MTG Deck"
  "Major mode to edit MTG decks."
  (setq-local font-lock-defaults '(mtg-deck--font-lock-defaults))
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+ *")
  (setq-local completion-ignore-case t)
  (setq-local completion-at-point-functions
              '(mtg-deck--card-complete-at-point))
  (unless (file-exists-p mtg-card-database-path)
    (message "Run `M-x mtg-card-database-update' to get a card database!")))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.(mw)?dec\\'" . mtg-deck-mode))

(provide 'mtg-deck)
;;; mtg-deck.el ends here

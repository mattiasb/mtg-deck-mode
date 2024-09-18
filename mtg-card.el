;;; mtg-card.el --- Search and view Magic: the Gathering cards -*- lexical-binding: t -*-

;; Copyright 2024, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 0.3
;; Keywords         : data MTG Magic
;; Package-Requires : ((emacs "29.4"))
;; URL              : https://github.com/mattiasb/mtg-deck-mode
;; Compatibility    : GNU Emacs: 29.x

;;; Commentary:

;; This package contains functions for searching for Magic: the Gathering cards
;; and a major mode for viewing them.

;;; Note:

;;; Code:

(require 'mm-util)

(defgroup mtg-card nil
  "Magic: the Gathering cards."
  :prefix "mtg-card-"
  :group 'wp
  :link '(url-link "https://github.com/mattiasb/mtg-deck-mode"))

(defcustom mtg-card-mode-hook '(view-mode visual-line-mode)
  "Hook called by `mtg-card-mode'."
  :type 'hook
  :group 'mtg-card)

(defcustom mtg-card-database-path
  (file-name-concat user-emacs-directory "mtg-cards.sqlite")
  "Where to store the card database."
  :type 'file)

(defvar mtg-card--database-url
  "https://mtgjson.com/api/v5/AllPrintings.sqlite.xz")

;;;###autoload
(define-derived-mode mtg-card-mode fundamental-mode "MTG Card")

;;;###autoload
(defun mtg-card-database-open ()
  "Open the card database with `sqlite-mode-open-file'."
  (interactive)
  (mtg-card-database-update)
  (sqlite-mode-open-file mtg-card-database-path))

;;;###autoload
(defun mtg-card-database-update (&optional force)
  "Update the card database from mtgjson.com if it doesn't exist.
When called with a FORCE prefix argument forcibly update the database."
  (interactive "P")
  (when (or force (not (file-exists-p mtg-card-database-path)))
    (let ((magic-mode-alist nil))
      (with-temp-buffer
        (url-insert-file-contents mtg-card--database-url)
        (mm-decompress-buffer "cards.db.xz" t t)
        (write-file mtg-card-database-path)))))

;;;###autoload
(defun mtg-card-show (card-name)
  "Choose and show CARD-NAME in a new buffer."
  (interactive
   (list (completing-read "Card: "
                          (mtg-card--names-in-format (or mtg-format 'all)))))
  (display-buffer (mtg-card--create-buffer card-name)))

(defun mtg-card--create-buffer (card-name)
  "Create a buffer showing CARD-NAME."
  (with-current-buffer (get-buffer-create (format "*MTG Card: %s*" card-name))
    (fundamental-mode)
    (erase-buffer)
    (save-excursion
      (insert (mtg-card--by-name card-name))
      (mtg-card-mode))
    (current-buffer)))

(defun mtg-card--database-query (query &optional values)
  "Run QUERY against the card database, returning the result.
VALUES (if non-nil) is a list or vector to be interpolated into a
parameterized statement."
  (let* ((db (sqlite-open mtg-card-database-path))
         (result (sqlite-select db query values)))
    (sqlite-close db)
    result))

(defun mtg-card--names-in-format (format)
  "Read a list of all card names in FORMAT from disk."
  ;; TODO: Look at using `with-memoization' here.
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
    (mapcar #'car (mtg-card--database-query query))))

(defun mtg-card--by-name (name)
  "Get card doc info by NAME."
  (let* ((query "SELECT DISTINCT name,manaCost,types,text FROM cards
                 WHERE name=?
                 ORDER BY name ASC")
         (result (car (mtg-card--database-query query (list name))) ))
    (string-replace "\\n" "\n"
                    (string-join (seq-filter #'identity result)
                                 "\n"))))

(provide 'mtg-card)
;;; mtg-card.el ends here

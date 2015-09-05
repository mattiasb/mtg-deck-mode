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
;; Keywords: MTG, Magic
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/mattiasb/mtg-deck-mode
;; Doc URL: https://github.com/mattiasb/mtg-deck-mode
;; Compatibility: GNU Emacs: 24.x

;;; Commentary:

;;; Note:

;;; Code:

(defvar mtg-deck--font-lock-defaults
  '(
    ("^[[:blank:]]*SB:"
     (0 font-lock-function-name-face))
    ("^[[:blank:]]*\\(SB:\\)?[[:blank:]]*\\([[:digit:]]*\\)"
     (2 font-lock-constant-face))
    ("//.*$"
     (0 font-lock-comment-face))
    )
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

;;;###autoload
(define-derived-mode mtg-deck-mode fundamental-mode "MTG Deck"
  "Major mode to edit MTG decks."
  (setq-local comment-start "// ")
  (setq-local font-lock-defaults '(mtg-deck--font-lock-defaults))
  (setq-local mode-name "MTG Deck"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.(mw)?dec\\'" . mtg-deck-mode))

(provide 'mtg-deck)
;;; mtg-deck-mode.el ends here

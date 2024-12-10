;;; eldev-emacs-init.el ---  -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2023, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20240108
;; Keywords         : local
;; Package-Requires : ((emacs "29.1" corfu))
;; URL              : https://github.com/mattiasb/mtg-deck-mode
;; Compatibility    : GNU Emacs: 29.x

;;; Commentary:

;;; Note:

;;; Code:

(tool-bar-mode   -1)
(menu-bar-mode   -1)
(scroll-bar-mode -1)

(load-theme 'wombat)

(use-package emacs
  :demand t
  :hook (emacs-startup   . fido-vertical-mode)
  :hook (emacs-startup   . windmove-default-keybindings)

  :custom
  (inhibit-startup-buffer-menu         t)
  (inhibit-startup-echo-area-message   "mattiasb")
  (inhibit-startup-screen              t)
  (initial-major-mode                  'mtg-deck-mode)
  (tab-always-indent                   'complete)
  (text-quoting-style                  'grave)

  :config
  (let ((deck '("4 Animate Dead"
                "4 Cryptic Command"
                "4 Jace, Vryn's Prodigy // Jace, Telepath Unbound"
                "4 Karakas"
                "4 Karn, the Great Creator"
                "4 Nezumi Graverobber // Nighteyes the Desecrator")))
    (setopt initial-scratch-message (string-join deck "\n"))))

(use-package corfu
  :hook (after-init      . global-corfu-mode))

(use-package mtg-deck
  :demand t
  :hook (mtg-card-mode   . visual-line-fill-column-mode)
  :hook (mtg-deck-mode   . (lambda ()
                             (mtg-card-database-update)
                             (mtg-card-show "Animate Dead")))
  :bind ( :map mtg-deck-mode-map
          ( "C-<return>" . mtg-deck-show-card-at-point)
          ( "<tab>"      . completion-at-point)))

;; (with-eval-after-load 'mtg-deck
;;   ;; (declare-function visual-fill-column-mode   "visual-fill-column")
;;   ;; (declare-function mtg-card-database-update  "mtg-card")
;;   ;; (declare-function mtg-card-show             "mtg-card")
;;   ;; (defvar mtg-deck-mode-map)

;;   ;; (define-key mtg-deck-mode-map (kbd "C-<return>")
;;   ;;             #'mtg-deck-show-card-at-point)
;;   ;; (define-key mtg-deck-mode-map (kbd "<tab>")
;;   ;;             #'completion-at-point)
;;   (add-hook 'mtg-deck-card-mode-hook #'visual-fill-column-mode)
;;   (mtg-card-database-update)
;;   (mtg-card-show "Animate Dead"))

;; (normal-mode)

(provide 'eldev-emacs-init)
;;; eldev-emacs-init.el ends here

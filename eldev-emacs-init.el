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

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(load-theme 'wombat)

(fido-vertical-mode)
(windmove-default-keybindings)

(declare-function global-corfu-mode "corfu")
(use-package corfu :config (global-corfu-mode))

(setq inhibit-startup-buffer-menu       t
      inhibit-startup-echo-area-message "mattiasb"
      inhibit-startup-screen            t
      initial-major-mode                'mtg-deck-mode
      tab-always-indent                 'complete
      text-quoting-style                'grave)

(setq initial-scratch-message
      (string-join '("4 Animate Dead"
                     "4 Cryptic Command"
                     "4 Jace, Vryn's Prodigy // Jace, Telepath Unbound"
                     "4 Karakas"
                     "4 Karn, the Great Creator"
                     "4 Nezumi Graverobber // Nighteyes the Desecrator")
                   "\n"))

(with-eval-after-load 'mtg-deck
  (declare-function mtg-deck-mode                 "mtg-deck")
  (declare-function mtg-deck-show-card            "mtg-deck")
  (declare-function mtg-deck-update-card-database "mtg-deck")

  (mtg-deck-mode)
  (mtg-deck-update-card-database)
  (mtg-deck-show-card "Animate Dead"))

(provide 'eldev-emacs-init)
;;; eldev-emacs-init.el ends here

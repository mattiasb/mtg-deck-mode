;;; mtg-deck-mode-tests.el --- Test suite for mtg-dec-mode -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright 2023, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 20231225
;; Keywords         : local
;; Package-Requires : ((emacs 29.1))
;; URL              : https://github.com/mattiasb/mtg-deck-mode
;; Compatibility    : GNU Emacs: 29.x

;;; Commentary:

;;; Note:

;;; Code:
(require 'mtg-deck-mode)

;; (ert-deftest formats-directory ()
;;   (should (string= mtg-deck--formats-directory
;;                    "/home/mattiasb/Code/github.com/mattiasb/mtg-deck-mode/formats")))

(ert-deftest database-exists ()
  (should (string= mtg-deck--database
                   "/home/mattiasb/Code/github.com/mattiasb/mtg-deck-mode/AllPrintings.sqlite")))

(ert-deftest addition-test ()
  (should (= (+ 1 2) 3)))

(provide 'mtg-deck-mode-tests)
;;; mtg-deck-mode-tests.el ends here

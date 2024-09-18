;;; mtg.el --- General Magic: the Gathering functions and data -*- lexical-binding: t -*-

;; Copyright 2024, Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author           : Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version          : 0.3
;; Keywords         : local
;; Package-Requires : ((emacs "29.4"))
;; URL              : https://github.com/mattiasb/.emacs.d
;; Compatibility    : GNU Emacs: 29.x

;;; Commentary:

;;; Note:

;; General Magic: the Gathering functions and data.

;;; Code:

(defcustom mtg-format 'all
  "Default format for `mtg-deck-mode' and `mtg-card-show'."
  ;;See: https://mtgjson.com/data-models/legalities/#typescript-model
  :group 'mtg-deck-mode
  :type '(choice (const :tag "All"             all)
                 (const :tag "Alchemy"         alchemy)
                 (const :tag "Brawl"           brawl)
                 (const :tag "Commander"       commander)
                 (const :tag "Duel"            duel)
                 (const :tag "Explorer"        explorer)
                 (const :tag "Future"          future)
                 (const :tag "Gladiator"       gladiator)
                 (const :tag "Historic"        historic)
                 (const :tag "Historicbrawl"   historicbrawl)
                 (const :tag "Legacy"          legacy)
                 (const :tag "Modern"          modern)
                 (const :tag "Oathbreaker"     oathbreaker)
                 (const :tag "Oldschool"       oldschool)
                 (const :tag "Pauper"          pauper)
                 (const :tag "Paupercommander" paupercommander)
                 (const :tag "Penny"           penny)
                 (const :tag "Pioneer"         pioneer)
                 (const :tag "Predh"           predh)
                 (const :tag "Premodern"       premodern)
                 (const :tag "Standard"        standard)
                 (const :tag "Standardbrawl"   standardbrawl)
                 (const :tag "Timeless"        timeless)
                 (const :tag "Vintage"         vintage)))

(provide 'mtg)
;;; mtg.el ends here

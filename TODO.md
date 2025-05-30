# TODO #

## Card Completion ##

- [ ] Make card completion add a newline.

## Card mode ##

- [ ] Show sub type
- [ ] Show flip- and DFC- cards.
- [ ] Limit card width.
  - Just saying "Use `visual-fill-column-mode`" won't work with
    `corfu-popupinfo-mode`.
- [ ] Make a fancy card view with:
  - Card image
  - Rules text
  - Rulings
  - Legality
  - Sets
  - Etc.
- [ ] Make card database downloads asynchronous.
  - Maybe try out [PDD][melpa-pdd].

## Bugs ##

- [ ] `mtg-deck-show-card-at-point` fails when there are comments after the card
      name.
  - This will be a little tricky to fix given `Fire // Ice` etc.

- [ ] The font-lock stuff fails with `goto-address-mode`.
- [ ] The font-lock stuff sometimes stops marking comments.

<!----------------------------------------------------------------------------->

[melpa-pdd]: https://github.com/lorniu/pdd.el

Guipher TODO
============

- [ ] Keep navigation history separate from the back/forward stack
- [ ] Keyboard shortcuts for navigation
  - Up, down, page up, page down, top, bottom
  - Backspace to go back
- [x] Bring back graceful failure when rendering menus
- [x] `h` selectors should `xdg-open` when starting with `URL:` *or* be downloaded/rendered as HTML otherwise
- [ ] Further restructure code
      Some things need to be shown, some things need to be downloaded (all binaries), and some things need to be dispatched to some external app (telnet, h-type selectors)
- [x] Restructure code:
  - [x] Protocol -> handler dispatch table
  - [x] Selector type -> (icon tag handler) dispatch table
        When handler is not #f, add both `tag` and `link` to tags.
- [ ] Clear history when jumping to a new page (both by clicking on a link *and* by writing it in the address bar)
- [x] Better creation of URIs (remove leading `/` from path when aggregating, the selector already contains it)
- [x] Better UI handling
- [x] Remove hashmap - put target in the tag, and then retrieve it
  - [x] ~~Clean up hashmap when changing page~~
- [x] Modify history to contain addresses, and not just gopher quadruplets
- [x] Rename functions handling UI to make that clear
- [x] URI "parser"
  - If no protocol, add `gopher://`
  - If no port, use 70
  - If path, try splitting on `/` and check the first item - if it's a single character, use it as the selector
- [x] Take URI from the command line *or* jump to home page
  - [ ] Allow home page to be defined
  - [ ] `about:home`? `about:guipher`?
- [x] "Gracefully" handle errors (especially poorly formatted directories like those returned by gophernicus when they can't be found)
- [ ] Binary downloader
  - Need somehow to get a path, and the Tk saveAs dialog isn't working
- [x] Handle images (inline? display when clicking?)
- [x] Parse out kind from URLs, or assume 1
- [x] History
  - [x] Ignore going back/forward with no history
  - [x] Back
  - [x] Forward
  - [x] Display history log (`about:history`?)
  - [x] Keep only URIs in history?
- [ ] Handle search (7) selectors
  - Click -> display input box w. cancel and submit?
  - Display entry box inline?
- [ ] Cache already downloaded pages
  - Cache forever/for a day unless reloading?
  - Cache for the current session unless reloading?

Graphical customization
-----------------------

- [ ] Set the font
  - [ ] Make `ch-width` take some arbitrary Tcl font
  - [ ] Different fonts for directories and text?
- [ ] Set background/foreground/link background and foreground
- [ ] Tags for each item type, allow customization of them?

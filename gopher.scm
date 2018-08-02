#!/usr/bin/env guile
;; Guile + PS/Tk Gopher client !#

(use-modules (ice-9 rdelim) (srfi srfi-1))
(load "pstk.scm")

(define (get-ip-of address)
  "Returns the IP of ADDRESS in a form that can be used by (connect ... AF_INET)."
  (car (hostent:addr-list (gethost address))))

(define (get-ipv4-of address)
  "Returns the IPv4 of ADDRESS in string form (e.g.: 127.0.0.1)."
  (let [(ip (get-ip-of address))]
    (inet-ntop AF_INET ip)))

;; TODO: this should take an arbitrary font
;; TODO: the font should be defined elsewhere
(define (ch-width)
  (let [(f (tk-eval "font create f -family {Ubuntu Mono} -size 11"))
        (w (string->number (tk-eval "font measure f {0}")))]
    (tk-eval "font delete f")
    w))

(define (tk-get-list name)
  (string-split (tk-get-var name) #\space))

(define (gopher-get address selector port)
  "Returns a list of lines containing the server's response to a Gopher request at ADDRESS:PORT for SELECTOR."
  (let [(s (socket PF_INET SOCK_STREAM 0))]
    (connect s AF_INET (get-ip-of address) port)
    (display (format #f "~a\r\n" selector) s) ; Ask for the selector
    (let loop [(lines '())]
      (let [(line (read-line s))]
        (if (not (or (eof-object? line) (string= line ".\r")))
            (loop (append lines (list line)))
            lines)))))

(define links (make-hash-table))

(define (nth n l)
  (if (or (> n (length l)) (< n 0))
    (error "Index out of bounds.")
    (if (eq? n 0)
      (car l)
      (nth (- n 1) (cdr l)))))

(define (gopher-render-line widget line)
  (let* [(kind (substring line 0 1))
         (line (string-trim-right (substring line 1) #\return))
         (pieces (string-split line #\tab))
         (description (list-ref pieces 0))
         (selector (list-ref pieces 1))
         (host (list-ref pieces 2))
         (port (string->number (list-ref pieces 3)))
         (line-tags '("default"))]
    (cond
     [(equal? kind "1")
      (widget 'image 'create 'end 'image: "img-directory")
      (set! line-tags '("link" "directory"))]
     [(equal? kind "0")
      (widget 'image 'create 'end 'image: "img-text")
      (set! line-tags '("link" "text"))]
     [else
      (set! line-tags '("no-icon"))])
    (if (> (string-length selector) 0)
        (let [(tag (format #f "LINK:~a:~a:~a" host port selector))]
          (hash-set! links tag (list host selector port))
          (set! line-tags (append line-tags (list tag)))))
    (widget 'insert 'end
           (string-concatenate (list " " (list-ref pieces 0) "\n"))
           line-tags)))

(define (gopher-render widget lines)
  (widget 'config 'state: 'normal)
  (for-each
   (lambda (line)
     (gopher-render-line widget line))
   lines)
  (widget 'config 'state: 'disabled))

(define (gopher-goto)
  (display (address-bar 'get))
  (tk/focus main-text))

(tk-start)
;; Initial window settings
(tk/wm 'title tk "Guipher")
(tk/wm 'geometry tk "640x480")
;; Load the various icons we use
(tk/image 'create 'photo "img-back" 'file: "assets/arrow_left.png")
(tk/image 'create 'photo "img-forward" 'file: "assets/arrow_right.png")
(tk/image 'create 'photo "img-directory" 'file: "assets/folder.png")
(tk/image 'create 'photo "img-text" 'file: "assets/page.png")
;; Define the tool bar
(define tool-bar
  (tk 'create-widget 'frame 'bd: 1 'relief: 'raised))
(define btn-back
  (tk 'create-widget 'button 'image: "img-back" 'relief: "flat"))
(define btn-forward
  (tk 'create-widget 'button 'image: "img-forward" 'relief: "flat"))
(define address-bar
  (tk 'create-widget 'ttk::combobox))
(tk/bind address-bar "<Return>" gopher-goto)
(tk/pack btn-back btn-forward 'side: 'left 'in: tool-bar)
(tk/pack address-bar 'side: 'left 'expand: 1 'fill: 'x 'in: tool-bar)
(tk/pack tool-bar 'fill: 'x)
;; Define the main widget
(define main-text
  (tk 'create-widget 'text 'border: 0 'relief: 'flat 'wrap: 'word
      'state: 'disabled 'font: '("Ubuntu Mono" 11)))
(tk/pack main-text 'fill: 'both 'expand: 1)
;; Define the main tags
(main-text 'tag 'config "default" 'lmargin1: 0); (+ 1 (ch-width)))
(main-text 'tag 'config "no-icon" 'lmargin1: 16)
(main-text 'tag 'config "link" 'foreground: 'blue)
(main-text 'tag 'bind "link" "<Enter>"
           (lambda () (main-text 'config 'cursor: 'hand2)))
(main-text 'tag 'bind "link" "<Leave>"
           (lambda () (main-text 'config 'cursor: "")))

(define (get-link widget x y)
  (tk-eval (format #f "set ::scmVar(tags) \"[~a tag names @~a,~a]\"" widget x y))
  ;; Apparently the following line is required - otherwise `tk-get-var`,
  ;; `tk-get-list` and the like don't work inside of `let` or `find`
  (tk-get-var 'tags)
  ;;(write links)
  (gopher-render main-text (apply gopher-get
   (hash-ref links
              (find (lambda (i) (string-prefix? "LINK:" i))
                    (tk-get-list 'tags)))
   ))
  )
(main-text 'tag 'bind "link" "<1>"
           `(,(lambda (widget x y)
                (display (format #f "Clicked ~a, ~a\n" x y))
                (get-link widget x y)) ,main-text %x %y))
;; Load the home page/address given on the command line
;; TODO: actually do it
(gopher-render main-text (gopher-get "localhost" "/" 70))
;; Start Tk
(tk-event-loop)

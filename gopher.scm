#!/usr/bin/env guile
;; Guile + PS/Tk Gopher client !#
(use-modules (ice-9 rdelim))
(load "pstk.scm")

(define (get-ip-of address)
  "Returns the IP of ADDRESS in a form that can be used by (connect ... AF_INET)."
  (car (hostent:addr-list (gethost address))))

(define (get-ipv4-of address)
  "Returns the IPv4 of ADDRESS in string form (e.g.: 127.0.0.1)."
  (let [(ip (get-ip-of address))]
    (inet-ntop AF_INET ip)))

(define (ch-width)
  (let [(f (tk-eval "font create f -family \"Ubuntu Mono\" -size 10"))
        (w (string->number (tk-eval "font measure f 0")))]
    (tk-eval "font delete f")
    w))

(define (gopher-get address selector port)
  (let [(s (socket PF_INET SOCK_STREAM 0))]
    (connect s AF_INET (get-ip-of address) port)
    (display (format #f "~a\r\n" selector) s) ; Ask for the selector
    (let loop [(lines '())]
      (let [(line (read-line s))]
        (if (not (eof-object? line))
            (loop (append lines (list line)))
            lines)))))

(define (gopher-render-line widget line)
  (let* [(kind (substring line 0 1))
         (line (substring line 1))
         (pieces (string-split line #\tab))
         (tags '("default"))]
    (cond
     [(equal? kind "1")
      (widget 'image 'create 'end 'image: "img-directory")
      (widget 'insert 'end " ")]
     [(equal? kind "0")
      (widget 'image 'create 'end 'image: "img-text")
      (widget 'insert 'end " ")]
     [else
      (set! tags '("no-icon"))])
    (widget 'insert 'end
           (string-concatenate (list (list-ref pieces 0) "\n"))
           tags)))

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
(tk/image 'create 'photo "img-back" 'file: "assets/arrow_left.png")
(tk/image 'create 'photo "img-forward" 'file: "assets/arrow_right.png")
(tk/image 'create 'photo "img-directory" 'file: "assets/folder.png")
(tk/image 'create 'photo "img-text" 'file: "assets/page.png")
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
(define main-text
  (tk 'create-widget 'text 'border: 0 'relief: 'flat 'wrap: 'word
      'state: 'disabled 'font: '("Ubuntu Mono" 10)))
(main-text 'tag 'config "default" 'lmargin1: (ch-width))
(main-text 'tag 'config "no-icon" 'lmargin1: (+ 16 (ch-width)))
(tk/pack main-text 'fill: 'both 'expand: 1)

(gopher-render main-text (gopher-get "localhost" "/" 70))

(tk/wm 'title tk "Guipher")
(tk/wm 'geometry tk "640x480")
(tk-event-loop)

#!/usr/bin/env guile
;; Guile + PS/Tk Gopher client !#
(use-modules (ice-9 rdelim) (pstk.scm))
(load "pstk.scm")

(define (get-ip-of address)
  "Returns the IP of ADDRESS in a form that can be used by (connect ... AF_INET)."
  (car (hostent:addr-list (gethost address))))

(define (get-ipv4-of address)
  "Returns the IPv4 of ADDRESS in string form (e.g.: 127.0.0.1)."
  (let [(ip (get-ip-of address))]
    (inet-ntop AF_INET ip)))

(define (gopher-goto)
  (display (address-bar 'get))
  (tk/focus main-text))

(define (gopher-to-tk-text line where)
  (let* [(kind (substring line 0 1))
         (line (substring line 1))
         (pieces (string-split line #\tab))
         (tags '("default"))]
    (cond
     [(equal? kind "1")
      (where 'image 'create 'end 'image: "img-directory")
      (where 'insert 'end " ")]
     [(equal? kind "0")
      (where 'image 'create 'end 'image: "img-text")
      (where 'insert 'end " ")]
     [else
      (set! tags '("no-icon"))])
    (where 'insert 'end
           (string-concatenate (list (list-ref pieces 0) "\n"))
           tags)))

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
(define (ch-width)
  (let [(f (tk-eval "font create f -family \"Ubuntu Mono\" -size 10"))
        (w (string->number (tk-eval "font measure f 0")))]
    (tk-eval "font delete f")
    w))
(main-text 'tag 'config "default" 'lmargin1: (ch-width))
(main-text 'tag 'config "no-icon" 'lmargin1: (+ 16 (ch-width)))
(tk/pack main-text 'fill: 'both 'expand: 1)

(let [(s (socket PF_INET SOCK_STREAM 0))
      (address "localhost")
      (port 70)
      (selector "/")]
  (connect s AF_INET (get-ip-of address) port)
  (display (format #f "~a\r\n" selector) s)       ; Send the wanted selector
  (main-text 'config 'state: 'normal)
  (do [(line (read-line s) (read-line s))]        ; Read and display the response
      [(or (eof-object? line) (equal? line "."))]
    (gopher-to-tk-text line main-text))
  (main-text 'config 'state: 'disabled))

(tk/wm 'title tk "Guipher")
(tk/wm 'geometry tk "640x480")
(tk-event-loop)

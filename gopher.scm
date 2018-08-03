#!/usr/bin/env guile
;; Guile + PS/Tk Gopher client !#

(use-modules (ice-9 rdelim) (ice-9 regex) (srfi srfi-1) (web uri))
(load "pstk.scm")

(declare-default-port! "gopher" 70)
(define default-protocol "gopher://")

(define (gopher-handler string uri)
  (debug "Gopher jump-to ~a" (uri-to-quadruplet uri))
  (gopher-goto (uri-to-quadruplet uri))
  ;; Add element to browsing history
  (set! hist-back (cons string hist-back)))

(define (about-handler string uri)
  (debug "About jump-to ~a" (uri-path uri))
  (cond
   [(equal? (uri-path uri) "blank")
    (debug "YOYOYOYOYO")
    (main-text 'config 'state: 'normal)
    (main-text 'delete '1.0 'end)
    (main-text 'config 'state: 'disabled)]
   [(equal? (uri-path uri) "history")
    (main-text 'config 'state: 'normal)
    (main-text 'delete '1.0 'end)
    (for-each
     (lambda (line)
       (main-text 'insert 'end (string-append line "\n")))
     hist-back)
    (main-text 'config 'state: 'disabled)]))

(define protocol-handlers
  (list
   (cons 'gopher gopher-handler)
   (cons 'about about-handler)))

(define (default-handler string uri)
  (debug "Unknown protocol ~a\n" (uri-scheme uri)))

(define (dispatch-by-uri string)
  (debug "dispatch-by-uri invoked w. ~a" string)
  (let [(parsed (string->uri string))]
    (if (not parsed)
        ;; Not parsed, likely missing the protocol - add it and try
        ;; again, *once*, throwing an error on the second attempt
        (if (not (string-prefix? default-protocol string))
            (dispatch-by-uri (string-append default-protocol string))
            (display "Malformed URI\n"))
        ;; Handle the URI with the proper function
        (let [(handler (assoc (uri-scheme parsed) protocol-handlers))]
          (if handler
              ((cdr handler) string parsed)
              (default-handler parsed))))))

(define (uri-to-quadruplet uri)
  (let [(host (uri-host uri))
        (port (or (uri-port uri) 70))
        (path (uri-path uri))]
    ;; Check to see if there is a possible selector in the path
    (if (string-match "/[0-9a-zA-Z]/" path)
        ;; There is - extract and remove it from the path
        (let [(path (substring path 3))
              (selector (substring path 1 2))]
          (list selector host port path))
        ;; There is not - assume we are dealing with a menu
        (list "1" host port path))))

(define (quadruplet-to-uri quadruplet)
  (let* [(host (list-ref quadruplet 1))
         (port (list-ref quadruplet 2))
         (path (list-ref quadruplet 3))
         (kind (if (string-prefix? "/" path)
                   (car quadruplet)
                   (string-append (car quadruplet) "/")))]
    (string-append "gopher://" host ":" (number->string port) "/" kind path)))

;;====================================================================;;

(define (debug message . rest)
  (apply format #t (string-concatenate (list "DEBUG: " message "\n")) rest))

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

(define (gopher-get address port selector)
  "Returns a list of lines containing the server's response to a Gopher request at ADDRESS:PORT for SELECTOR."
  (debug "gopher-get recived (~a ~a ~a)" address port selector)
  (let [(s (socket PF_INET SOCK_STREAM 0))]
    (connect s AF_INET (get-ip-of address) port)
    (display (format #f "~a\r\n" selector) s) ; Ask for the selector
    (let loop [(lines '())]
      (let [(line (read-line s))]
        (if (not (or (eof-object? line) (string= line ".\r")))
            (loop (append lines (list line)))
            lines)))))

;; Stacks for the history
(define hist-back '())
(define hist-forw '())
;; Item type -> tag - icon - handler map
(define gopher-types
  '(("0" . ("text" "img-text" gopher-render-text))
    ("1" . ("directory" "img-directory" gopher-render-menu))
    ("7" . ("search" "img-search" gopher-render-menu))
    ("g" . ("image" "img-image" gopher-render-image))
    ("I" . ("image" "img-image" gopher-render-image))
    ("9" . ("binary" "img-binary" gopher-download))))

(define (gopher-history-back)
  (write hist-back)(newline)
  (if (>= (length hist-back) 2)
      (let [(current (car hist-back))
            (previous (car (cdr hist-back)))]
        (set! hist-back (cdr (cdr hist-back)))
        (set! hist-forw (cons current hist-forw))
        (dispatch-by-uri previous)
        ;;(gopher-goto previous)
        )))

(define (gopher-history-forward)
  (if (>= (length hist-forw) 1)
      (let [(next (car hist-forw))]
        (set! hist-forw (cdr hist-forw))
        (dispatch-by-uri next)
        ;;(gopher-goto next)
        )))

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
        (let* [(uri (quadruplet-to-uri (list kind host port selector)))
               (tag (string-append "LINK:" uri))]
          (set! line-tags (append line-tags (list tag)))))
    (widget 'insert 'end
            (string-concatenate (list " " (list-ref pieces 0) "\n"))
            line-tags)))

(define (gopher-render-directory widget lines)
  (widget 'config 'state: 'normal)
  (widget 'delete '1.0 'end)
  (catch #t
    (lambda ()
      (for-each
       (lambda (line)
         (gopher-render-line widget line))
       lines))
    (lambda (key . args)
      (cond
       [(eqv? key 'out-of-range)
        (debug "An out-of range error happened, likely because the server returned a simple Page Not Found")
        (debug "Retrying with `gopher-render-text'")
        (gopher-render-text widget lines)]
       [else
        (write key)
        (newline)])
      ))
  (widget 'config 'state: 'disabled))

(define (gopher-render-text widget lines)
  (widget 'config 'state: 'normal)
  (widget 'delete '1.0 'end)
  (for-each
   (lambda (line)
     (widget 'insert 'end (string-concatenate (list line "\n"))))
   lines)
  (widget 'config 'state: 'disabled))

(define (gopher-jump-to-address)
  (debug "Trying to jump to ~a" (address-bar 'get))
  (let [(address (address-bar 'get))]
    (dispatch-by-uri address))
  (tk/focus dummy))

(tk-start)
;; Initial window settings
(tk/wm 'title tk "Guipher")
(tk/wm 'geometry tk "640x480")
;; Load the various icons we use
(tk/image 'create 'photo "img-back" 'file: "assets/arrow_left.png")
(tk/image 'create 'photo "img-forward" 'file: "assets/arrow_right.png")
(tk/image 'create 'photo "img-refresh" 'file: "assets/arrow_refresh.png")
(tk/image 'create 'photo "img-history" 'file: "assets/time.png")
(tk/image 'create 'photo "img-directory" 'file: "assets/folder.png")
(tk/image 'create 'photo "img-text" 'file: "assets/page.png")
;; Dummy widget to receive focus
(define dummy (tk 'create-widget 'frame))
;; Define the tool bar
(define tool-bar
  (tk 'create-widget 'frame 'bd: 1 'relief: 'raised))
(define btn-back
  (tk 'create-widget 'button 'image: "img-back" 'relief: "flat"
      'command: gopher-history-back))
(define btn-forward
  (tk 'create-widget 'button 'image: "img-forward" 'relief: "flat"
      'command: gopher-history-forward))
(define btn-refresh
  (tk 'create-widget 'button 'image: "img-refresh" 'relief: "flat"
      'command: (lambda ()
                  (let [(current (car hist-back))]
                    (set! hist-back (cdr hist-back))
                    (dispatch-by-uri current)))))
(define btn-history
  (tk 'create-widget 'button 'image: "img-history" 'relief: "flat"
      'command: (lambda () (dispatch-by-uri "about:history"))))
(define address-bar
  (tk 'create-widget 'ttk::combobox))
(tk/bind address-bar "<Return>" gopher-jump-to-address)
;;; Pack
(tk/pack btn-back btn-forward btn-refresh btn-history 'side: 'left 'in: tool-bar)
(tk/pack address-bar 'side: 'left 'expand: 1 'fill: 'x 'in: tool-bar)
(tk/pack tool-bar 'fill: 'x)
;; Define the main widget
(define main-text
  (tk 'create-widget 'text 'border: 0 'relief: 'flat 'wrap: 'word
      'state: 'disabled 'font: '("Ubuntu Mono" 11)))
;;; Pack
(tk/pack main-text 'fill: 'both 'expand: 1)
;; Define the main tags
(main-text 'tag 'config "default" 'lmargin1: 0); (+ 1 (ch-width)))
(main-text 'tag 'config "no-icon" 'lmargin1: 16)
(main-text 'tag 'config "link" 'foreground: 'blue)
(main-text 'tag 'bind "link" "<Enter>"
           (lambda () (main-text 'config 'cursor: 'hand2)))
(main-text 'tag 'bind "link" "<Leave>"
           (lambda () (main-text 'config 'cursor: "")))

(define (gopher-link-at-point widget x y)
  "Given a Tk text WIDGET and a pair of X, Y coordinates, returns a quadruplet of kind, host, selector and port if a link exists at that point, or `#f` if it doesn't."
  (tk-eval (format #f "set ::scmVar(tags) \"[~a tag names @~a,~a]\"" widget x y))
  ;; HACK: Apparently the following line is required - otherwise `tk-get-var`,
  ;;   `tk-get-list` and the like don't work inside of `let` or `find`
  (tk-get-var 'tags)
  (substring (find (lambda (i) (string-prefix? "LINK:" i))
                   (tk-get-list 'tags)) 5))
  
(define (gopher-goto quadruplet)
  (address-bar 'set (quadruplet-to-uri quadruplet))
  (let [(kind (car quadruplet))
        (triplet (cdr quadruplet))]
    (cond
     [(equal? kind "1")
      (gopher-render-directory main-text (apply gopher-get triplet))]
     [(equal? kind "0")
      (gopher-render-text main-text (apply gopher-get triplet))]
     [else
      (debug "Gopher type ~a not handled" kind)])
    ))

(main-text 'tag 'bind "link" "<1>"
           `(,(lambda (widget x y)
                (debug "Clicked on ~a, ~a" x y)
                (let [(link (gopher-link-at-point widget x y))]
                  (if link (begin
                               ;; Clear the forward history stack
                               (set! hist-forw '())
                               (dispatch-by-uri link)
                               ))))
             ,main-text %x %y))
;; Load the home page/address given on the command line
(if (>= (length (command-line)) 2)
     (dispatch-by-uri (car (cdr (command-line))))
     (dispatch-by-uri "gopher://localhost/"))
;; Start Tk
(tk-event-loop)

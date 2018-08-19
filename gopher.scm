#!/usr/bin/env guile
;; Guile + PS/Tk Gopher client !#

(use-modules (ice-9 rdelim) (ice-9 regex) (srfi srfi-1) (web uri)
             (rnrs io ports))
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
    (ui-render main-text (lambda (widget) #f))]
   [(equal? (uri-path uri) "history")
    (ui-render main-text
               (lambda (widget items)
                 (for-each
                  (lambda (item)
                    (main-text 'insert 'end (string-append item "\n")
                               (list "link" (string-append "LINK:" item))))
                  items))
               hist-back)]))

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
        (let [(path (substring path 2))
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
(define (tk-ch-width)
  (let [(f (tk-eval "font create f -family {Ubuntu Mono} -size 11"))
        (w (string->number (tk-eval "font measure f {0}")))]
    (tk-eval "font delete f")
    w))

(define (tk-get-list name)
  (string-split (tk-get-var name) #\space))

(define (gopher-get-lines address port selector)
  "Returns a list of lines containing the server's response to a Gopher request at ADDRESS:PORT for SELECTOR."
  (debug "gopher-get-lines called with (~a ~a ~a)" address port selector)
  (let [(s (socket PF_INET SOCK_STREAM 0))]
    (connect s AF_INET (get-ip-of address) port)
    (display (format #f "~a\r\n" selector) s) ; Ask for the selector
    (let loop [(lines '())]
      (let [(line (read-line s))]
        (if (not (or (eof-object? line) (string= line ".\r")))
            (loop (append lines (list line)))
            lines)))))

(define (gopher-get-raw address port selector)
  (debug "gopher-get-raw called with (~a ~a ~a)" address port selector)
  (let [(s (socket PF_INET SOCK_STREAM 0))]
    (connect s AF_INET (get-ip-of address) port)
    (display (format #f "~a\r\n" selector) s) ; Ask for the selector
    ;; HACK: this is globbing all of the response, true, but it's putting
    ;;   it inside a string - it probably won't work with binary files
    (get-string-all s)))

;; Stacks for the history
(define hist-back '())
(define hist-forw '())
;; Item type -> tag - icon - raw? - handler map
(define gopher-type-image
  (list "image" "img-image"
        (lambda (widget triplet)
          (ui-render widget ui-render-image
                     (apply gopher-get-raw triplet)))))
(define gopher-types
  (list
   (cons "0" (list "text" "img-text"
                   (lambda (widget triplet)
                     (ui-render widget ui-render-text
                                (apply gopher-get-lines triplet)))))
   (cons "1" (list "menu" "img-directory"
                   (lambda (widget triplet)
                     (ui-render widget ui-render-menu
                                (apply gopher-get-lines triplet)))))
   (cons "I" gopher-type-image)
   (cons "g" gopher-type-image)
   (cons "h" (list "html" "img-web"
                   (lambda (widget triplet)
                     (let [(target (list-ref triplet 2))]
                       (if (string-prefix? "URL" target)
                           (system* "xdg-open" (substring target 4))
                           (ui-render widget ui-render-text
                                      (apply gopher-get-lines triplet)))))))
   (cons "9" (list "binary" "img-box"
                   (lambda (widget triplet)
                     (default-type-handler widget triplet))))))

(define (default-type-handler widget triplet)
  (debug ":: ~a" (save-file))
  (debug "default-type-handler called on ~a" triplet)
  (when #f
    (let [(filename (tk/get-save-file
                     'initialfile: (car (reverse triplet))
                     'filetypes: '(("All files" "*")))
                    )
          ;;(filename (eval-wish (format #f "tk_getSaveFile -initialfile ~a"
          ;;                             (car (reverse triplet)))))
          ]
      (tk-get-var 'tags)
      (debug "Filename: ~a :: ~a" filename (car (reverse triplet)))
      (debug "Pulling data from ~a" triplet)
      (apply gopher-get-raw triplet))
    (tk-eval "set ::scmVar(filename) [tk_getSaveFile]")
    (tk-get-var 'filename)
    (debug "Hello ~a" (tk-get-var 'filename))))

(define (gopher-history-back)
  (write hist-back)(newline)
  (if (>= (length hist-back) 2)
      (let [(current (car hist-back))
            (previous (car (cdr hist-back)))]
        (set! hist-back (cdr (cdr hist-back)))
        (set! hist-forw (cons current hist-forw))
        (dispatch-by-uri previous))))

(define (gopher-history-forward)
  (if (>= (length hist-forw) 1)
      (let [(next (car hist-forw))]
        (set! hist-forw (cdr hist-forw))
        (dispatch-by-uri next))))

(define (ui-render-image widget data)
  ;; HACK: I'm creating a temporary file *and* a temporary Tk image here
  ;;   There should be a way to render a chunk of image data without
  ;;   having to do any of this, but a straight-up `create -data data`
  ;;   doesn't work, and it doesn't seem to return a value I can use
  ;;   for the text widget `-image`
  (let [(port (mkstemp! (string-copy "/tmp/guipher-img-XXXXXX")))]
    (display data port)
    (flush-output-port port)
    (let [(img (tk/image 'create 'photo "img-tmp" 'file: (port-filename port)))]
      (widget 'image 'create 'end 'image: "img-tmp"))))

(define (ui-render-text widget lines)
  (for-each
   (lambda (line)
     (widget 'insert 'end (string-concatenate (list line "\n"))))
   lines))

(define (ui-render-menu-line widget line)
  (let* [(kind (substring line 0 1))
         (line (string-trim-right (substring line 1) #\return))
         (pieces (string-split line #\tab))
         (description (list-ref pieces 0))
         (line-tags '("no-icon" "default"))]
    (if (equal? kind "i")
        (widget 'insert 'end (string-append " " description "\n") line-tags)
        (let* [(selector (list-ref pieces 1))
               (host (list-ref pieces 2))
               (port (string->number (list-ref pieces 3)))
               (uri (quadruplet-to-uri (list kind host port selector)))
               (line-tags (cons "link"
                                (cons (string-append "LINK:" uri) line-tags)))
               (type-data (assoc kind gopher-types))]
          (if (and type-data (list-ref (cdr type-data) 1))
              (widget 'image 'create 'end 'image: (list-ref (cdr type-data) 1)))
          (widget 'insert 'end (string-append " " description "\n") line-tags)))))

(define (ui-render-menu widget lines)
  (for-each
   (lambda (line)
     (catch 'out-of-range
       (lambda ()
         (ui-render-menu-line widget line))
       ;; Handle malformed menu lines by rendering them raw
       (lambda (key . args)
         (widget 'insert 'end (string-append  line "\n")))))
   lines))

(define (ui-render widget function . rest)
  (widget 'config 'state: 'normal)
  (widget 'delete '1.0 'end)
  (apply function widget rest)
  (widget 'config 'state: 'disabled))

(define (ui-jump-to-address)
  (debug "Trying to jump to ~a" (address-bar 'get))
  (let [(address (address-bar 'get))]
    (dispatch-by-uri address))
  (tk/focus dummy))

(define (ui-link-at-point widget x y)
  "Given a Tk text WIDGET and a pair of X, Y coordinates, returns a quadruplet of kind, host, selector and port if a link exists at that point, or `#f` if it doesn't."
  (tk-eval (format #f "set ::scmVar(tags) \"[~a tag names @~a,~a]\"" widget x y))
  ;; HACK: Apparently the following line is required - otherwise `tk-get-var`,
  ;;   `tk-get-list` and the like don't work inside of `let` or `find`
  (tk-get-var 'tags)
  (substring (find (lambda (i) (string-prefix? "LINK:" i))
                   (tk-get-list 'tags)) 5))

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
(tk/image 'create 'photo "img-web" 'file: "assets/world_link.png")
(tk/image 'create 'photo "img-image" 'file: "assets/image.png")
(tk/image 'create 'photo "img-box" 'file: "assets/box.png")
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
(tk/bind address-bar "<Return>" ui-jump-to-address)
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

(define (gopher-goto quadruplet)
  (address-bar 'set (quadruplet-to-uri quadruplet))
  (let* [(kind (car quadruplet))
         (triplet (cdr quadruplet))
         (type-data (assoc kind gopher-types))]
    (if (and type-data (car (reverse type-data)))
        ((car (reverse type-data)) main-text triplet)
        (default-type-handler main-text triplet))))

(main-text 'tag 'bind "link" "<1>"
           `(,(lambda (widget x y)
                (debug "Clicked on ~a, ~a" x y)
                (let [(link (ui-link-at-point widget x y))]
                  (if link (begin
                               ;; Clear the forward history stack
                               (set! hist-forw '())
                               (dispatch-by-uri link)
                               ))))
             ,main-text %x %y))
;; Load the home page/address given on the command line
;;(display (tk/get-save-file))
(define (save-file)
  (tk-read-wish)
  (tk-eval "tk_getSaveFile"))
(if (>= (length (command-line)) 2)
     (dispatch-by-uri (car (cdr (command-line))))
     (dispatch-by-uri "gopher://localhost/"))
;; Start Tk
(tk-event-loop)

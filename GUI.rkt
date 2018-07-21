#lang racket/base

(provide choose-file message)

(require (only-in racket/gui get-file message-box))

; choose-file : string -> string
; Ask the user for a file name using a file chooser with the given title
;  and produce the user chosen file name as a string.
(define (choose-file title)
  (path->string (get-file title)))

; message : string string -> string
; Pop up a window with the given title and contents.
(define (message title contents)
  (void (message-box title contents)))

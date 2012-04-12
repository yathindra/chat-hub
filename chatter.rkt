#lang racket/gui
(require racket/tcp)
(define PORT 12345)
;(define server-addr "localhost")



;Here the user joins
(define (new-connection host port uname)
  ;TO-DO Create a  thread for the user 
  ;Connecting to the server here
  (define-values (in out) (tcp-connect host port))
  
  ;Some Header here
  ;Sending the user name
  (print uname out))
 

;===================================================
;Dialog box for join
(define join-dialog (instantiate dialog% ("Join Chat")))
 
;Fields in the dialog
(define host-field 
  (new text-field% [parent join-dialog] [label "Hostname"]))
(send host-field set-value "localhost")


(define name-field
  (new text-field% [parent join-dialog] [label "nick name"]))

(define panel (new horizontal-panel% [parent join-dialog]
                                     [alignment '(center center)]))
 
; Add Cancel and Ok buttons to the horizontal panel
(new button% [parent panel] [label "Connect"]
     (callback (lambda (button event)
                  ;TO-DO ERROR CHECKING AND USER NAME VALIDATION using with-handlers
                                            (new-connection (send host-field get-value) PORT (send name-field get-value)))))
(new button% [parent panel] [label "Cancel"])

;*****START*********
;CLient starts by showing the dialog box
(send join-dialog show #t)




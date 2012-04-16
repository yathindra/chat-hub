#lang racket/gui
(require racket/tcp)
(define PORT 8090)
;(define server-addr "localhost")


;(define (client-loop)
;  (define (loop)
;    (display "Chat box will be displayed\n"))
;  (loop)
;  )
    


;Here the user joins
(define (new-connection host port uname)
  ;TO-DO Create a  thread for the user 
  ;Connecting to the server here
  (define-values (in out) (tcp-connect host port))
  
  (send join-dialog show #f)
  (thread (lambda() ;(client-loop)
           (file-stream-buffer-mode out 'none)
           (display "Chat box will be displayed\n") 
            ;Some Header here
            ;Sending the user name
            ;(write-bytes (string->bytes/locale uname) out)
            (write-bytes (string->bytes/locale uname) out)
             
            ;(if (equal? (read in) "ack") (display "success") (display "no ACK"))
            (if (equal? (read-bytes 3 in) #"ack") (display "success") (display "no ACK"))
            ;(send select-dialog show #f)
            
            )) 
  
  ;Some Header here
  ;Sending the user name
  ;(print uname out)
  ;(if (equal? (read in) "ack") (display "success") (display "no ACK")
      )
 
;===================================================
;Dialog box for select
(define select-dialog (instantiate dialog% ("Select type of chat")))

(define hpanel (new horizontal-panel% [parent select-dialog]
                                     [alignment '(center center)]))
(new button% [parent hpanel] [label "P2P"])
     ;(callback (lambda (button event)
                  ;TO-DO ERROR CHECKING AND USER NAME VALIDATION using with-handlers
      ;                                      (new-connection (send host-field get-value) PORT (send name-field get-value)))))
(new button% [parent hpanel] [label "ChatRoom"]
      (callback (lambda (button event)
                  ;TO-DO ERROR CHECKING AND USER NAME VALIDATION using with-handlers
                                           (new-connection (send host-field get-value) PORT (send name-field get-value)))))
 






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
                                        ;    (new-connection (send host-field get-value) PORT (send name-field get-value))
                 (send select-dialog show #t))))
(new button% [parent panel] [label "Cancel"])

;*****START*********
;CLient starts by showing the dialog box
(send join-dialog show #t)




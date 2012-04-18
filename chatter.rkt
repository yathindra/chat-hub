#lang racket/gui
(require racket/tcp)
(define PORT 8090)
;(define server-addr "localhost")

(define listening-thread null)
           
;Here the user joins
(define (new-connection host port uname)
   ;Connecting to the server here
  (define-values (in out) (tcp-connect host port))
  (send join-dialog show #f)
  (file-stream-buffer-mode out 'none)
  (display "Chat box will be displayed\n") 
  ;sending signin message to the server
  (display (string? uname))
  (write `(signin ,uname) out)
  
  
  (define (loop)
    (define event (thread-receive-evt));you are sending messagea
    (define receive (sync event in))
    (if (equal? event receive) (broadcast-message out)
      (receive-broadcast in out));receive a message from server
    (loop))
  
   (send main-win show #t)
  
   (set! listening-thread (thread (lambda ()
                                    (loop)))))
                     
            


(define (broadcast-message out)
  (define msg (thread-try-receive));will get #f or message
  (display "\nEntered :")(display msg)
  ;(write (cons 'comm msg) out)
  (write (list 'broadcast msg) out)
  ;(write-bytes (string->bytes/locale msg) out)
  (display "\nbroadcasted\n"))

(define (read-message in)
 ; (let ([buff (read-bytes-line in 'return)])
  ;  (if (eof-object? buff) #f
   ;   (bytes->string/utf-8 buff))
  
   ; )
  (read in)
  )



(define (add-to-chat msg out)
   (send chatbox set-value (string-append (send chatbox get-value) msg "\n"))
   (display (string-append "Received message :" msg "\n"))
    (write (list 'listofusers) out))

(define (refresh-list l)
  (for ([i l]) (display i)))

(define (receive-broadcast in out)
  (display "Receiving from server\n")
  ;try to do the header extraction
  (define msg (read-message in))
  (display msg)
  (match msg
    [(list 'signin a) (handle-error a)]
    [(list 'broadcast b) (add-to-chat b out)]
    [(list 'listofusers c) (refresh-list c)]
    [eof-object (void)]
    [_ (error "Unexpected!!!!!")]))
   
  
;===================================================
;Dialog box for error
(define (handle-error msg)
(define error-dialog (instantiate dialog% (msg)))
  (send error-dialog show #t)
  (send name-field set-value ""))



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
;Dialog box for chat/broadcast


(define (textbox-changed t e)
  (and (eq? (send e get-event-type) 'text-field-enter)
       (let ([msg (send t get-value)])
         (display msg)
         ; Queue the message for sending
         (thread-send listening-thread msg)
         ; Clear the textbox
         (send t set-value ""))))

(define main-win  (new frame%
                        [label "ChatRoom"]
                        [width 600]
                        [height 400]
                        [min-width 200]
                        [min-height 200]
                        ))
;(define user-panel (new vertical-panel% [parent main-win]
 ;                     [alignment '(right right)]))

; The box that holds the logged in users
;(define users-list-box (new list-box%
;                            [parent main-win]
;                            [choices (get-users)]
;                            [label "List of users"]
;                            ))

(define chatbox (new text-field%
                     [parent main-win]
                     [label "Conversation"]
                     [min-height 300]
                     [enabled #f]))
(define textbox (new text-field%
                     [parent main-win]
                     [label "Send a message:"]
                     [callback textbox-changed]
                            ))





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
                 ;(send select-dialog show #t))))
(new button% [parent panel] [label "Cancel"])

;*****START*********
;CLient starts by showing the dialog box
(send join-dialog show #t)




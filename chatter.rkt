#lang racket/gui
(require racket/tcp)
(define PORT 8090)
;(define server-addr "localhost")

(define listening-thread null)
(define users-list '())      
(define p2p-windows (make-hash))

;Here the user joins
(define (new-connection host port cmd uname)
   ;Connecting to the server here
  (define-values (in out) (tcp-connect host port))
  (send join-dialog show #f)
  (file-stream-buffer-mode out 'none)
  (display "Chat box will be displayed\n") 
  ;sending signin message to the server
  (display (string? uname))
  (cond
     [(equal? cmd "signin") (write `(signin ,uname) out)]
     [(equal? cmd "signout") (write `(signout ,uname) out) 
                             (send main-win show #f) 
                             (close-input-port in)
                             (close-output-port out)
                             (kill-thread (current-thread))
                             ])
   (define (loop)
    (define event (thread-receive-evt));you are sending messagea
    (define receive (sync event in))
    (if (equal? event receive) (broadcast-message out)
      (receive-broadcast in out));receive a message from server
    (loop))
  
   (send main-win show #t)
  
   (set! listening-thread (thread (lambda ()
                                    (loop))))
  )
                     
            


(define (broadcast-message out)
  (define msg (thread-try-receive));will get #f or message
  (cond
    [(list? msg) (write msg out)]
    [else  (display "\nEntered :")(display msg)
  (write (list 'broadcast msg) out)])
  (display "\nbroadcasted\n"))

(define (read-message in)
  (read in)
  )



(define (add-to-chat msg out box)
   (send box set-value (string-append (send box get-value) msg "\n"))
   (display (string-append "Received message :" msg "\n"))
    (write (list 'listofusers) out))

(define (refresh-list l)
  (set! users-list l)
  ;(display "GOt here")
  ;(display l)
  (send users-list-box set l)
  ;if p2p shown refresh there as well
  )

(define (receive-broadcast in out)
  (display "Receiving from server\n")
  ;try to do the header extraction
  (define msg (read-message in))
  (display msg)
  (display "\n")
  (match msg
    [(list 'signin a) (handle-error a)]
    [(list 'broadcast-signout b) (add-to-chat b out chatbox)]
    [(list 'broadcast b) (add-to-chat b out chatbox)]
    [(list 'listofusers c) (refresh-list c)]
    [(list 'p2p-nouser u) (display "got no user ")(display u)]
    [(list 'p2p-success u) (display "got p2p success ")(display u)
                           (let* ([p2p-win (new frame% [label u]
                                        [width 400]
                                        [height 200]
                                        [min-width 200]
                                        [min-height 200])]
                                  [p2pchatbox (new text-field%
                                                   [parent p2p-win]
                                                   [label "Conversation"]
                                                   [min-height 150]
                                                   [enabled #f])]
                                  [p2ptextbox (new text-field%
                                                   [parent p2p-win]
                                                   [label "Send a message:"]
                                                   [callback textbox-changed]
                                                   )]
                                  [disconnect (new button% [parent p2p-win]
                                       [label "Disconnect"]
                                       [callback (lambda (button event)
                                                   (write `(p2p-disconnect ,(send name-field get-value) ,u)))])]) 
                                                   
		     
		     (hash-set! p2p-windows u p2p-win)
		     (send p2p-win show #t)
		     )]
    [(list 'p2p-message f m) (define target (hash-ref p2p-windows f))
                            (define tbox (car (send target get-children)))
                            (add-to-chat m out tbox)
                            (send target show #t)];will append here
    [(list 'p2p-disconnect a) (display "User left in middle of chat\n")]
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
  ;(display "the text box was : ")(display (send (send t get-parent) get-label))(display "\n")
  
  (and (eq? (send e get-event-type) 'text-field-enter)
       (let ([msg (send t get-value)]
             [label (send (send t get-parent) get-label)])
         ;(display label)
         ;(display msg)
         (if (equal? "ChatRoom" label) (thread-send listening-thread msg) 
             (thread-send listening-thread `(p2p-message ,(send name-field get-value) ,label ,msg) ))
          ; Clear the textbox
         (send t set-value "")))
  (and (eq? (send e get-event-type) 'list-box-dclick)
       (let (;[lst (send t get-selections)] 
             [msg (list-ref  users-list (car (send t get-selections)))])
         ;(define msg (list-ref  users-list (car lst))))
        ;(and (not (hash-has-key? p2p-windows msg)) (thread-send listening-thread `(p2p-start ,(send name-field get-value) ,msg))) 
         ;(and (not (hash-has-key? p2p-windows msg)) (thread-send listening-thread `(p2p-start ,(send name-field get-value) ,msg)))
         (if (not (hash-has-key? p2p-windows msg))  (thread-send listening-thread `(p2p-start ,(send name-field get-value) ,msg))
             (send (hash-ref p2p-windows msg) show #t))
             ;(if (send (hash-ref p2p-windows msg) is-shown?) (void) (send (hash-ref p2p-windows msg) show #t)
         ;(and (hash-has-key? p2p-windows msg) (send (hash-ref p2p-windows msg) show #t))
         ;(display "\ngot the user name from list box here!!\n")
       
  )))

(define main-win  (new frame%
                        [label "ChatRoom"]
                        [width 600]
                        [height 400]
                        [min-width 200]
                        [min-height 200]
                        ))

(define align-panel (new horizontal-panel% [parent main-win]
                      [alignment '(right top)]
                      ;[min-width 200]
                      ;[min-height 200]
                      ))   




(define chatbox (new text-field%
                     [parent align-panel]
                     [label "Conversation"]
                     [min-height 300]
                     [enabled #f]))


; The box that holds the logged in users
(define users-list-box (new list-box%
                            [parent align-panel]
                            ;[horiz-margin 200]
                            [choices users-list]
                            [label "List of users"]
                            [style '(single vertical-label)]
                            [callback textbox-changed]
                            
                         ))


(define textbox (new text-field%
                     [parent main-win]
                     [label "Send a message:"]
                     [callback textbox-changed]
                            ))

(define bottom-button-panel (new horizontal-panel% [parent main-win]
                                     [alignment '(center bottom)]))

; Add Disconnect button to the main chat window

(new button% [parent bottom-button-panel] [label "Disconnect"]
     (callback (lambda (button event)                 
                  (new-connection (send host-field get-value) PORT "signout" (send name-field get-value)))))
                 
(new button% [parent bottom-button-panel] [label "p2p"]
     ;(callback (lambda (button event)  
      ;           (new-p2p (send host-field get-value) PORT (send name-field get-value))))
     )
     



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
                  (new-connection (send host-field get-value) PORT "signin" (send name-field get-value)))))
                 ;(send select-dialog show #t))))
(new button% [parent panel] [label "Cancel"])
;*****START*********
;CLient starts by showing the dialog box
(send join-dialog show #t)




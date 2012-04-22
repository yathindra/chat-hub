;Lambda Chat Server
#lang racket

(require racket/date)
(require racket/set)

;Global Constants
(define HOSTNAME #f)    ;Host is the localhost
(define PORT 8090)      ;Port is some convenient
(define REUSE-PORT #t)  ;For Debugging
(define MAX 10)         ;Max number of clients on the waiting Q

;Main Function
(define (server port)
  
  (define users (make-hash))    ;Mapping from the user to the socket
  (define user-threads (set))   ;A set of user Threads so the server can broadcast
  
  (define (reg-user username in out)
    (hash-set! users username (cons in out)))
  
  (define (unreg-user username)
    (and (hash-has-key? users username) (hash-remove! users username)))
  
  (define (reg-thread thr)
    (set! user-threads (set-add user-threads thr))
    )
  
  (define (unreg-thread thr)
    (set! user-threads (set-remove user-threads thr)))
  
  (define (broadcast cmd msg)
    (define sendIt `(,cmd ,msg)) 
    (display "Inside broadcast\n")
    (display sendIt)
    (set-for-each user-threads (lambda (t) (thread-send t sendIt)))
    ;(hash-map users (lambda (_ v) (write `(,cmd ,msg) (second v))))
    )
  
  (define (list-of-users)
    (hash-map users (lambda (u _) u)))
  
  (define listener
    (begin
      (display "Starting the server\n")
      (let ([res (tcp-listen port MAX REUSE-PORT HOSTNAME)])
      (display (string-append "Server waiting for clients at port " (number->string port) ".\n"))
        res
        )
      )
    )
  
  (define (handle-client listener)
    (define-values (in out)
                   (tcp-accept listener))
    (thread (lambda ()
              (reg-thread (current-thread))           ;Register the thread
              (file-stream-buffer-mode out 'none)     ;Cancel the buffer. No delay.
              (communicate in out)                    ;Actual Thread communication happens
              (display "Connection ended. Removing thread...")
              (unreg-thread (current-thread))
              (close-input-port in)
              (close-output-port out)
              )
            )
    )
  
  (define (communicate in out)
    
    ;Provides the functions to set and get the username. Initially the username is set to null
    (define-values (get-user set-user)
                   (let ((user null)) 
                     (define (get) user)
                     (define (set username) (set! user username))
                     (values get set)
                     ))
    
    ;Signin the user
    (define (signin username in out)
      (define (post-action)
        (display "\ninside post action of signin\n ")
        (broadcast 'broadcast (string-append username " has joined"))
        )
      (display (string-append "Signup : " username))
      (if (hash-has-key? users username)
          (write (cons 'signin-samename (string-append username " -username has been taken\n")))
          (begin
            (reg-user username in out)
            (set-user username)
            (display (string-append (number->string (hash-count users)) " users online"))
            (post-action)
            )
          )
      )
    
    ;Signout the user
    (define (signout username in out)
      (define (post-action)
        (broadcast 'broadcast (string-append username " has signed out"))
        )
      (display (string-append "Trying to sign out the user " username))
      (if (not (hash-has-key? users username))
          (display (string-append "No user named " username " to sign out\n"))
          (begin
            (unreg-user username)
            (display (string-append username " is signed out\n"))
            (post-action)
            )
          )
      )
    
    ;p2p between the users
    (define (p2p-start fromUser toUser in out)
      (display "Inside p2p-start\n")
      (display (not (hash-has-key? users toUser)))
      (if (not (hash-has-key? users toUser))
          ;true - The toUser is  not present so send a p2p-nouser message
          (thread-send  (current-thread) (list 'p2p-nouser toUser))
          ;false - The toUser is present and send a 
          (begin
            ;(display "P2P-One\n")
            (thread-send (current-thread) (list 'p2p-success toUser))
            ;(display "P2P-Two\n")
            (write `( p2p-success ,fromUser )(cdr (hash-ref users toUser)))
            ;(display "P2P Success\n")
            )
          )
      )
    
    ;return the list of users
    (define (userlist in out)
      (define msg (list 'listofusers (list-of-users)))
      (display "Returning the list of users\n")
      (thread-send (current-thread) msg)
      )
    
    ;;Handle the events by looping
    (define (communicate-loop)
      (display "Looping in the communicate-loop\n")
      (let* ([msg-evt (thread-receive-evt)]
             [ready (sync msg-evt in)])
        (if (eq? ready msg-evt)
          (handle-push)
          (handle-input ready)
          )
        )
      )
    
    
    (define (handle-push)
     (display "Pushing...\n")
      (let ([instruction (thread-try-receive)])
        ; Send push only if user is logged in
        (display instruction)
        (and (get-user) instruction (begin
                                      ( write instruction out)
                                      )
             (communicate-loop)
        ) 
        )
      )
    
    ;;Handle the input
    (define (handle-input in)
      (define message (read in))
      (define id (first message))
      (cond
        [(eq? id 'signin ) (signin (second message) in out)]
        [(eq? id 'signout) (signout (second message) in out)]
        [(eq? id 'listofusers) (userlist in out )]
        [(eq? id 'broadcast) (broadcast 'broadcast (string-append (get-user) " says : "(second message)))]
        [(eq? id 'p2p-start) (p2p-start (second message) (third message) in out)]
        [else (begin (display id) (display "\n") (display (second message)) (display "\n")  (display (third message)) (display "\n") (display message))]
        )
      (communicate-loop)
      )
    
    ;The thread is quiting so remove the user from the user name and remove the thread
    (define (unreg)
      (unreg-thread (current-thread))
      (unreg-user (get-user))
      )
    
    ;The starting point of the communicate with the exception handlers for exception
    (with-handlers ([(lambda (e) #t) (lambda (e) (unreg))])
                   (communicate-loop))
    
    
    )

  (define (main)
    (handle-client listener)
    (main))
  
  (main)
  
 )

(server PORT)

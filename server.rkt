#lang racket
(require racket/tcp)

(define PORT 8090)
(define HOSTNAME "localhost")            ; bind to all interfaces
(define REUSE-PORT #t)          ; for debugging
(define MAX-CLIENTS 30)         ; maximum number of clients waiting

  

  
(define (handle-request listen-port)
 ; (display "\n")
  ;handle user signout
  (display "Server waiting for client.\n")
  (define-values (in out) (tcp-accept listen-port))
  
  (thread (lambda() 
             (file-stream-buffer-mode out 'none)
            ;(display (read in))
            (communicate-loop in)
            ;To let the user know he has logged in
            ;(print "ack" out)
            (write-bytes #"ack" out)))
  (handle-request listen-port)) 
  

(define (server-loop)
  
  (display "Server is now listening\n")
  (define listen-port (tcp-listen PORT MAX-CLIENTS REUSE-PORT HOSTNAME))
  (define-values (in out) (tcp-accept listen-port))
  (communicate-loop in)
  ;(display "Server is listening\n")
  ;(handle-request listen-port))
  )
  
(define (communicate-loop in)
      ;(debug-message "Looping in communicate-loop")
      ; Wait for messages or for client input
      (let* ([msg-evt (thread-receive-evt)]
             [ready (sync msg-evt in)])
        (if (eq? ready msg-evt)
          (display "This was never meant to be executed")
          (handle-input ready)
          )
        )
  (communicate-loop in)
      )
  
(define (handle-input in)
  (display in))
;;Begin server 
(server-loop)
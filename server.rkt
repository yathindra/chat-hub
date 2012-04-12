#lang racket
(require racket/tcp)

(define PORT 8081)
(define HOSTNAME "localhost")            ; bind to all interfaces
(define REUSE-PORT #t)          ; for debugging
(define MAX-CLIENTS 30)         ; maximum number of clients waiting

  
(define (handle-request in out)
 ; (display "\n")
  (display (read in))
  ;To let the user know he has logged in
  (print "ack" out)
  (handle-request in out))
  

(define (server-loop)
  
  (define listen-port (tcp-listen PORT MAX-CLIENTS REUSE-PORT HOSTNAME))
  (display "Server is now active\n")
  (define-values (in out) (tcp-accept listen-port))
  (display "Server is listening\n")
  (handle-request in out))
  
  

;;Begin server 
(server-loop)
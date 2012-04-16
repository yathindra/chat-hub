#lang racket
(require racket/tcp)

(define PORT 12345)
(define HOSTNAME "localhost")            ; bind to all interfaces
(define REUSE-PORT #t)          ; for debugging
(define MAX-CLIENTS 30)         ; maximum number of clients waiting

  
(define (handle-request in out)
  (display "\n")
  (display (read-line in))
  (print "ack" out)
  (handle-request in out))
  

(define (server-loop)
  
  (define listen-port (tcp-listen PORT MAX-CLIENTS REUSE-PORT HOSTNAME))
  (define-values (in out) (tcp-accept listen-port))
  (handle-request in out))
  
  

;;Begin server 
(server-loop)
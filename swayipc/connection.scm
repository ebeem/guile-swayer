(define-module (swayipc connection)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 hash-table)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (oop goops)
  #:use-module (srfi srfi-18)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)

  #:export (RUN-COMMMAND-MSG-ID
            GET-WORKSPACES-MSG-ID
            SUBSCRIBE-MSG-ID
            GET-OUTPUTS-MSG-ID
            GET-TREE-MSG-ID
            GET-MARKS-MSG-ID
            GET-BAR-CONFIG-MSG-ID
            GET-VERSION-MSG-ID
            GET-BINDING-MODES-MSG-ID
            GET-CONFIG-MSG-ID
            SEND-TICK-MSG-ID
            SYNC-MSG-ID
            GET-BINDING-STATE-MSG-ID
            GET-INPUTS-MSG-ID
            GET-SEATS-MSG-ID

            WORKSPACE-EVENT-REPLY
            OUTPUT-EVENT-REPLY
            MODE-EVENT-REPLY
            WINDOW-EVENT-REPLY
            BAR-CONFIG-UPDATE-EVENT-REPLY
            BINDING-EVENT-REPLY
            SHUTDOWN-EVENT-REPLY
            TICK-EVENT-REPLY
            BAR-STATE-UPDATE-EVENT-REPLY
            INPUT-EVENT-REPLY

            SOCKET-PATH
            COMMAND-SOCKET
            LISTENER-SOCKET
            LISTENER-THREAD
            MSG-MAGIC
            MSG-MAGIC-BV
            start-event-listener-thread
            start-event-listener
            data-received-hook
            command-received-hook

            SOCKET-COMMANDS-LISTENER-PATH
            COMMANDS-LISTENER-SOCKET
            COMMANDS-LISTENER-THREAD
            start-commands-listener-thread
            start-commands-listener

            write-msg
            read-msg
            encode-msg))

;; sway messages and replies types
;; man: sway-ipc(7): MESSAGES AND REPLIES
(define RUN-COMMMAND-MSG-ID 0)
(define GET-WORKSPACES-MSG-ID 1)
(define SUBSCRIBE-MSG-ID 2)
(define GET-OUTPUTS-MSG-ID 3)
(define GET-TREE-MSG-ID 4)
(define GET-MARKS-MSG-ID 5)
(define GET-BAR-CONFIG-MSG-ID 6)
(define GET-VERSION-MSG-ID 7)
(define GET-BINDING-MODES-MSG-ID 8)
(define GET-CONFIG-MSG-ID 9)
(define SEND-TICK-MSG-ID 10)
(define SYNC-MSG-ID 11)
(define GET-BINDING-STATE-MSG-ID 12)
(define GET-INPUTS-MSG-ID 100)
(define GET-SEATS-MSG-ID 101)

(define WORKSPACE-EVENT-REPLY 2147483648)
(define OUTPUT-EVENT-REPLY 2147483649)
(define MODE-EVENT-REPLY 2147483650)
(define WINDOW-EVENT-REPLY 2147483651)
(define BAR-CONFIG-UPDATE-EVENT-REPLY 2147483652)
(define BINDING-EVENT-REPLY 2147483653)
(define SHUTDOWN-EVENT-REPLY 2147483654)
(define TICK-EVENT-REPLY 2147483655)
(define BAR-STATE-UPDATE-EVENT-REPLY 2147483656)
(define INPUT-EVENT-REPLY 2147483657)

(define LISTENER-THREAD #:f)
(define COMMANDS-LISTENER-THREAD #:f)
(define MSG-MAGIC "i3-ipc")
(define MSG-MAGIC-BV (string->utf8 MSG-MAGIC))

;; TODO: maybe also get from sway and i3 binaries
(define SOCKET-PATH
  (and (getenv "SWAYSOCK")
       (getenv "I3SOCK")))

(define SOCKET-COMMANDS-LISTENER-PATH
  (string-append (dirname SOCKET-PATH) "/sway-commands-ipc.sock"))

(define COMMAND-SOCKET (socket AF_UNIX SOCK_STREAM 0))
(connect COMMAND-SOCKET (make-socket-address AF_UNIX SOCKET-PATH))
(define LISTENER-SOCKET (socket AF_UNIX SOCK_STREAM 0))
(connect LISTENER-SOCKET (make-socket-address AF_UNIX SOCKET-PATH))
(define COMMANDS-LISTENER-SOCKET (socket AF_UNIX SOCK_STREAM 0))

;; <magic-string> is i3-ipc, for compatibility with i3
;; <payload-length> is a 32-bit integer in native byte order
;; <payload-type> is a 32-bit integer in native byte order
(define (encode-msg command-id payload)
  (let* ((bv (make-bytevector (+ 14 (string-length payload)))))
    ;; <magic-string> <payload-length> <payload-type> <payload>
    (bytevector-copy! (string->utf8 "i3-ipc") 0 bv 0 6)
    (bytevector-u32-set! bv 6 (string-length payload) (native-endianness))
    (bytevector-u32-set! bv 10 command-id (native-endianness))

    ;; payload is optional
    (when (> (string-length payload) 0)
      (bytevector-copy! (string->utf8 payload) 0 bv 14 (string-length payload)))
    bv))

(define (write-msg sock command-id payload)
  (put-bytevector sock (encode-msg command-id payload)))

;; Mutex for synchronization
(define mutex-table (make-hash-table))

(define (read-msg sock)
  (let* ((mutex (if (hash-get-handle mutex-table (fileno sock))
                    (cdr (hash-get-handle mutex-table (fileno sock)))
                    (hash-set! mutex-table (fileno sock) (make-mutex)))))
    (mutex-lock! mutex)
    (let* ((bv-header (get-bytevector-n sock 14))
          (payload-length (bytevector-u32-ref bv-header 6 (native-endianness)))
          (command-id (bytevector-u32-ref bv-header 10 (native-endianness)))
          (payload (utf8->string (get-bytevector-n sock payload-length))))
      (mutex-unlock! mutex)
      (list command-id (or payload "")))))

(define (read-from-socket sock)
  (let loop ()
      (let ((data (read-msg sock)))
        (run-hook data-received-hook
                  (list-ref data 0)
                  (list-ref data 1))
              (loop))))

(define data-received-hook
  ;; data received: emitted on new data received via ipc.

  ;; Parameters:
  ;;   - arg1: command-id.
  ;;   - arg2: payload.
  (make-hook 2))

(define command-received-hook
  ;; data received: emitted on new command received via ipc.

  ;; Parameters:
  ;;   - arg1: command-id.
  ;;   - arg2: payload.
  (make-hook 2))

(define (handle-client client)
  (let ((port (car client)))
    (let ((data (read-msg port)))
      (run-hook command-received-hook
                (list-ref data 0)
                (list-ref data 1)))

    ;; Close the connection
    (close-port port)))

(define (custom-exception-handler exc)
  (display "An error occurred while handling client connection\n"))

(define (start-server-socket sock)
  (listen sock 15)
  (let loop ()
    (let ((client (accept sock)))
      (handle-client client)
      (loop))))

(define (start-event-listener)
  (read-from-socket LISTENER-SOCKET))

(define (start-event-listener-thread)
  (set! LISTENER-THREAD (make-thread start-event-listener))
  (thread-start! LISTENER-THREAD))

(define (start-commands-listener)
  (when (file-exists? SOCKET-COMMANDS-LISTENER-PATH)
    (delete-file SOCKET-COMMANDS-LISTENER-PATH))

  (bind COMMANDS-LISTENER-SOCKET (make-socket-address AF_UNIX SOCKET-COMMANDS-LISTENER-PATH))
  (start-server-socket COMMANDS-LISTENER-SOCKET))

(define (start-commands-listener-thread)
  (set! COMMANDS-LISTENER-THREAD (make-thread start-commands-listener))
  (thread-start! COMMANDS-LISTENER-THREAD))

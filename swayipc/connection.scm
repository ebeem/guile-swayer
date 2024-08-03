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

  #:export (SWAY-MSG-ID-RUN-COMMMAND
            SWAY-MSG-ID-GET-WORKSPACES
            SWAY-MSG-ID-SUBSCRIBE
            SWAY-MSG-ID-GET-OUTPUTS
            SWAY-MSG-ID-GET-TREE
            SWAY-MSG-ID-GET-MARKS
            SWAY-MSG-ID-GET-BAR-CONFIG
            SWAY-MSG-ID-GET-VERSION
            SWAY-MSG-ID-GET-BINDING-MODES
            SWAY-MSG-ID-GET-CONFIG
            SWAY-MSG-ID-SEND-TICK
            SWAY-MSG-ID-SYNC
            SWAY-MSG-ID-GET-BINDING-STATE
            SWAY-MSG-ID-GET-INPUTS
            SWAY-MSG-ID-GET-SEATS

            SWAY-EVENT-ID-WORKSPACE
            SWAY-EVENT-ID-OUTPUT
            SWAY-EVENT-ID-MODE
            SWAY-EVENT-ID-WINDOW
            SWAY-EVENT-ID-BAR-CONFIG-UPDATE
            SWAY-EVENT-ID-BINDING
            SWAY-EVENT-ID-SHUTDOWN
            SWAY-EVENT-ID-TICK
            SWAY-EVENT-ID-BAR-STATE-UPDATE
            SWAY-EVENT-ID-INPUT

            SWAY-SOCKET-PATH
            SWAY-COMMAND-SOCKET
            SWAY-LISTENER-SOCKET
            SWAY-LISTENER-THREAD
            SWAY-MSG-MAGIC

            sway-connect-sockets!
            sway-start-event-listener-thread
            sway-start-event-listener
            sway-data-received-hook
            sway-write-msg
            sway-read-msg
            sway-encode-msg))

;; sway messages and replies types
;; man: sway-ipc(7): MESSAGES AND REPLIES
(define SWAY-MSG-ID-RUN-COMMMAND 0)
(define SWAY-MSG-ID-GET-WORKSPACES 1)
(define SWAY-MSG-ID-SUBSCRIBE 2)
(define SWAY-MSG-ID-GET-OUTPUTS 3)
(define SWAY-MSG-ID-GET-TREE 4)
(define SWAY-MSG-ID-GET-MARKS 5)
(define SWAY-MSG-ID-GET-BAR-CONFIG 6)
(define SWAY-MSG-ID-GET-VERSION 7)
(define SWAY-MSG-ID-GET-BINDING-MODES 8)
(define SWAY-MSG-ID-GET-CONFIG 9)
(define SWAY-MSG-ID-SEND-TICK 10)
(define SWAY-MSG-ID-SYNC 11)
(define SWAY-MSG-ID-GET-BINDING-STATE 12)
(define SWAY-MSG-ID-GET-INPUTS 100)
(define SWAY-MSG-ID-GET-SEATS 101)

(define SWAY-EVENT-ID-WORKSPACE 2147483648)
(define SWAY-EVENT-ID-OUTPUT 2147483649)
(define SWAY-EVENT-ID-MODE 2147483650)
(define SWAY-EVENT-ID-WINDOW 2147483651)
(define SWAY-EVENT-ID-BAR-CONFIG-UPDATE 2147483652)
(define SWAY-EVENT-ID-BINDING 2147483653)
(define SWAY-EVENT-ID-SHUTDOWN 2147483654)
(define SWAY-EVENT-ID-TICK 2147483655)
(define SWAY-EVENT-ID-BAR-STATE-UPDATE 2147483656)
(define SWAY-EVENT-ID-INPUT 2147483657)

;; listener thread reference
(define SWAY-LISTENER-THREAD #:f)
;; commands listener thread reference
(define SWAY-COMMANDS-LISTENER-THREAD #:f)
;; the magic string used in the encoded sway/i3 ipc messages
(define SWAY-MSG-MAGIC "i3-ipc")

;; TODO: maybe also get from sway and i3 binaries
;; the path of sway ipc socket
;; (system "sway --get-socketpath")
;; (system "i3 --get-socketpath")
(define SWAY-SOCKET-PATH
  (or (getenv "SWAYSOCK")
      (getenv "I3SOCK")))

;; sway listen socket, this is used to listen to subscribed events
;; from sway via IPC.
(define SWAY-LISTENER-SOCKET (socket AF_UNIX SOCK_STREAM 0))

;; sway command socket, this is used to send commands and queries
;; to sway via IPC.
(define SWAY-COMMAND-SOCKET (socket AF_UNIX SOCK_STREAM 0))

(define (sway-connect-sockets!)
  (connect SWAY-LISTENER-SOCKET (make-socket-address AF_UNIX SWAY-SOCKET-PATH))
  (connect SWAY-COMMAND-SOCKET (make-socket-address AF_UNIX SWAY-SOCKET-PATH)))

;; Hashtable of mutexes for synchronization, keeps each socket separate.
;; This is important to lock sockets while reading/writing.
;; Without it, sway kept sending invalid messages in case so many
;; commands/events are triggered.
(define mutex-table (make-hash-table))

;; <magic-string> is i3-ipc, for compatibility with i3
;; <payload-length> is a 32-bit integer in native byte order
;; <payload-type> is a 32-bit integer in native byte order
(define (sway-encode-msg command-id payload)
  "Return a bytevector representing a message based on i3/sway IPC protocol.
Parameters:
	- command-id: a number representing the id of the command to send.
	- payload: a json string to send along with the command.

Note: returned format is <magic-string> <payload-length> <payload-type> <payload>"
  (let* ((bv (make-bytevector (+ 14 (string-length payload)))))
    (bytevector-copy! (string->utf8 SWAY-MSG-MAGIC) 0 bv 0 6)
    (bytevector-u32-set! bv 6 (string-length payload) (native-endianness))
    (bytevector-u32-set! bv 10 command-id (native-endianness))

    ;; payload is optional
    (when (> (string-length payload) 0)
      (bytevector-copy! (string->utf8 payload) 0 bv 14 (string-length payload)))
    bv))

(define (sway-write-msg sock command-id payload)
  "Encode then send a message based on i3/sway IPC protocol to i3/sway IPC.
Parameters:
	- sock: the socket to write the message to.
	- command-id: a number representing the id of the command to send.
	- payload: a json string to send along with the command."

  (put-bytevector sock (sway-encode-msg command-id payload)))

(define (sway-read-msg sock)
  "Return a list in the format of '(command-id payload).
It reads and then decode the read message into command-id payload.

Parameters:
	- sock: the socket to read the message from.

Note: read format is <magic-string> <payload-length> <payload-type> <payload>"
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

;; data received: emitted on new data received via ipc.
;; Parameters:
;;   - arg1: command-id.
;;   - arg2: payload.
(define sway-data-received-hook
  (make-hook 2))

(define (read-from-socket sock)
  "Read the message from the given socket.
Once a message is recieved, the sway-data-received-hook will be triggered."
  (let loop ()
      (let ((data (sway-read-msg sock)))
        (run-hook sway-data-received-hook
                  (list-ref data 0)
                  (list-ref data 1))
              (loop))))

(define (sway-start-event-listener)
  "Start the event listener socket.
This is a blocking listener, use sway-start-event-listener-thread for
threading."
  (read-from-socket SWAY-LISTENER-SOCKET))

(define (sway-start-event-listener-thread)
  "Start the event listener socket in a thread."
  (set! SWAY-LISTENER-THREAD (make-thread sway-start-event-listener))
  (thread-start! SWAY-LISTENER-THREAD))

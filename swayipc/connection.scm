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

;; listener thread reference
(define LISTENER-THREAD #:f)
;; commands listener thread reference
(define COMMANDS-LISTENER-THREAD #:f)
(define MSG-MAGIC "i3-ipc")
(define MSG-MAGIC-BV (string->utf8 MSG-MAGIC))

;; TODO: maybe also get from sway and i3 binaries
;; the path of sway ipc socket
(define SOCKET-PATH
  (and (getenv "SWAYSOCK")
       (getenv "I3SOCK")))

;; the path of the swayipc command ipc socket 
(define SOCKET-COMMANDS-LISTENER-PATH
  (string-append (dirname SOCKET-PATH) "/sway-commands-ipc.sock"))

;; sway listen socket, this is used to listen to subscribed events
;; from sway via IPC.
(define LISTENER-SOCKET (socket AF_UNIX SOCK_STREAM 0))
(connect LISTENER-SOCKET (make-socket-address AF_UNIX SOCKET-PATH))

;; sway command socket, this is used to send commands and queries
;; to sway via IPC.
(define COMMAND-SOCKET (socket AF_UNIX SOCK_STREAM 0))
(connect COMMAND-SOCKET (make-socket-address AF_UNIX SOCKET-PATH))

;; Hashtable of mutexes for synchronization, keeps each socket separate.
;; This is important to lock sockets while reading/writing.
;; Without it, sway kept sending invalid messages in case so many
;; commands/events are triggered.
(define mutex-table (make-hash-table))

;; <magic-string> is i3-ipc, for compatibility with i3
;; <payload-length> is a 32-bit integer in native byte order
;; <payload-type> is a 32-bit integer in native byte order
(define (encode-msg command-id payload)
  "Return a bytevector representing a message based on i3/sway IPC protocol.
Parameters:
	- command-id: a number representing the id of the command to send.
	- payload: a json string to send along with the command.

Note: returned format is <magic-string> <payload-length> <payload-type> <payload>"
  (let* ((bv (make-bytevector (+ 14 (string-length payload)))))
    (bytevector-copy! (string->utf8 "i3-ipc") 0 bv 0 6)
    (bytevector-u32-set! bv 6 (string-length payload) (native-endianness))
    (bytevector-u32-set! bv 10 command-id (native-endianness))

    ;; payload is optional
    (when (> (string-length payload) 0)
      (bytevector-copy! (string->utf8 payload) 0 bv 14 (string-length payload)))
    bv))

(define (write-msg sock command-id payload)
  "Encode then send a message based on i3/sway IPC protocol to i3/sway IPC.
Parameters:
	- sock: the socket to write the message to.
	- command-id: a number representing the id of the command to send.
	- payload: a json string to send along with the command."

  (put-bytevector sock (encode-msg command-id payload)))

(define (read-msg sock)
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
(define data-received-hook
  (make-hook 2))

(define (read-from-socket sock)
  "Read the message from the given socket.
Once a message is recieved, the data-received-hook will be triggered."
  (let loop ()
      (let ((data (read-msg sock)))
        (run-hook data-received-hook
                  (list-ref data 0)
                  (list-ref data 1))
              (loop))))

(define (start-event-listener)
  "Start the event listener socket."
  (read-from-socket LISTENER-SOCKET))

(define (start-event-listener-thread)
  "Start the event listener socket in a thread."
  (set! LISTENER-THREAD (make-thread start-event-listener))
  (thread-start! LISTENER-THREAD))

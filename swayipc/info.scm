(define-module (swayipc info)
  #:use-module (swayipc connection)
  #:use-module (swayipc records)
  #:use-module (oop goops)
  #:use-module (sjson parser)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)

  #:export (sway-get-workspaces
            sway-get-outputs
            sway-get-tree
            sway-get-marks
            sway-get-bars
            sway-get-bar-config
            sway-get-version
            sway-get-binding-modes
            sway-get-config
            sway-get-binding-state
            sway-get-inputs
            sway-get-seats))

(define (custom-exception-handler exc command-id payload)
  (display "An error occurred: ")
  (display (exp->string exc))
  (newline)
  (display (string-append "command: " (number->string command-id) ", payload: " payload)))

(define (sway-send-query message-id payload)
  "returns the ipc response from sway after sending the message-id and payload"
  (with-exception-handler
              (lambda (exc)
                (custom-exception-handler exc command-id payload))
               (lambda () (begin
                            (write-msg COMMAND-SOCKET
                                      message-id
                                      payload)
                            (let* ((out (read-msg COMMAND-SOCKET)))
                              (list-ref out 1))))
               #:unwind? #t))

(define (sway-get-workspaces)
  "Retrieves the list of workspaces."
  (map
    (lambda (workspace)
      (scm->sway-workspace workspace))
    (vector->list
     (json-string->scm
      (sway-send-query GET-WORKSPACES-MSG-ID "")))))

(define (sway-get-outputs)
  "Retrieve the list of outputs."
  (map
    (lambda (output)
      (scm->sway-output output))
    (vector->list
     (json-string->scm
      (sway-send-query GET-OUTPUTS-MSG-ID "")))))

(define (sway-get-tree)
  "Retrieve a representation of the tree."
  (json->sway-tree (sway-send-query GET-TREE-MSG-ID "")))

(define (sway-get-marks)
  "Retrieve the currently set marks."
  (vector->list
     (json-string->scm
      (sway-send-query GET-MARKS-MSG-ID ""))))

(define (sway-get-bars)
  "retrieves the list of configured bar IDs."
  (vector->list
     (json-string->scm
      (sway-send-query GET-BAR-CONFIG-MSG-ID ""))))

(define (sway-get-bar-config bar-id)
  "retrieves the config associated with the specified by the bar ID."
  (json->sway-bar-config (sway-send-query GET-BAR-CONFIG-MSG-ID bar-id)))

(define (sway-get-version)
  "Retrieve version information about the sway process."
  (json->sway-version (sway-send-query GET-VERSION-MSG-ID "")))

(define (sway-get-binding-modes)
  "Retrieve the list of binding modes that currently configured."
  (vector->list
     (json-string->scm
      (sway-send-query GET-BINDING-MODES-MSG-ID ""))))

(define (sway-get-config)
  "Retrieve the list of binding modes that currently configured."
  (json->sway-config (sway-send-query GET-CONFIG-MSG-ID "")))

(define (sway-get-binding-state)
  "Returns the currently active binding mode."
  (json->sway-binding-state (sway-send-query GET-BINDING-STATE-MSG-ID "")))

(define (sway-get-inputs)
  "Retrieve a list of the input devices currently available."
  (map
      (lambda (input)
        (scm->sway-input input))
      (vector->list
      (json-string->scm
        (sway-send-query GET-INPUTS-MSG-ID "")))))

(define (sway-get-seats)
  "Retrieve a list of the seats currently configured."
  (map
      (lambda (seat)
        (scm->sway-seat seat))
      (vector->list
      (json-string->scm
        (sway-send-query GET-SEATS-MSG-ID "")))))

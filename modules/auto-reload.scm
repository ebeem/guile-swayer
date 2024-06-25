;; The auto-reload module is designed to automatically reload Sway
;; configuration when changes are detected in specified directories.
;; This module leverages the Sway IPC protocol for reloading and uses
;; inotify for monitoring file system events. The key functions are:
;;     auto-reload-configure: Configures the directories to watch for changes.
;;     auto-reload-init: Initiates the monitoring of the specified directories
;; 						 and reloads the Sway configuration upon detecting changes.

;; Here's an example of how to use this module:
;; NOTE: use the full path for your home, don't use tilde (~)
;; (auto-reload-configure #:directories '("/home/ebeem/.config/sway/"))
;; (auto-reload-init)

(define-module (modules auto-reload)
  #:use-module (swayipc)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-18)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 rdelim)
  #:export (auto-reload-configure
            auto-reload-init))

;; The list directories to watch for changes
(define DIRECTORIES '())

;; Load the inotify library
(define inotify (dynamic-link "libc.so.6"))

;; Import the necessary functions from the inotify library
(define inotify-init
  (pointer->procedure int (dynamic-func "inotify_init" inotify) '()))

(define inotify-add-watch
  (pointer->procedure int (dynamic-func "inotify_add_watch" inotify)
                      (list int '* uint32)))

(define inotify-rm-watch
  (pointer->procedure int (dynamic-func "inotify_rm_watch" inotify)
                      (list int int)))

(define read
  (pointer->procedure int (dynamic-func "read" inotify)
                      (list int '* size_t)))

;; Define constants for inotify events
(define IN_ACCESS #x00000001)
(define IN_MODIFY #x00000002)
(define IN_ATTRIB #x00000004)
(define IN_CLOSE_WRITE #x00000008)
(define IN_CLOSE_NOWRITE #x00000010)
(define IN_OPEN #x00000020)
(define IN_MOVED_FROM #x00000040)
(define IN_MOVED_TO #x00000080)
(define IN_CREATE #x00000100)
(define IN_DELETE #x00000200)
(define IN_DELETE_SELF #x00000400)
(define IN_MOVE_SELF #x00000800)
(define IN_UNMOUNT #x00002000)
(define IN_Q_OVERFLOW #x00004000)
(define IN_IGNORED #x00008000)
(define IN_ONLYDIR #x01000000)
(define IN_DONT_FOLLOW #x02000000)
(define IN_EXCL_UNLINK #x04000000)
(define IN_MASK_ADD #x20000000)
(define IN_ISDIR #x40000000)
(define IN_ONESHOT #x80000000)

;; Create an inotify instance
(define fd (inotify-init))

(define (watch-directory dir)
  "Watch the directory DIR for file changes."
  (inotify-add-watch fd dir (+ IN_CREATE IN_DELETE IN_MODIFY)))

(define (event-loop)
  "Event loop to process inotify events."
  (let* ((buffer (make-bytevector 1024))
         (event-size 4)
         (event (bytevector->pointer buffer)))
    (let loop ()
      (let ((length (read fd event 1024)))
        (if (> length 0)
            (let ((event-type (bytevector-s32-native-ref
                               (pointer->bytevector event 4 0) 0)))

              ;; TODO: should check for event, currently it always returns 1 for some reason
              ;; there might be something wrong with the way the point is converted

              ;; (cond
              ;;  ((logand event-type IN_CREATE)
              ;;   (format #t "File created: ~a\n" name))
              ;;  ((logand event-type IN_DELETE)
              ;;   (format #t "File deleted: ~a\n" name))
              ;;  ((logand event-type IN_MODIFY)
              ;;   (format #t "File modified: ~a\n" name)))
              (sway-reload))
            (loop))))))

(define* (auto-reload-configure #:key directories)
  "Configure auto reload parameters.
Parameters:
	- directories: list of directories to watch for changes."
  (set! DIRECTORIES directories))

(define (auto-reload-init)
  "Start listening for change events on the configured directories"
  (for-each (lambda (dir)
              (format #t "watching directory ~a for auto reload\n" dir)
              (watch-directory (string->pointer dir)))
            DIRECTORIES)

  ;; Start the event loop
  (thread-start! (make-thread event-loop)))

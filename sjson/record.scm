;;; (json record) --- Guile JSON implementation.

;; Copyright (C) 2020-2021 Aleix Conchillo Flaque <aconchillo@gmail.com>
;; Copyright (C) 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;
;; This file is part of guile-json.
;;
;; guile-json is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; guile-json is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with guile-json. If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; JSON module for Guile

;; The initial code of this file was copied from GNU Guix:
;;  http://git.savannah.gnu.org/cgit/guix.git/tree/guix/json.scm

;;; Code:

(define-module (sjson record)
  #:use-module (sjson builder)
  #:use-module (sjson parser)
  #:use-module (srfi srfi-9)
  #:export (<=> define-json-mapping define-json-type))

(define <=> '<=>)

(define-syntax-rule (define-json-reader json->record ctor spec ...)
  "Define JSON->RECORD as a procedure that converts a JSON representation,
read from a port, string, or alist, into a record created by CTOR and following
SPEC, a series of field specifications."
  (define (json->record input)
    (let ((table (cond ((port? input)
                        (json->scm input))
                       ((string? input)
                        (json-string->scm input))
                       ;; This allows to pass native values.
                       ((or (null? input) (pair? input))
                        input))))
      (let-syntax ((extract-field (syntax-rules ()
                                    ((_ table (field key scm->value value->scm))
                                     (scm->value (if (and (pair? (assoc key table)) (not (equal? 'null (cdr (assoc key table)))))
                                                     (cdr (assoc key table)) *unspecified*)))
                                    ((_ table (field key scm->value))
                                     (scm->value (if (and (pair? (assoc key table)) (not (equal? 'null (cdr (assoc key table)))))
                                                     (cdr (assoc key table)) *unspecified*)))
                                    ((_ table (field key))
                                     (if (and (pair? (assoc key table)) (not (equal? 'null (cdr (assoc key table)))))
                                         (cdr (assoc key table)) *unspecified*))
                                    ((_ table (field))
                                     (if (pair? (assoc (symbol->string 'field) table)) (cdr (assoc (symbol->string 'field) table)) *unspecified*)))))
        (ctor (extract-field table spec) ...)))))

(define-syntax-rule (define-json-writer record->json spec ...)
  "Define RECORD->JSON as a procedure that converts a RECORD into its JSON
representation following SPEC, a series of field specifications."
  (define (record->json record)
    (let-syntax ((extract-field (syntax-rules ()
                                  ((_ (field getter key scm->value value->scm))
                                   (cons key (if (unspecified? (getter record)) *unspecified* (value->scm (getter record)))))
                                  ((_ (field getter key scm->value))
                                   (cons key (getter record)))
                                  ((_ (field getter key))
                                   (cons key (getter record)))
                                  ((_ (field getter))
                                   (cons (symbol->string 'field) (getter record))))))
      (let* ((full-object `(,(extract-field spec) ...))
             (object (filter (lambda (p) (not (unspecified? (cdr p))))
                             full-object)))
        (scm->json-string object)))))

(define-syntax-rule (define-native-reader scm->record ctor spec ...)
  "Define SCM->RECORD as a procedure that converts an alist into a record
created by CTOR and following SPEC, a series of field specifications."
  (define (scm->record table)
    (let-syntax ((extract-field (syntax-rules ()
                                  ((_ table (field key scm->value value->scm))
                                   (scm->value (if (pair? (assoc key table)) (cdr (assoc key table)) *unspecified*)))
                                  ((_ table (field key scm->value))
                                   (scm->value (if (pair? (assoc key table)) (cdr (assoc key table)) *unspecified*)))
                                  ((_ table (field key))
                                   (if (pair? (assoc key table)) (cdr (assoc key table)) *unspecified*))
                                  ((_ table (field))
                                   (if (pair? (assoc (symbol->string 'field) table)) (cdr (assoc (symbol->string 'field) table)) *unspecified*)))))
      (ctor (extract-field table spec) ...))))

(define-syntax-rule (define-native-writer record->scm spec ...)
  "Define RECORD->SCM as a procedure that converts a RECORD into it an alist
representation following SPEC, a series of field specifications."
  (define (record->scm record)
    (let-syntax ((extract-field (syntax-rules ()
                                  ((_ (field getter key scm->value value->scm))
                                   (cons key (if (unspecified? (getter record)) *unspecified* (value->scm (getter record)))))
                                  ((_ (field getter key scm->value))
                                   (cons key (getter record)))
                                  ((_ (field getter key))
                                   (cons key (getter record)))
                                  ((_ (field getter))
                                   (cons (symbol->string 'field) (getter record))))))
      (let ((full-object `(,(extract-field spec) ...)))
        (filter (lambda (p) (not (unspecified? (cdr p)))) full-object)))))

(define-syntax define-json-mapping
  (syntax-rules (<=>)
    "Define RTD as a record type with the given FIELDs and GETTERs, à la SRFI-9,
and define JSON->RECORD as a conversion from JSON (from a port, string or alist)
to a record of this type. Optionally, define RECORD->JSON as a conversion from a
record of this type to a JSON string. Additionally, define SCM->RECORD as a
conversion from an alist to a record of this type (equivalent to JSON->RECORD
when passing an alist) and RECORD->SCM as a conversion from a record of this
type to an alist."
    ((_ rtd ctor pred json->record (field getter spec ...) ...)
     (begin
       (define-record-type rtd
         (ctor field ...)
         pred
         (field getter) ...)

       (define-json-reader json->record ctor
         (field spec ...) ...)))
    ((_ rtd ctor pred json->record <=> record->json (field getter spec ...) ...)
     (begin
       (define-record-type rtd
         (ctor field ...)
         pred
         (field getter) ...)

       (define-json-reader json->record ctor
         (field spec ...) ...)

       (define-json-writer record->json
         (field getter spec ...) ...)))
    ((_ rtd ctor pred json->record <=> record->json <=> scm->record <=> record->scm (field getter spec ...) ...)
     (begin
       (define-record-type rtd
         (ctor field ...)
         pred
         (field getter) ...)

       (define-json-reader json->record ctor
         (field spec ...) ...)

       (define-json-writer record->json
         (field getter spec ...) ...)

       (define-native-reader scm->record ctor
         (field spec ...) ...)

       (define-native-writer record->scm
         (field getter spec ...) ...)))))

(define-syntax define-json-type
  (lambda (x)
    "Define RTD as a record type with the given FIELDs. This will automatically
define a record and its constructor, predicate and fields with their getters as
they would be defined by define-json-mapping."
    (define (gen-id template-id . args)
      (datum->syntax
       template-id
       (string->symbol
        (apply string-append
               (map (lambda (x)
                      (if (string? x) x (symbol->string (syntax->datum x))))
                    args)))))
    (define (cleanup-single-rtd template-id)
      (datum->syntax
       template-id
       (string->symbol
        (string-delete
         (lambda (c) (or (eq? c #\<) (eq? c #\>)))
         (symbol->string (syntax->datum template-id))))))
    (define (cleanup-vector-rtd template-id)
      (cleanup-single-rtd (datum->syntax template-id (vector-ref (syntax->datum template-id) 0))))

    (define (cleanup-rtd template-id)
      (if (vector? (syntax->datum template-id))
          (cleanup-vector-rtd template-id)
          (cleanup-single-rtd template-id)))
    (syntax-case x (<=>)
      ((_ rtd field ...)
       (with-syntax ((mapping-rtd #'rtd)
                     (constructor (gen-id #'rtd "make-" (cleanup-rtd #'rtd)))
                     (predicate (gen-id #'rtd (cleanup-rtd #'rtd) "?"))
                     (json->record (gen-id #'rtd "json->" (cleanup-rtd #'rtd)))
                     (record->json (gen-id #'rtd (cleanup-rtd #'rtd) "->json"))
                     (scm->record (gen-id #'rtd "scm->" (cleanup-rtd #'rtd)))
                     (record->scm (gen-id #'rtd (cleanup-rtd #'rtd) "->scm"))
                     ((fields ...)
                      (map
                       (lambda (f)
                         (syntax-case f ()
                           ((name)
                            #`(name #,(gen-id #'rtd (cleanup-rtd #'rtd) "-" #'name)))
                           ((name key)
                            #`(name #,(gen-id #'rtd (cleanup-rtd #'rtd) "-" #'name) key))
                           ((name key field-rtd)
                            #`(name
                               #,(gen-id #'rtd (cleanup-rtd #'rtd) "-" #'name)
                               key
                               #,(if (vector? (syntax->datum #'field-rtd))
                                     #`(lambda (v) (if (unspecified? v)
                                                       *unspecified*
                                                       (map #,(gen-id #'field-rtd "scm->" (cleanup-rtd #'field-rtd))
                                                            (vector->list v))))
                                     #`(lambda (v) (if (unspecified? v)
                                                       *unspecified*
                                                       (#,(gen-id #'field-rtd "scm->" (cleanup-rtd #'field-rtd)) v))))
                               #,(if (vector? (syntax->datum #'field-rtd))
                                     #`(lambda (v)
                                         (list->vector
                                          (map #,(gen-id #'field-rtd (cleanup-rtd #'field-rtd) "->scm") v)))
                                     (gen-id #'field-rtd (cleanup-rtd #'field-rtd) "->scm" ))))))
                       #'(field ...))))
         #'(define-json-mapping mapping-rtd
             constructor
             predicate
             json->record <=> record->json <=> scm->record <=> record->scm
             fields ...))))))

;;; (json record) ends here

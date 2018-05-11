#lang racket
(require "preprocess.rkt")

(provide add-active-token def-active-token process-string)

; File inclusion
(def-active-token "#include" (string)
  (let ([iclude-pattern #px"#include\\s*\"(\\S+)\""])
    (for ([matched-string (regexp-match* iclude-pattern string)])
      (let* ( [filepath (regexp-replace iclude-pattern matched-string "\\1")])
        (set! string (string-replace string matched-string (if (file-exists? filepath ) (file->string filepath) string)))
        )
      )
    string))

; Getter and setter generation
(def-active-token "#getset" (string)
  (let ([pattern #px"#getset\\s*((\\w*)\\s+(\\w+)\\s+(\\w+)\\S*\\s*;)\\s*(\\{\\s*(get;)?\\s*(set;)?\\s*\\})"])
    (for ([matched-string (regexp-match* pattern string)])
      (let* ([attribute (string-trim (regexp-replace pattern matched-string "\\1"))]
             [encapsulation (regexp-replace pattern matched-string "\\2")]
             [attribute-name (regexp-replace pattern matched-string "\\4")]
             [attribute-type (regexp-replace pattern matched-string "\\3")]
             [attribute-get (non-empty-string?(regexp-replace pattern matched-string "\\6"))]
             [attribute-set (non-empty-string?(regexp-replace pattern matched-string "\\7"))])
        (set! string
           (string-replace string matched-string
              (string-append "" attribute "\r\n\r\n"

                 ;; If its the case, generate the getter
                 (if (equal? #t attribute-get)
                     (string-append (if (non-empty-string? encapsulation) (string-append encapsulation " ")  " ")
                                 attribute-type " get" (string-titlecase attribute-name)
                                 "(){\r\n\t return this." attribute-name ";\r\n}\r\n\r\n" )
                     "")
                 ;; If its the case, generate the setter
                 (if (equal? #t attribute-set)
                     (string-append
                        (if (non-empty-string? encapsulation)
                            (string-append encapsulation " ")
                            " ")
                        "void set" (string-titlecase attribute-name) "("
                        attribute-type " " attribute-name "){\r\n\t this."
                        attribute-name "=" attribute-name ";\r\n}\r\n\r\n" )
                     ""))))))
    string))
#lang racket
(provide add-active-token def-active-token process-string)

;; the hash table for the active tokens
(define ht (make-hash))

;; The add active token function
;; receives the token and the function and adds it to the hashtable
(define (add-active-token token function)
  (hash-set! ht token function))

;; defines the syntax rules to use the def-active-token
(define-syntax def-active-token
  (syntax-rules ()
    [(def-active-token token params body)
    (add-active-token token (lambda params body))]))

;; processes the string with the active tokens
(define (process-string string)
  (let ((keys (hash-keys ht))
       (result (string-copy string)))
    (begin
      (for ([key keys])
        (set! result (apply-active-tokens key (hash-ref ht key) result)))
      (if (equal? result string ) result (process-string result)))))

;; Creates a clausure to be used on the hash-for-each with the string to be processed in
;; the apply-active-tokens for each token and respective function
(define (get-proc-function string)
  (lambda (key value) (apply-active-tokens key value string)))

(define (apply-active-tokens key value string)
  (if (equal? #f (regexp-match-positions key string))
      string                                                                     ;; No match, return the string
      (let*
          ((result-string (string-copy string))                                  ;; To store the formatted string
           (match-position (car (regexp-match-positions key result-string)))     ;; The position of the last match with the token
           (match-counts (length (regexp-match-positions* key result-string))))  ;; Number of matches
        (begin
          (for/list ([i match-counts])
            (if (equal? #f match-position) #f
                (begin
                  (set! result-string (string-append
                                       (substring result-string 0 (car match-position))
                                       (value (substring result-string (car match-position)))))
                  (set! match-position (if (regexp-match-positions key result-string)
                                           (car (regexp-match-positions key result-string))
                                           #f)))))
          result-string))))

; Process a string and replace the existing alias in it.
(def-active-token "alias" (string)
  (let ([alias-matcher-pattern #px"\\balias[\\s]+([\\S]+)[\\s]*=[\\s]*([^;]+);"])
    (for ([matched-string (regexp-match* alias-matcher-pattern string)])
      (begin
        ;; take away the matched-string to prevent wrong replacements
        (set! string (string-replace string matched-string ""))

        ;; do the actual matching
        (let* ([alias-name (regexp-replace alias-matcher-pattern matched-string "\\1")]
               [alias-expr (regexp-replace alias-matcher-pattern matched-string "\\2")]
               [is-word (regexp-match #px"^[\\w]*$" alias-name)]
               [alias-name-pattern (pregexp (if (equal? #f is-word) (regexp-quote alias-name) (format "\\b~a\\b" (regexp-quote alias-name)) ))])
           (begin
          (set! string (string-replace string alias-name-pattern alias-expr))))))
    string))

; Infer the type of a var keyword and replace it with the type declared ahead
(def-active-token "var" (string)
  (let ([type-inference-pattern #px"\\bvar([\\s]+[\\w]+[\\s]*=[\\s]*\\bnew\\b[\\s]*([^\\(]+))"])
    (for ([matched-string (regexp-match* type-inference-pattern string)])
      (begin
        (let* ([infered-type (regexp-replace type-inference-pattern matched-string "\\2")]
               [statement-body (regexp-replace type-inference-pattern matched-string "\\1")])
          (set! string (string-replace string matched-string (string-append infered-type statement-body))))))
    string))


; String interpolation
(def-active-token "#" (string)
  (let ([type-inference-pattern #px"#\"([^\"]*)\""]
        [type-inference-pattern-interpolation "#{([^}]+)}"])
    (for ([matched-string (regexp-match* type-inference-pattern string)])
      (begin
        (let* ([statement-body (regexp-replace type-inference-pattern matched-string "\\1")]
               [statement-body-replica (regexp-replace type-inference-pattern matched-string "\\1")])
          (begin
            (for ([matched-interpolation-var (regexp-match* type-inference-pattern-interpolation statement-body)])
              (let* ([statement-var (regexp-replace type-inference-pattern-interpolation matched-interpolation-var "\\1")])
                (set! statement-body  (string-replace statement-body (string-append "#{" statement-var "}") (string-append "\" + (" statement-var ") + \"")))))
            (set! string (string-replace string (string-append "#\"" statement-body-replica "\"") (string-append "\"" statement-body "\"")))))))
    string))
#lang racket

;; the hash table for the active tokens
(define ht (make-hash))

(provide add-active-token def-active-token process-string)

;; the add active token function
(define (add-active-token token function)
  ;; receives the token and the function and adds it to the hashtable
  (hash-set! ht token function)
  )

;; defines the syntax rules to use the def-active-token
(define-syntax def-active-token
  (syntax-rules ()
    [(def-active-token token params body)
    (add-active-token token (lambda params body))]
    )
  )


;; processes the string with the active tokens
(define (process-string string)
  (let
      (
       (keys (hash-keys ht))
       (result (string-copy string))
       )
    (begin
      (for ([key keys])
        (set! result (apply-active-tokens key (hash-ref ht key) result))
        )
      (if (equal? result string ) result (process-string result))
      )
    )
  )

;;Creates a clausure to be used on the hash-for-each with the string to be processed in
;; the apply-active-tokens for each token and respective function
(define (get-proc-function string)
  (lambda (key value) (apply-active-tokens key value string))
  )

(define (apply-active-tokens key value string)
  (if (equal? #f (regexp-match-positions key string))
      string      ;; no match return the string
      (let*
          ((result-string (string-copy string)) ;; to store the formatted string
           (match-position (car (regexp-match-positions key result-string))) ;; the position of the last match with the token
           (match-counts (length (regexp-match-positions* key result-string))) ;; number of matches
           )
        (begin
          (for/list ([i match-counts]) ;; for i< match-counts
            
            (if (equal? #f match-position)
                #f
                (begin
                 ; (writeln (string-append " BEFORE " result-string))
                  ;(writeln (string-append " RESULT: " (~a (car match-position)) "and" (~a (cdr match-position))))
                  (set! result-string (string-append
                                       (substring result-string 0 (car match-position))
                                       (value (substring result-string (car match-position) )) ;; process the string bettween the match
                                       ;; (substring result-string (cdr match-position))
                                       )
                        )
                  (set! match-position (if (regexp-match-positions key result-string)(car (regexp-match-positions key result-string)) #f))
                  ;(writeln (string-append " AFTER " result-string))
                  )
                
                )
            ) ;; aqui obtemos as posições
          result-string
          )
        )
      )
  )



;;;;; Some regex here

;;: Alias matchers

;; Capture the alias name and the matching expression
;; under the named groups 'aliasName' and 'aliasExpr'
; alias[^\S]+(?<aliasName>[^=\s]+)[\s+]*=[\s+]*(?<aliasExpr>[^;]*);
; example: https://regex101.com/r/XYwvMg/2

;; To detect the alias later we can use \b to match a word boundary.
;; This example captures both strings between quotes and aliases named.
;; With some logic we can just ignore the quotes and proceed to replace the bound word tokens.
; example: https://regex101.com/r/5m6Pir/1

;; test stuff
(def-active-token ";;" (str)
  (or (for/or ((c (in-string str))
               (i (in-naturals)))
        (and (char=? c #\newline)
             (substring str (+ i 1))))
""))


; (alias-processor "this alias should survive; maybe this alias=another alias?; alias 123 = body; i guess 123 should change to body;")
; (alias-processor "alias 123 = body; alias 234 = anotherbody; whos better? 123 or 234?")
; (type-inference-processor "somevar string = new String()")
; (type-inference-processor "some var string = newString()")
; (type-inference-processor "var something = new Something<OtherThing>()")
; (type-inference-processor "var whitespaces = new     WhiteSpace<Parameter, Parameter< Spaced,       Parameter>>()")
; (type-inference-processor (alias-processor "alias someAlias=somethingNew<Parameter>; var variable=new someAlias();"))   <- Bugged when alias has a ( character. the () character represent a capture group and in the first instruction of alias processor the string is not striped of this token

; Process a string and replace the existing alias in it.
(def-active-token "alias" (string)
  (let ([alias-matcher-pattern #px"\\balias[\\s]+([\\S]+)[\\s]*=[\\s]*([^;]+);"])
    (for ([matched-string (regexp-match* alias-matcher-pattern string)])
      (begin
        ;; take away the matched-string to prevent wrong replacements
        ;(println "hee")
        ;(println string)
        (set! string (string-replace string matched-string ""))
        ;(println string)
        ;; do the actual matching
        (let* ([alias-name (regexp-replace alias-matcher-pattern matched-string "\\1")]
               [alias-expr (regexp-replace alias-matcher-pattern matched-string "\\2")]
               [is-word (regexp-match #px"^[\\w]*$" alias-name)]
               [alias-name-pattern (pregexp (if (equal? #f is-word) (regexp-quote alias-name) (format "\\b~a\\b" (regexp-quote alias-name)) ))])
           (begin ;(println alias-name) (println alias-expr) (println alias-name-pattern)  (println matched-string) (println string)
          (set! string (string-replace string alias-name-pattern alias-expr))))))
    string))

; Infer the type of a var keyword and replace it with the type declared ahead
(def-active-token "var" (string)
  (let ([type-inference-pattern #px"\\bvar([\\s]+[\\w]+[\\s]*=[\\s]*\\bnew\\b[\\s]*([^\\(]+))"])
    (for ([matched-string (regexp-match* type-inference-pattern string)])
      (begin
        (let* ([infered-type (regexp-replace type-inference-pattern matched-string "\\2")]
               [statement-body (regexp-replace type-inference-pattern matched-string "\\1")])
          (begin ;(println infered-type) (println statement-body) (println matched-string) (println string)
                 
          (set! string (string-replace string matched-string (string-append infered-type statement-body)))))))
    string))


; String interpolation
(def-active-token "#" (string)
  (let (
        [type-inference-pattern #px"#\"([^\"]*)\""]
        [type-inference-pattern-interpolation "#{([^}]+)}"]
        )
    (for ([matched-string (regexp-match* type-inference-pattern string)])
      (begin
        (let* ([statement-body (regexp-replace type-inference-pattern matched-string "\\1")]
               [statement-body-replica (regexp-replace type-inference-pattern matched-string "\\1")])
          (begin
            (for ([matched-interpolation-var (regexp-match* type-inference-pattern-interpolation statement-body)])
              (begin
                (let* ([statement-var (regexp-replace type-inference-pattern-interpolation matched-interpolation-var "\\1")])
                  (set! statement-body  (string-replace statement-body (string-append "#{" statement-var "}") (string-append "\" + (" statement-var ") + \"")))
                  )
                )
              )
            (set! string (string-replace string (string-append "#\"" statement-body-replica "\"") (string-append "\"" statement-body "\"")))
            )
          ) 
        )
      )
    string)
  )



;(process-string "dads ;; dsads
;dsa d")


(define ns (make-base-namespace))
(def-active-token "//eval " (str)
(call-with-input-string
str
(lambda (in)
(string-append (~a (eval (read in) ns))
(port->string in)))))


;(process-string "if (curYear > //eval (date-year (seconds->date (current-seconds)))) {")
;(process-string "var something = new Something<OtherThing>()")
(process-string "public class Foo {
    public String foo(String[] args) {
    	alias a1 = arg1;
    	var a1 = new String[](){(#\"#{args[0]}\" + #\"{a+b}\")};
    	return a1.toString();
    }
}")


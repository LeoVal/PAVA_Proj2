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
  (hash-for-each ht (get-proc-function string))
 )

;;Creates a clausure to be used on the hash-for-each with the string to be processed in
;; the apply-active-tokens for each token and respective function
(define (get-proc-function string)
  (lambda (key value) (apply-active-tokens key value string))
  )

(define (apply-active-tokens key value string)
 (let*
     ((result-string (string-copy string)) ;; to store the formatted string
      (match-position (car (regexp-match-positions key result-string))) ;; the position of the last match with the token
      (match-counts (length (regexp-match-positions* key result-string))) ;; number of matches
      )
   
   (for/list ([i match-counts]) ;; for i< match-counts
     (begin
       (writeln (string-append " BEFORE " result-string))
       (writeln (string-append " RESULT: " (~a (car match-position)) "and" (~a (cdr match-position))))
       (set! result-string (string-append
                            (substring result-string 0 (car match-position))
                            (value (substring result-string (car match-position) (cdr match-position))) ;; process the string bettween the match
                            (substring result-string (cdr match-position))
                            )
             )
       (set! match-position (if (regexp-match-positions key result-string)(car (regexp-match-positions key result-string)) #f))
       )
     (writeln (string-append " AFTER " result-string))
     ) ;; aqui obtemos as posições
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


(process-string "dads ;; dsads ;; dsa d")
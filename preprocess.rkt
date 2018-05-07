#lang racket
(provide add-active-token def-active-token process-string)

(define (add-active-token)
  values "")
(define (def-active-token)
  values "")
(define (process-string string)
  values string)



;;;;; Some regex here

;;: Alias matcher
;; Capture the alias name and the matching expression
;; under the named groups 'aliasName' and 'aliasExpr'
; alias[^\S]+(?<aliasName>[^=\s]+)[\s+]*=[\s+]*(?<aliasExpr>[^;]*);
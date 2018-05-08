#lang racket
(provide add-active-token def-active-token process-string)

(define (add-active-token)
  values "")
(define (def-active-token)
  values "")
(define (process-string string)
  values string)



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
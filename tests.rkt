#lang racket
(require "preprocessextra.rkt")

(define verbose #t)
(define total-tests 0)
(define passed-tests 0)

; Run all tests in the given folder.
; recursive calls ensure nested folders are scanned
(define (run-tests testdir)
  (for ([path (directory-list testdir)])
    (run-file-or-go-deeper (build-path testdir path))))

; If its a file, try and run it, else iterate over its contents.
(define (run-file-or-go-deeper path)
  (if (directory-exists? path)
      (run-tests path)
      (execute-test path)))

; Print the test result
(define (execute-test path)
  (let ([string-path (path->string path)])
  (when (string-suffix? string-path ".in")
  (display (string-append "executing test: [" string-path "]" (compare-outputs path) "\n")))))

; Does a comparison of the contents of the processed in-file
; with the contents of the expected out-file.
; If verbose flag is true then the expectations and result will be printed
(define (compare-outputs path)
  (letrec ([base-file (substring (path->string path) 0 (- (string-length (path->string path)) 3))]
      [in-file (string->path (string-append base-file ".in"))]
      [out-file (string->path (string-append base-file ".out"))]
      [in-string  (file->string (path->string in-file))]
      [out-string (file->string (path->string out-file))]
      ;[test (println path))]
      [process-result (process-string in-string)])
    (increment-total-tests)
    ; Print result and clean windows line-endings
    
    ;(println process-result)
    ;(println out-string)
    (if (equal? process-result out-string)
        (and (increment-passing-tests) (values " \u2713"))
        (values (string-append "\u2718"
                               (if (equal? verbose #t)
                                   (string-append "\n======== output was =========\n"
                                                  (string-replace process-result "\r\n" "\n")
                                                  "\n================================================\n"
                                                  "============= expectation was ==================\n"
                                                  (string-replace out-string "\r\n" "\n"))
                                   (values "")))))))
(define (increment-total-tests)
  (set! total-tests (add1 total-tests)))
(define (increment-passing-tests)
  (set! passed-tests (add1 passed-tests)))
;; Run the tests here
(run-tests (path->complete-path (build-path (current-directory) "resources")))
(print (string-append "Result: " (number->string passed-tests) "/" (number->string total-tests) " tests passing"))
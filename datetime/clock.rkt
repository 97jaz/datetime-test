#lang racket/base

(require rackunit
         datetime/clock)

(module+ test
  (require rackunit/text-ui)
  (run-tests clock-tests))

(define clock-tests
  (test-suite
    "Clock parameter and procedures"

    (test-case "current-clock"
      (parameterize ([current-clock (λ () 123)])
        (check-equal? ((current-clock)) 123)))))
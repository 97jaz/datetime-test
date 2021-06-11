#lang racket/base

(require rackunit
         datetime/datetime/tz
         datetime/generic/tz-provider)

(module+ test
  (require rackunit/text-ui)
  (run-tests tz-provider-tests))

(define tz-provider-tests
  (test-suite
    "TZ provider generics"

    (test-case "->tz"
      (check-equal? (->tz (datetime/tz 2000 #:tz "Europe/Moscow")) "Europe/Moscow"))

    (test-case "->datetime/tz"
      (check-equal? (->datetime/tz (datetime/tz 2000 #:tz "America/New_York"))
                    (datetime/tz 2000 #:tz "America/New_York"))
      (check-equal? (->datetime/tz (datetime/tz 2000 #:tz "America/New_York") "UTC")
                    (datetime/tz 2000 1 1 5 #:tz "UTC")))))

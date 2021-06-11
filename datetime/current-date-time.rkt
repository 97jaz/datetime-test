#lang racket/base

(require rackunit
         datetime/clock
         datetime/current-date-time
         datetime/date
         datetime/datetime
         datetime/datetime/offset
         datetime/datetime/tz
         datetime/time
         datetime/zone)

(module+ test
  (require rackunit/text-ui)
  (run-tests current-date-time-tests))

(define current-date-time-tests
  (test-suite
    "Current date and time"

    (parameterize ([current-clock (λ () 0)]
                   [current-time-zone "America/New_York"])
      
      (test-case "current-date"
        (check-equal? (current-date) (date 1969 12 31))
        (check-equal? (current-date "UTC") (date 1970)))

      (test-case "current-time"
        (check-equal? (current-time) (time 19))
        (check-equal? (current-time "UTC") MIDNIGHT))

      (test-case "current-datetime"
        (check-equal? (current-datetime) (datetime 1969 12 31 19))
        (check-equal? (current-datetime "UTC") (datetime 1970)))

      (test-case "current-datetime/offset"
        (check-equal? (current-datetime/offset) (datetime/offset 1969 12 31 19 #:offset -18000))
        (check-equal? (current-datetime/offset "UTC") (datetime/offset 1970 #:offset 0)))

      (test-case "current-datetime/tz"
        (check-equal? (current-datetime/tz) (datetime/tz 1969 12 31 19 #:tz "America/New_York"))
        (check-equal? (current-datetime/tz "UTC") (datetime/tz 1970 #:tz "UTC"))))))
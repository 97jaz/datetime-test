#lang racket/base

(require rackunit
         datetime/datetime
         datetime/datetime/offset
         datetime/datetime/tz
         datetime/generic/utc-offset-provider
         (prefix-in base: (only-in racket/base date date*)))

(module+ test
  (require rackunit/text-ui)
  (run-tests utc-offset-provider-tests))

(define utc-offset-provider-tests
  (test-suite
    "UTC offset provider generics"

    (test-case "->datetime/offset"
      (check-equal? (->datetime/offset (datetime/offset 2000 #:offset 0))
                    (datetime/offset 2000 #:offset 0))
      (check-equal? (->datetime/offset (datetime/offset 2000 #:offset 0) -18000)
                    (datetime/offset 1999 12 31 19 #:offset -18000))

      (check-equal? (->datetime/offset (datetime/tz 2000 #:tz "UTC"))
                    (datetime/offset 2000 #:offset 0))
      (check-equal? (->datetime/offset (datetime/tz 2000 #:tz "UTC") -18000)
                    (datetime/offset 1999 12 31 19 #:offset -18000))

      (check-equal? (->datetime/offset (base:date* 0 0 0 1 1 2000 0 0 #f 0 0 ""))
                    (datetime/offset 2000 #:offset 0))

      (check-equal? (->datetime/offset (base:date* 0 0 0 1 1 2000 0 0 #f 0 0 "") -18000)
                    (datetime/offset 1999 12 31 19 #:offset -18000)))

    (test-case "->utc-offset"
      (check-equal? (->utc-offset (datetime/offset 2000 #:offset 986)) 986)
      (check-equal? (->utc-offset (datetime/tz 2000 #:tz "America/New_York")) -18000)
      (check-equal? (->utc-offset (base:date* 0 0 0 1 1 2000 0 0 #f 0 0 "")) 0))

    (test-case "->local-datetime"
      (check-equal? (->local-datetime (datetime/offset 2000 #:offset -18000))
                    (datetime 2000))
      (check-equal? (->local-datetime (datetime/tz 2000 #:tz "America/New_York"))
                    (datetime 2000))
      (check-equal? (->local-datetime (base:date* 0 0 0 1 1 2000 0 0 #f -18000 0 ""))
                    (datetime 2000)))

    (test-case "->utc-datetime"
      (check-equal? (->utc-datetime (datetime/offset 2000 #:offset -18000))
                    (datetime 2000 1 1 5))
      (check-equal? (->utc-datetime (datetime/tz 2000 #:tz "America/New_York"))
                    (datetime 2000 1 1 5))
      (check-equal? (->utc-datetime (base:date* 0 0 0 1 1 2000 0 0 #f -18000 0 ""))
                    (datetime 2000 1 1 5)))))
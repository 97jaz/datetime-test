#lang racket/base

(require rackunit
         datetime/date
         datetime/datetime
         datetime/exn
         datetime/period
         datetime/datetime/offset
         datetime/datetime/tz
         datetime/generic/datetime-provider
         datetime/offset-resolver
         datetime/private/time/util
         (prefix-in base: (only-in racket/base date date*)))

(module+ test
  (require rackunit/text-ui)
  (run-tests datetime-provider-tests))

(define datetime-provider-tests
  (test-suite
    "datetime provider generics"

    (test-case "datetime-provider?"
      (check-false (datetime-provider? (date 2000)))
      (check-true (datetime-provider? (datetime 2000)))
      (check-true (datetime-provider? (datetime/offset 2000 #:offset 0)))
      (check-true (datetime-provider? (datetime/tz 2000 #:tz "UTC")))
      (check-true (datetime-provider? (base:date* 0 0 0 1 1 2000 0 0 #f 0 0 "")))
      (check-false (datetime-provider? (years 6))))

    (test-case "->datetime"
      (let ([t (datetime 2017 8 12 2 34 9 234)])
        (check-equal? (->datetime t) t)
        (check-equal? (->datetime (datetime/offset 2017 8 12 2 34 9 234 #:offset -18000)) t)
        (check-equal? (->datetime (datetime/tz 2017 8 12 2 34 9 234 #:tz "America/Denver")) t)
        (check-equal? (->datetime (base:date* 9 34 2 12 8 2017 0 0 #f 0 234 "")) t)))

    (test-case "->posix"
      (check-equal? (->posix (datetime 1970)) 0)
      (check-equal? (->posix (datetime/offset 1950 6 20 #:offset -14400)) -616449600)
      (check-equal? (->posix (datetime/tz 1970 #:tz "Etc/UTC")) 0)
      (check-equal? (->posix (datetime/tz 1950 6 20 #:tz "America/New_York")) -616449600)
      (check-equal? (->posix (base:date* 0 0 0 20 6 1950 0 0 #t -14400 0 "")) -616449600))

    (test-case "->jd"
      (check-equal? (->jd (datetime 1970)) (+ 2440587 1/2))
      (check-equal? (->jd (datetime/offset 2014 12 8 7 #:offset -18000)) 2457000)
      (check-equal? (->jd (datetime/tz 2014 12 8 7 #:tz "America/New_York")) 2457000)
      (check-equal? (->jd (base:date* 0 0 7 8 12 2014 0 0 #f -18000 0 "")) 2457000))

    (test-case "period-between"
      (check-exn exn:fail:contract?
                 (λ () (period-between (datetime/offset 2000 #:offset 0)
                                       (datetime 2000))))
      (check-equal? (period-between (datetime 2000) (datetime 1970))
                    (years -30))
      (check-equal? (period-between
                     (datetime/tz 1970 #:tz "America/New_York")
                     (datetime/tz 2017 1 29 17 16 30 123456789 #:tz "America/New_York"))
                    (period [years 47] [weeks 4]
                            [hours 17] [minutes 16] [seconds 30]
                            [milliseconds 123] [microseconds 456] [nanoseconds 789]))
      (check-equal? (period-between
                     (datetime/offset 1970 #:offset -18000)
                     (datetime/offset 2017 1 29 17 16 30 123456789 #:offset -18000))
                    (period [years 47] [weeks 4]
                            [hours 17] [minutes 16] [seconds 30]
                            [milliseconds 123] [microseconds 456] [nanoseconds 789]))
      (check-equal? (period-between
                     (base:date* 0 0 0 1 1 1970 4 0 #f -18000 0 "")
                     (base:date* 30 16 17 29 1 2017 0 28 #f -18000 123456789 ""))
                    (period [years 47] [weeks 4]
                            [hours 17] [minutes 16] [seconds 30]
                            [milliseconds 123] [microseconds 456] [nanoseconds 789])))))
                     

      
      
#lang racket/base

(require rackunit
         datetime/date
         datetime/datetime
         datetime/exn
         datetime/period
         datetime/datetime/offset
         datetime/datetime/tz
         datetime/generic/datetime-arithmetic-provider
         datetime/offset-resolver
         (prefix-in base: (only-in racket/base date date*)))

(module+ test
  (require rackunit/text-ui)
  (run-tests datetime-arithmetic-provider-tests))

(define datetime-arithmetic-provider-tests
  (test-suite
    "datetime arithmetic provider generics"

    (test-case "datetime-arithmetic-provider?"
      (check-false (datetime-arithmetic-provider? 3))
      (check-false (datetime-arithmetic-provider? (date 2000)))
      (check-true (datetime-arithmetic-provider? (datetime 2000)))
      (check-true (datetime-arithmetic-provider? (datetime/offset 2000 #:offset 0)))
      (check-true (datetime-arithmetic-provider? (datetime/tz 2000 #:tz "UTC")))
      (check-true (datetime-arithmetic-provider? (base:date* 0 0 2 8 3 2016 0 0 #f -18000 0 "")))
      (check-true (datetime-arithmetic-provider? (period))))

    (test-case "+period"
      (check-exn exn:fail:contract?
                 (λ () (+period (date 2000) (years 5))))
      (check-equal? (+period (datetime 2000) (period [years 6] [months -12] [hours 14]))
                    (datetime 2005 1 1 14))
      (check-equal? (+period (datetime/offset 2017 #:offset -18000)
                             (period [weeks 2] [seconds 5]))
                    (datetime/offset 2017 1 15 0 0 5 #:offset -18000))
      (check-equal? (+period (datetime/tz 2017 #:tz "America/New_York")
                             (period [months -6] [hours 5]))
                    (datetime/tz 2016 7 1 5 #:tz "America/New_York"))
      (check-equal? (+period (base:date* 0 0 0 1 1 2017 0 0 #f 0 0 "")
                             (period [years -2]))
                    (base:date* 0 0 0 1 1 2015 4 0 #f 0 0 ""))
      (check-equal? (+period (years 5) (period [years -2] [hours 3]))
                    (period [years 3] [hours 3]))

      ;; We'll land on the start of DST and should pass right over it to 3AM
      (check-equal? (+period (datetime/tz 2017 3 11 1 #:tz "America/New_York")
                             (period [days 1] [hours 1]))
                    (datetime/tz 2017 3 12 3 #:tz "America/New_York")))

    (test-case "-period"
      (check-exn exn:fail:contract?
                 (λ () (-period (date 200) (years 2))))
      (check-equal? (-period (datetime 2017) (hours 15))
                    (datetime 2016 12 31 9))
      (check-equal? (-period (datetime/offset 2017 #:offset 0) (hours 15))
                    (datetime/offset 2016 12 31 9 #:offset 0))
      (check-equal? (-period (datetime/tz 2017 #:tz "America/New_York") (hours 15))
                    (datetime/tz 2016 12 31 9 #:tz "America/New_York"))
      (check-equal? (-period (base:date* 0 0 0 1 1 2017 0 0 #f 0 0 "") (hours 15))
                    (base:date* 0 0 9 31 12 2016 6 365 #f 0 0 ""))
      (check-equal? (-period (period [years -5] [days 2] [milliseconds 2000])
                             (days -20))
                    (period [years -5] [days 22] [milliseconds 2000]))

      ;; We'll land in an overlapping period at the end of DST and should
      ;; retain the offset we started in.
      (let* ([t1 (datetime/tz 2017 12 5 1 30 #:tz "America/New_York")]
             [t2 (-period t1 (months 1))]
             [t3 (-period t2 (hours 1))])
        (check-equal? (datetime/tz->utc-offset t1) -18000)
        (check-equal? (datetime/tz->utc-offset t2) -18000)
        (check-equal? (datetime/tz->utc-offset t3) -14400)
        (check-equal?
         t2
         (datetime/tz 2017 11 5 1 30
                      #:tz "America/New_York"
                      #:offset-resolver (with-overlap-resolver resolve/raise
                                                               overlap-resolver/post)))
        (check-equal?
         t3
         (datetime/tz 2017 11 5 1 30
                      #:tz "America/New_York"
                      #:offset-resolver (with-overlap-resolver resolve/raise
                                                               overlap-resolver/pre)))))))
                       
                             
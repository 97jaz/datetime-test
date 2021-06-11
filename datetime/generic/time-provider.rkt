#lang racket/base

(require rackunit
         datetime/date
         datetime/time
         datetime/datetime
         datetime/exn
         datetime/period
         datetime/datetime/offset
         datetime/datetime/tz
         datetime/generic/time-provider
         datetime/offset-resolver
         datetime/private/time/util
         (prefix-in base: (only-in racket/base date date*)))

(module+ test
  (require rackunit/text-ui)
  (run-tests time-provider-tests))

(define time-provider-tests
  (test-suite
    "time provider generics"

    (test-case "time-provider?"
      (check-false (time-provider? 3))
      (check-false (time-provider? (date 2000)))
      (check-true (time-provider? MIDNIGHT))
      (check-true (time-provider? (datetime 2000)))
      (check-true (time-provider? (datetime/offset 2000 #:offset 0)))
      (check-true (time-provider? (datetime/tz 2000 #:tz "UTC")))
      (check-true (time-provider? (base:date* 0 0 0 1 1 2000 0 0 #f 0 0 ""))))

    (test-case "same-time-provider-type?"
      (check-true (same-time-provider-type? MIDNIGHT NOON))
      (check-false (same-time-provider-type? MIDNIGHT (datetime 2000))))

    (test-case "->time"
      (check-equal? (->time MIDNIGHT) MIDNIGHT)
      (check-equal? (->time (datetime 2000)) MIDNIGHT)
      (check-equal? (->time (datetime 2000 5 1 12)) NOON)
      (check-equal? (->time (datetime/offset 2017 1 29 20 53 #:offset 0)) (time 20 53))
      (check-equal? (->time (datetime/tz 1970 1 1 1 2 3 4)) (time 1 2 3 4))
      (check-equal? (->time (base:date* 1 2 3 1 1 1970 0 0 #f 0 0 "")) (time 3 2 1 0)))

    (test-case "->hours"
      (check-equal? (->hours NOON) 12)
      (check-equal? (->hours (datetime 2000 1 1 13 14 15 1666)) 13)
      (check-equal? (->hours (datetime/offset 2000 #:offset 0)) 0)
      (check-equal? (->hours (datetime/tz 2000 1 1 16 30)) 16)
      (check-equal? (->hours (base:date* 1 2 3 1 1 1970 0 0 #f 0 0 "")) 3))

    (test-case "->minutes"
      (check-equal? (->minutes NOON) 0)
      (check-equal? (->minutes (datetime 2000 1 1 0 40)) 40)
      (check-equal? (->minutes (datetime/offset 2000 1 1 0 40 #:offset 0)) 40)
      (check-equal? (->minutes (datetime/tz 2000 1 1 0 40)) 40)
      (check-equal? (->minutes (base:date* 1 2 3 1 1 1970 0 0 #f 0 0 "")) 2))

    (test-case "->seconds"
      (check-equal? (->seconds (time 0 0 13 200)) 13)
      (check-equal? (->seconds (time 0 0 13 200) #f) 13)
      (check-equal? (->seconds (time 0 0 13 200) #t) (+ 13 (/ 200 1000000000)))
      (check-equal? (->seconds (datetime 2000 1 1 12 0 0 500000000)) 0)
      (check-equal? (->seconds (datetime 2000 1 1 12 0 0 500000000) #t) (/ 1 2))
      (check-equal? (->seconds (datetime/offset 2000 1 1 12 0 50 500000000 #:offset 0)) 50)
      (check-equal? (->seconds (datetime/offset 2000 1 1 12 0 50 500000000 #:offset 0) #t) (+ 50 1/2))
      (check-equal? (->seconds (datetime/tz 2000 1 1 12 0 50 500000000)) 50)
      (check-equal? (->seconds (datetime/tz 2000 1 1 12 0 50 500000000) #t) (+ 50 1/2))
      (check-equal? (->seconds (base:date* 50 0 12 1 1 2000 0 0 #f 0 500000000 ""))
                    50)
      (check-equal? (->seconds (base:date* 50 0 12 1 1 2000 0 0 #f 0 500000000 "") #t)
                    (+ 50 1/2)))

    (test-case "->milliseconds"
      (check-equal? (->milliseconds (time 0 0 0 200000000)) 200)
      (check-equal? (->milliseconds (datetime 2000 1 1 0 0 0 200000000)) 200)
      (check-equal? (->milliseconds (datetime/offset 2000 1 1 0 0 0 200000000 #:offset 0)) 200)
      (check-equal? (->milliseconds (datetime/tz 2000 1 1 0 0 0 200000000)) 200)
      (check-equal? (->milliseconds (base:date* 0 0 0 1 1 2000 0 0 #f 0 200000000 "")) 200))

    (test-case "->microseconds"
      (check-equal? (->microseconds (time 0 0 0 200000000)) 200000)
      (check-equal? (->microseconds (datetime 2000 1 1 0 0 0 200000000)) 200000)
      (check-equal? (->microseconds (datetime/offset 2000 1 1 0 0 0 200000000 #:offset 0)) 200000)
      (check-equal? (->microseconds (datetime/tz 2000 1 1 0 0 0 200000000)) 200000)
      (check-equal? (->microseconds (base:date* 0 0 0 1 1 2000 0 0 #f 0 200000000 "")) 200000))

    (test-case "->nanoseconds"
      (check-equal? (->nanoseconds (time 0 0 0 200000000)) 200000000)
      (check-equal? (->nanoseconds (datetime 2000 1 1 0 0 0 200000000)) 200000000)
      (check-equal? (->nanoseconds (datetime/offset 2000 1 1 0 0 0 200000000 #:offset 0)) 200000000)
      (check-equal? (->nanoseconds (datetime/tz 2000 1 1 0 0 0 200000000)) 200000000)
      (check-equal? (->nanoseconds (base:date* 0 0 0 1 1 2000 0 0 #f 0 200000000 "")) 200000000))

    (test-case "hours-between"
      (check-equal? (hours-between MIDNIGHT (time 13 59)) 13)
      (check-equal? (hours-between (datetime 2000) (datetime 2000 1 1 13 59)) 13)
      (check-equal? (hours-between (datetime/offset 2015 3 8 1 #:offset 0)
                                   (datetime/offset 2015 3 8 3 #:offset 0))
                    2)
      (check-equal? (hours-between (datetime/tz 2015 3 8 1 #:tz "America/New_York")
                                   (datetime/tz 2015 3 8 3 #:tz "America/New_York"))
                    1)
      (check-equal? (hours-between (datetime/tz 2015 3 8 1 #:tz "Etc/UTC")
                                   (datetime/tz 2015 3 8 3 #:tz "Etc/UTC"))
                    2)
      (check-equal? (hours-between (base:date* 0 0 1 8 3 2015 0 0 #f 0 0 "")
                                   (base:date* 0 0 3 8 3 2015 0 0 #f 0 0 ""))
                    2))

    (test-case "minutes-between"
      (check-equal? (minutes-between MIDNIGHT (time 13 59)) (+ (* 13 60) 59))
      (check-equal? (minutes-between (time 23 59) MIDNIGHT) (- (* 23 -60) 59))
      (check-equal? (minutes-between (datetime 2000) (datetime 2000 1 1 1)) 60)
      (check-equal? (minutes-between (datetime/offset 2000 #:offset -18000)
                                     (datetime/offset 2000 #:offset 0))
                    -300)
      (check-equal? (minutes-between (datetime/tz 2000 #:tz "America/New_York")
                                     (datetime/tz 2000 #:tz "Etc/UTC"))
                    -300)
      (check-equal? (minutes-between (base:date* 0 0 0 1 1 2000 0 0 #f -18000 0 "")
                                     (base:date* 0 0 0 1 1 2000 0 0 #f 0 0 ""))
                    -300))

    (test-case "seconds-between"
      (check-equal? (seconds-between NOON (time 11)) (* 60 -60))
      (check-equal? (seconds-between (datetime 2000) (datetime 2000 1 1 1)) 3600)
      (check-equal? (seconds-between (datetime/offset 2000 #:offset -18000)
                                     (datetime/offset 2000 #:offset 0))
                    -18000)
      (check-equal? (seconds-between (datetime/tz 2000 #:tz "America/New_York")
                                     (datetime/tz 2000 #:tz "Etc/UTC"))
                    -18000)
      (check-equal? (seconds-between (base:date* 0 0 0 1 1 2000 0 0 #f -18000 0 "")
                                     (base:date* 0 0 0 1 1 2000 0 0 #f 0 0 ""))
                    -18000))

    (test-case "milliseconds-between"
      (check-equal? (milliseconds-between NOON (time 11)) (* 60 -60 1000))
      (check-equal? (milliseconds-between (datetime 2000) (datetime 2000 1 1 1)) 3600000)
      (check-equal? (milliseconds-between (datetime/tz 2000 #:tz "America/New_York")
                                          (datetime/tz 2000 #:tz "UTC"))
                    -18000000)
      (check-equal? (milliseconds-between (datetime/offset 2000 #:offset -18000)
                                          (datetime/offset 2000 #:offset 0))
                    -18000000)
      (check-equal? (milliseconds-between (base:date* 0 0 0 1 1 2000 0 0 #f -18000 0 "")
                                          (base:date* 0 0 0 1 1 2000 0 0 #f 0 0 ""))
                    -18000000))

    (test-case "microseconds-between"
      (check-equal? (microseconds-between NOON (time 11)) (* 60 -60 1000000))
      (check-equal? (microseconds-between (datetime 2000) (datetime 2000 1 1 1)) 3600000000)
      (check-equal? (microseconds-between (datetime/tz 2000 #:tz "America/New_York")
                                          (datetime/tz 2000 #:tz "UTC"))
                    -18000000000)
      (check-equal? (microseconds-between (datetime/offset 2000 #:offset -18000)
                                          (datetime/offset 2000 #:offset 0))
                    -18000000000)
      (check-equal? (microseconds-between (base:date* 0 0 0 1 1 2000 0 0 #f -18000 0 "")
                                          (base:date* 0 0 0 1 1 2000 0 0 #f 0 0 ""))
                    -18000000000))

    (test-case "nanoseconds-between"
      (check-equal? (nanoseconds-between NOON (time 11)) (* 60 -60 1000000000))
      (check-equal? (nanoseconds-between (datetime 2000) (datetime 2000 1 1 1)) 3600000000000)
      (check-equal? (nanoseconds-between (datetime/tz 2000 #:tz "America/New_York")
                                         (datetime/tz 2000 #:tz "UTC"))
                    -18000000000000)
      (check-equal? (nanoseconds-between (datetime/offset 2000 #:offset -18000)
                                         (datetime/offset 2000 #:offset 0))
                    -18000000000000)
      (check-equal? (nanoseconds-between (base:date* 0 0 0 1 1 2000 0 0 #f -18000 0 "")
                                         (base:date* 0 0 0 1 1 2000 0 0 #f 0 0 ""))
                    -18000000000000))

    (test-case "time-period-between"
      (check-equal? (time-period-between NOON (time 11) '(hours))
                    (hours -1))
      (check-equal? (time-period-between (datetime 2000) (datetime 2000 1 1 1) '(minutes))
                    (minutes 60))
      (check-equal? (time-period-between (datetime/offset 2000 #:offset -18000)
                                         (datetime/offset 2000 #:offset 0)
                                         '(seconds))
                    (seconds -18000))
      (check-equal? (time-period-between (datetime/offset 2000 #:offset -18000)
                                         (datetime/offset 2000 #:offset 0)
                                         '(milliseconds))
                    (milliseconds -18000000))
      (check-equal? (time-period-between (base:date* 0 0 0 1 1 2000 0 0 #f -18000 0 "")
                                         (base:date* 0 0 0 1 1 2000 0 0 #f 0 0 "")
                                         '(microseconds))
                    (microseconds -18000000000)))))
      
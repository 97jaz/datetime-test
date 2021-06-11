#lang racket/base

(require rackunit
         datetime/date
         datetime/time
         datetime/datetime
         datetime/period
         datetime/datetime/offset
         datetime/datetime/tz
         datetime/generic/time-arithmetic-provider
         (prefix-in base: (only-in racket/base date date*)))

(module+ test
  (require rackunit/text-ui)
  (run-tests time-arithmetic-provider-tests))

(define time-arithmetic-provider-tests
  (test-suite
    "time arithmetic provider generics"

    (test-case "time-arithmetic-provider?"
      (check-false (time-arithmetic-provider? (date 2000)))
      (check-true (time-arithmetic-provider? (time 3)))
      (check-true (time-arithmetic-provider? (datetime 2000)))
      (check-true (time-arithmetic-provider? (datetime/offset 2000 #:offset 0)))
      (check-true (time-arithmetic-provider? (datetime/tz 2000 #:tz "UTC")))
      (check-true (time-arithmetic-provider? (base:date* 0 0 0 1 1 2000 0 0 #f 0 0 "")))
      (check-true (time-arithmetic-provider? (period [years 3] [minutes 3]))))

    (test-case "+hours"
      (check-equal? (+hours (time 0) 36) (time 12))
      (check-equal? (+hours (time 12) -4) (time 8))
      (check-equal? (+hours (datetime 2000 2 2 15 30 20) 26) (datetime 2000 2 3 17 30 20))
      (check-equal? (+hours (datetime/offset 2015 3 8 1 #:offset -18000) 1)
                    (datetime/offset 2015 3 8 2 #:offset -18000))
      (check-equal? (+hours (datetime/tz 2015 3 8 1 #:tz "America/New_York") 1)
                    (datetime/tz 2015 3 8 3 #:tz "America/New_York"))
      (check-equal? (+hours (base:date* 20 30 15 2 2 2000 0 0 #f 0 0 "") 26)
                    (base:date* 20 30 17 3 2 2000 4 33 #f 0 0 ""))
      (check-equal? (+hours (seconds 2) -6) (period [hours -6] [seconds 2])))

    
    (test-case "-hours"
      (check-equal? (-hours (time 12) 36) (time 0))
      (check-equal? (-hours (time 8) -4) (time 12))
      (check-equal? (-hours (datetime 2000 2 3 17 30 20) 26) (datetime 2000 2 2 15 30 20))
      (check-equal? (-hours (datetime/offset 2015 3 8 2 #:offset -18000) 1)
                    (datetime/offset 2015 3 8 1 #:offset -18000))
      (check-equal? (-hours (datetime/tz 2015 3 8 3 #:tz "America/New_York") 1)
                    (datetime/tz 2015 3 8 1 #:tz "America/New_York"))
      (check-equal? (-hours (base:date* 20 30 17 3 2 2000 4 33 #f 0 0 "") 26)
                    (base:date* 20 30 15 2 2 2000 3 32 #f 0 0 ""))
      (check-equal? (-hours (period [hours -6] [seconds 2]) -6) (seconds 2)))

    (test-case "+minutes"
      (check-equal? (+minutes (time 0) 121) (time 2 1))
      (check-equal? (+minutes (datetime 2000 2 28 23 59) 1) (datetime 2000 2 29))
      (check-equal? (+minutes (datetime/offset 1970 #:offset 0) -12)
                    (datetime/offset 1969 12 31 23 48 #:offset 0))
      (check-equal? (+minutes (datetime/tz 1970 #:tz "Etc/UTC") -12)
                    (datetime/tz 1969 12 31 23 48 #:tz "Etc/UTC"))
      (check-equal? (+minutes (base:date* 0 0 0 1 1 1970 0 0 #f 0 0 "") -12)
                    (base:date* 0 48 23 31 12 1969 3 364 #f 0 0 ""))
      (check-equal? (+minutes (years 2) 9) (period [years 2] [minutes 9])))

    (test-case "-minutes"
      (check-equal? (-minutes (time 2 1) 121) (time 0))
      (check-equal? (-minutes (datetime 2000 2 29) 1) (datetime 2000 2 28 23 59))
      (check-equal? (-minutes (datetime/offset 1969 12 31 23 48 #:offset 0) -12)
                    (datetime/offset 1970 #:offset 0))
      (check-equal? (-minutes (datetime/tz 1969 12 31 23 48 #:tz "Etc/UTC") -12)
                    (datetime/tz 1970 #:tz "Etc/UTC"))
      (check-equal? (-minutes (base:date* 0 48 23 31 12 1969 3 364 #f 0 0 "") -12)
                    (base:date* 0 0 0 1 1 1970 4 0 #f 0 0 ""))
      (check-equal? (-minutes (period [years 2] [minutes 9]) 9) (years 2)))

      
    (test-case "+seconds"
      (check-equal? (+seconds (time 0) 121) (time 0 2 1))
      (check-equal? (+seconds (datetime 2000 2 28 23 59 59) 1) (datetime 2000 2 29))
      (check-equal? (+seconds (datetime/offset 1970 #:offset 0) -12)
                    (datetime/offset 1969 12 31 23 59 48 #:offset 0))
      (check-equal? (+seconds (datetime/tz 1970 #:tz "Etc/UTC") -12)
                    (datetime/tz 1969 12 31 23 59 48 #:tz "Etc/UTC"))
      (check-equal? (+seconds (base:date* 0 0 0 1 1 1970 0 0 #f 0 0 "") -12)
                    (base:date* 48 59 23 31 12 1969 3 364 #f 0 0 ""))
      (check-equal? (+seconds (weeks 2) 12) (period [weeks 2] [seconds 12])))

    (test-case "-seconds"
      (check-equal? (-seconds (time 0 2 1) 121) (time 0))
      (check-equal? (-seconds (datetime 2000 2 29) 1) (datetime 2000 2 28 23 59 59))
      (check-equal? (-seconds (datetime/offset 1969 12 31 23 59 48 #:offset 0) -12)
                    (datetime/offset 1970 #:offset 0))
      (check-equal? (-seconds (datetime/tz 1969 12 31 23 59 48 #:tz "Etc/UTC") -12)
                    (datetime/tz 1970 #:tz "Etc/UTC"))
      (check-equal? (-seconds (base:date* 48 59 23 31 12 1969 3 364 #f 0 0 "") -12)
                    (base:date* 0 0 0 1 1 1970 4 0 #f 0 0 ""))
      (check-equal? (-seconds (period [weeks 2] [seconds 12]) 12) (weeks 2)))

    (test-case "+milliseconds"
      (check-equal? (+milliseconds (time 0) 121) (time 0 0 0 121000000))
      (check-equal? (+milliseconds (datetime 2000 2 28 23 59 59) 1)
                    (datetime 2000 2 28 23 59 59 1000000))
      (check-equal? (+milliseconds (datetime/offset 1970 #:offset 0) -12)
                    (datetime/offset 1969 12 31 23 59 59 988000000 #:offset 0))
      (check-equal? (+milliseconds (datetime/tz 1970 #:tz "Etc/UTC") -12)
                    (datetime/tz 1969 12 31 23 59 59 988000000 #:tz "Etc/UTC"))
      (check-equal? (+milliseconds (base:date* 0 0 0 1 1 1970 0 0 #f 0 0 "") -12)
                    (base:date* 59 59 23 31 12 1969 3 364 #f 0 988000000 ""))
      (check-equal? (+milliseconds (days 80) 34) (period [days 80] [milliseconds 34])))

    (test-case "-milliseconds"
      (check-equal? (-milliseconds (time 0 0 0 121000000) 121) (time 0))
      (check-equal? (-milliseconds (datetime 2000 2 28 23 59 59 1000000) 1)
                    (datetime 2000 2 28 23 59 59))
      (check-equal? (-milliseconds (datetime/offset 1969 12 31 23 59 59 988000000 #:offset 0) -12)
                    (datetime/offset 1970 #:offset 0))
      (check-equal? (-milliseconds (datetime/tz 1969 12 31 23 59 59 988000000 #:tz "Etc/UTC") -12)
                    (datetime/tz 1970 #:tz "Etc/UTC"))
      (check-equal? (-milliseconds (base:date* 59 59 23 31 12 1969 3 364 #f 0 988000000 "") -12)
                    (base:date* 0 0 0 1 1 1970 4 0 #f 0 0 ""))
      (check-equal? (-milliseconds (period [days 80] [milliseconds 34]) 34) (days 80)))

    (test-case "+mircoseconds"
      (check-equal? (+microseconds (time 0) 121) (time 0 0 0 121000))
      (check-equal? (+microseconds (datetime 2000 2 28 23 59 59) 1)
                    (datetime 2000 2 28 23 59 59 1000))
      (check-equal? (+microseconds (datetime/offset 1970 #:offset 0) -12)
                    (datetime/offset 1969 12 31 23 59 59 999988000 #:offset 0))
      (check-equal? (+microseconds (datetime/tz 1970 #:tz "Etc/UTC") -12)
                    (datetime/tz 1969 12 31 23 59 59 999988000 #:tz "Etc/UTC"))
      (check-equal? (+microseconds (base:date* 0 0 0 1 1 1970 4 0 #f 0 0 "") -12)
                    (base:date* 59 59 23 31 12 1969 3 364 #f 0 999988000 ""))
      (check-equal? (+microseconds (seconds 20) 100) (period [seconds 20] [microseconds 100])))

    (test-case "-microseconds"
      (check-equal? (-microseconds (time 0 0 0 121000) 121) (time 0))
      (check-equal? (-microseconds (datetime 2000 2 28 23 59 59 1000) 1)
                    (datetime 2000 2 28 23 59 59))
      (check-equal? (-microseconds (datetime/offset 1969 12 31 23 59 59 999988000 #:offset 0) -12)
                    (datetime/offset 1970 #:offset 0))
      (check-equal? (-microseconds (datetime/tz 1969 12 31 23 59 59 999988000 #:tz "Etc/UTC") -12)
                    (datetime/tz 1970 #:tz "Etc/UTC"))
      (check-equal? (-microseconds (base:date* 59 59 23 31 12 1969 3 364 #f 0 999988000 "") -12)
                    (base:date* 0 0 0 1 1 1970 4 0 #f 0 0 ""))
      (check-equal? (-microseconds (period [seconds 20] [microseconds 100]) 100) (seconds 20)))

    (test-case "+nanoseconds"
      (check-equal? (+nanoseconds (time 0) 121) (time 0 0 0 121))
      (check-equal? (+nanoseconds (datetime 2000 2 28 23 59 59) 1)
                    (datetime 2000 2 28 23 59 59 1))
      (check-equal? (+nanoseconds (datetime/offset 1970 #:offset 0) -12)
                    (datetime/offset 1969 12 31 23 59 59 999999988 #:offset 0))
      (check-equal? (+nanoseconds (datetime/tz 1970 #:tz "Etc/UTC") -12)
                    (datetime/tz 1969 12 31 23 59 59 999999988 #:tz "Etc/UTC"))
      (check-equal? (+nanoseconds (base:date* 0 0 0 1 1 1970 4 0 #f 0 0 "") -12)
                    (base:date* 59 59 23 31 12 1969 3 364 #f 0 999999988 ""))
      (check-equal? (+nanoseconds (years 1) 1) (period [years 1] [nanoseconds 1])))

    (test-case "-nanoseconds"
      (check-equal? (-nanoseconds (time 0 0 0 121) 121) (time 0))
      (check-equal? (-nanoseconds (datetime 2000 2 28 23 59 59 1) 1)
                    (datetime 2000 2 28 23 59 59))
      (check-equal? (-nanoseconds (datetime/offset 1969 12 31 23 59 59 999999988 #:offset 0) -12)
                    (datetime/offset 1970 #:offset 0))
      (check-equal? (-nanoseconds (datetime/tz 1969 12 31 23 59 59 999999988 #:tz "Etc/UTC") -12)
                    (datetime/tz 1970 #:tz "Etc/UTC"))
      (check-equal? (-nanoseconds (base:date* 59 59 23 31 12 1969 3 364 #f 0 999999988 "") -12)
                    (base:date* 0 0 0 1 1 1970 4 0 #f 0 0 ""))
      (check-equal? (-nanoseconds (period [years 1] [nanoseconds 1]) 1) (years 1)))

    (test-case "+time-period"
      (check-equal? (+time-period (time 15 30) (hours -70))
                    (-hours (time 15 30) 70))
      (check-equal? (+time-period (datetime 2000 9 9 8 30 28 6789)
                                  (period [hours -345]
                                          [minutes 7364]
                                          [seconds 9907]
                                          [nanoseconds -3000000000000]))
                    (let* ([t (datetime 2000 9 9 8 30 28 6789)]
                           [t (-hours t 345)]
                           [t (+minutes t 7364)]
                           [t (+seconds t 9907)]
                           [t (+nanoseconds t -3000000000000)])
                      t))
      (check-equal? (+time-period (datetime/offset 1970 #:offset 0)
                                  (period [hours 8] [minutes 10]))
                    (datetime/offset 1970 1 1 8 10 #:offset 0))
      (check-equal? (+time-period (datetime/tz 1970 #:tz "Etc/UTC")
                                  (period [hours 8] [minutes 10]))
                    (datetime/tz 1970 1 1 8 10 #:tz "Etc/UTC"))
      (check-equal? (+time-period (base:date* 0 0 0 1 1 1970 4 0 #f 0 0 "")
                                  (period [hours 8] [minutes 10]))
                    (base:date* 0 10 8 1 1 1970 4 0 #f 0 0 "")))

    (test-case "-time-period"
      (check-equal? (-time-period (-hours (time 15 30) 70) (hours -70))
                    (time 15 30))
      (check-equal? (-time-period
                     (let* ([t (datetime 2000 9 9 8 30 28 6789)]
                            [t (-hours t 345)]
                            [t (+minutes t 7364)]
                            [t (+seconds t 9907)]
                            [t (+nanoseconds t -3000000000000)])
                       t)
                     (period [hours -345]
                             [minutes 7364]
                             [seconds 9907]
                             [nanoseconds -3000000000000]))
                    (datetime 2000 9 9 8 30 28 6789))
      (check-equal? (-time-period (datetime/offset 1970 1 1 8 10 #:offset 0)
                                  (period [hours 8] [minutes 10]))
                    (datetime/offset 1970 #:offset 0))
      (check-equal? (-time-period (datetime/tz 1970 1 1 8 10 #:tz "Etc/UTC")
                                  (period [hours 8] [minutes 10]))
                    (datetime/tz 1970 #:tz "Etc/UTC"))
      (check-equal? (-time-period (base:date* 0 10 8 1 1 1970 4 0 #f 0 0 "")
                                  (period [hours 8] [minutes 10]))
                    (base:date* 0 0 0 1 1 1970 4 0 #f 0 0 "")))))
      
      
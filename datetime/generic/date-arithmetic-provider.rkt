#lang racket/base

(require rackunit
         datetime/date
         datetime/datetime
         datetime/exn
         datetime/period
         datetime/datetime/offset
         datetime/datetime/tz
         datetime/generic/date-arithmetic-provider
         datetime/offset-resolver
         datetime/private/time/util
         (prefix-in base: (only-in racket/base date date*)))

(module+ test
  (require rackunit/text-ui)
  (run-tests date-arithmetic-provider-tests))

(define date-arithmetic-provider-tests
  (test-suite
    "date arithmetic provider generics"

    (test-case "date-arithmetic-provider?"
      (check-true (date-arithmetic-provider? (date 2000)))
      (check-true (date-arithmetic-provider? (datetime 2000)))
      (check-true (date-arithmetic-provider? (datetime/offset 2000 #:offset 0)))
      (check-true (date-arithmetic-provider? (datetime/tz 2000)))
      (check-true (date-arithmetic-provider? (base:date* 0 0 0 1 1 2000 0 0 #f 0 0 "whatev")))
      (check-true (date-arithmetic-provider? (period [days 7] [seconds 1]))))

    (test-case "+years"
      (check-equal? (+years (date 2000 5 1) 20) (date 2020 5 1))
      (check-equal? (+years (date 2000 2 29) 5) (date 2005 2 28))
      (check-equal? (+years (datetime 1981 4 15 12) -10) (datetime 1971 4 15 12))
      (check-equal? (+years (datetime/offset 2000 #:offset 123) -1) (datetime/offset 1999 #:offset 123))
      (check-equal? (+years (base:date* 0 0 2 8 3 2016 0 0 #f -18000 0 "") -1)
                    (base:date* 0 0 2 8 3 2015 0 66 #f -18000 0 ""))
      (check-equal? (+years (datetime/tz 2014 3 8 2 #:tz "America/New_York") 1)
                    (datetime/tz 2015 3 8 3 #:tz "America/New_York"))
      (check-equal? (+years (datetime/tz 2016 3 8 2 #:tz "America/New_York") -1)
                    (datetime/tz 2015 3 8 3 #:tz "America/New_York")))

    (test-case "-years"
      (check-equal? (-years (date 1970) -4) (date 1974))
      (check-equal? (-years (date 1970) 4) (date 1966))
      (check-equal? (-years (datetime 1980 2 29) -1) (datetime 1981 2 28))
      (check-equal? (-years (datetime 1980 2 29) 1) (datetime 1979 2 28))
      (check-equal? (-years (datetime/offset 2016 3 8 2 #:offset -18000) 1)
                    (datetime/offset 2015 3 8 2 #:offset -18000))
      (check-equal? (-years (base:date* 0 0 2 8 3 2016 0 0 #f -18000 0 "") -1)
                    (base:date* 0 0 2 8 3 2017 3 66 #f -18000 0 ""))
      (check-equal? (-years (datetime/tz 2014 3 8 2 #:tz "America/New_York") -1)
                    (datetime/tz 2015 3 8 3 #:tz "America/New_York"))
      (check-equal? (-years (datetime/tz 2016 3 8 2 #:tz "America/New_York") 1)
                    (datetime/tz 2015 3 8 3 #:tz "America/New_York"))
      
      (check-exn exn:datetime:invalid-offset?
                 (λ ()
                   (-years (datetime/tz 2016 3 8 2 #:tz "America/New_York")
                           1
                           #:offset-resolver resolve/raise))))

    (test-case "+months"
      (check-equal? (+months (date 1970) 4) (date 1970 5))
      (check-equal? (+months (date 1970) -4) (date 1969 9))
      (check-equal? (+months (datetime 1979 1 29) 1) (datetime 1979 2 28))
      (check-equal? (+months (datetime 1979 3 29) -1) (datetime 1979 2 28))
      (check-equal? (+months (base:date* 0 0 2 8 4 2015 0 0 #f -14400 0 "") -1)
                    (base:date* 0 0 2 8 3 2015 0 66 #f -14400 0 ""))
      (check-equal? (+months (datetime/offset 2015 4 8 2 #:offset -14400) -1)
                    (datetime/offset 2015 3 8 2 #:offset -14400))
      (check-equal? (+months (datetime/tz 2015 2 8 2 #:tz "America/New_York") 1)
                    (datetime/tz 2015 3 8 3 #:tz "America/New_York"))
      (check-equal? (+months (datetime/tz 2015 4 8 2 #:tz "America/New_York") -1)
                    (datetime/tz 2015 3 8 3 #:tz "America/New_York")))

    (test-case "-months"
      (check-equal? (-months (date 1970) -4) (date 1970 5))
      (check-equal? (-months (date 1970) 4) (date 1969 9))
      (check-equal? (-months (datetime 1979 1 29) -1) (datetime 1979 2 28))
      (check-equal? (-months (datetime 1979 3 29) 1) (datetime 1979 2 28))
      (check-equal? (-months (base:date* 0 0 2 8 4 2015 0 0 #f -14400 0 "") 1)
                    (base:date* 0 0 2 8 3 2015 0 66 #f -14400 0 ""))
      (check-equal? (-months (datetime/offset 2015 4 8 2 #:offset -14400) 1)
                    (datetime/offset 2015 3 8 2 #:offset -14400))
      (check-equal? (-months (datetime/tz 2015 2 8 2 #:tz "America/New_York") -1)
                    (datetime/tz 2015 3 8 3 #:tz "America/New_York"))
      (check-equal? (-months (datetime/tz 2015 4 8 2 #:tz "America/New_York") 1)
                    (datetime/tz 2015 3 8 3 #:tz "America/New_York")))

    (test-case "+weeks"
      (check-equal? (+weeks (date 1970) 6) (date 1970 2 12))
      (check-equal? (+weeks (date 1970) -6) (date 1969 11 20))
      (check-equal? (+weeks (datetime 1980 1 4 12) 8) (datetime 1980 2 29 12))
      (check-equal? (+weeks (datetime 1980 2 29 12) -8) (datetime 1980 1 4 12))
      (check-equal? (+weeks (base:date 0 0 0 1 3 2015 0 0 #f -18000) 1)
                    (base:date 0 0 0 8 3 2015 0 66 #f -18000))
      (check-equal? (+weeks (datetime/offset 2015 3 1 #:offset -18000) 1)
                    (datetime/offset 2015 3 8 #:offset -18000))
      (check-equal?
       (+weeks (datetime/tz 2015 3 1 2
                            #:tz "America/Los_Angeles"
                            #:offset-resolver resolve/raise)
               1)
       (datetime/tz 2015 3 8 3 #:tz "America/Los_Angeles")))

    (test-case "-weeks"
      (check-equal? (-weeks (date 1970) -6) (date 1970 2 12))
      (check-equal? (-weeks (date 1970) 6) (date 1969 11 20))
      (check-equal? (-weeks (datetime 1980 1 4 12) -8) (datetime 1980 2 29 12))
      (check-equal? (-weeks (datetime 1980 2 29 12) 8) (datetime 1980 1 4 12))
      (check-equal? (-weeks (base:date 0 0 0 8 3 2015 0 0 #f -18000) 1)
                    (base:date 0 0 0 1 3 2015 0 59 #f -18000))
      (check-equal? (-weeks (datetime/offset 2015 3 8 #:offset -18000) 1)
                    (datetime/offset 2015 3 1 #:offset -18000))
      (check-equal?
       (-weeks (datetime/tz 2015 3 1 2
                            #:tz "America/Los_Angeles"
                            #:offset-resolver resolve/raise)
               -1)
       (datetime/tz 2015 3 8 3 #:tz "America/Los_Angeles")))

    (test-case "+days"
      (check-equal? (+days (date 1980) (* 365 2)) (date 1981 12 31))
      (check-equal? (+days (datetime 1980 11 15 10) (* 365 2)) (datetime 1982 11 15 10))
      (check-equal? (+days (base:date 0 0 2 7 3 2015 0 0 #f -25200) 1)
                    (base:date 0 0 2 8 3 2015 0 66 #f -25200))
      (check-equal? (+days (datetime/offset 2015 3 7 2 #:offset -25200) 1)
                    (datetime/offset 2015 3 8 2 #:offset -25200))
      (check-exn exn:datetime:invalid-offset?
                 (λ ()
                   (+days (datetime/tz 2015 3 7 2 #:tz "America/Denver")
                          1
                          #:offset-resolver resolve/raise)))
      (check-equal? (+days (datetime/tz 2015 3 7 2 #:tz "America/Denver") 1)
                    (datetime/tz 2015 3 8 3 #:tz "America/Denver")))

    (test-case "-days"
      (check-equal? (-days (date 1980) (* 365 -2)) (date 1981 12 31))
      (check-equal? (-days (datetime 1980 11 15 10) (* 365 -2)) (datetime 1982 11 15 10))
      (check-equal? (-days (base:date 0 0 2 7 3 2015 0 0 #f -25200) -1)
                    (base:date 0 0 2 8 3 2015 0 66 #f -25200))
      (check-equal? (-days (datetime/offset 2015 3 8 2 #:offset -25200) 1)
                    (datetime/offset 2015 3 7 2 #:offset -25200))
      (check-exn exn:datetime:invalid-offset?
                 (λ ()
                   (-days (datetime/tz 2015 3 7 2 #:tz "America/Denver")
                          -1
                          #:offset-resolver resolve/raise)))
      (check-equal? (-days (datetime/tz 2015 3 7 2 #:tz "America/Denver") -1)
                    (datetime/tz 2015 3 8 3 #:tz "America/Denver")))

    (test-case "+date-period"
      (check-equal? (+date-period (date 1980 6 5) (days 10)) (date 1980 6 15))
      (check-equal? (+date-period (datetime 2000 10 10) (weeks -2)) (datetime 2000 9 26))
      (check-equal? (+date-period (base:date 0 0 0 1 1 2000 0 0 #f -18000)
                                  (period [years 2] [days 31]))
                    (base:date 0 0 0 1 2 2002 5 31 #f -18000))
      (check-equal? (+date-period (datetime/offset 2015 8 30 #:offset 123) (months 23))
                    (datetime/offset 2017 7 30 #:offset 123))
      (check-equal? (+date-period (datetime/tz 2015 8 30 #:tz "UTC") (months 23))
                    (datetime/tz 2017 7 30 #:tz "UTC"))
      (check-equal? (+date-period (date 1990) (years 10)) (date 2000))
      (check-equal? (+date-period (date 1950) (period [years 6] [months 2] [days 15]))
                    (date 1956 3 16))
      (check-equal? (+date-period (months -3) (period [years 2] [months 3])
                                  #:offset-resolver resolve/raise)
                    (years 2)))

    (test-case "-date-period"
      (check-equal? (-date-period (date 1956 3 16) (period [years 6] [months 2] [days 15]))
                    (date 1950))
      (check-equal? (-date-period (datetime 1956 3 16) (period [years 6] [months 2] [days 15]))
                    (datetime 1950))
      (check-equal? (-date-period (datetime/tz 2017 7 30 #:tz "Europe/Paris") (months 23))
                    (datetime/tz 2015 8 30 #:tz "Europe/Paris"))
      (check-equal? (-date-period (days 28) (years 50))
                    (period [days 28] [years -50])))))
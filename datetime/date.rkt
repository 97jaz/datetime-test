#lang racket/base

(require rackunit
         racket/match
         racket/serialize
         datetime/date)

(module+ test
  (require rackunit/text-ui)
  (run-tests date-tests))

(define the-date (date 2000 4 15))
(define other-date (date 1970 1 1))

(define date-tests
  (test-suite
    "date functions"

    (test-case
      "date"
      (match (date -20)
        [(date y m d) (check-equal? (list y m d) (list -20 1 1))]
        [_ (fail "date match-expander")])

      (match (date 100 2)
        [(date y m d) (check-equal? (list y m d) (list 100 2 1))]
        [_ (fail "date match-expander")])

      (match (date 2000 6 20)
        [(date y m d) (check-equal? (list y m d) (list 2000 6 20))]
        [_ (fail "date match-expander")])

      (check-exn exn:fail:contract?
                 (λ () (date 1983 2 29))))
    
    (test-case
      "date?"
      (check-false (date? 3))
      (check-true (date? the-date)))

    (test-case
      "date->year"
      (check-eqv? (date->year the-date) 2000))

    (test-case
      "date->month"
      (check-eqv? (date->month the-date) 4))

    (test-case
      "date->day"
      (check-eqv? (date->day the-date) 15))

    (test-case
      "date->iso8601"
      (check-equal? (date->iso8601 the-date) "2000-04-15"))

    (test-case
      "date=?"
      (check-false (date=? the-date other-date))
      (check-true (date=? the-date (date 2000 4 15))))

    (test-case
      "date<?"
      (check-false (date<? the-date other-date))
      (check-false (date<? the-date the-date))
      (check-true (date<? other-date the-date)))

    (test-case
      "date<=?"
      (check-false (date<=? the-date other-date))
      (check-true (date<=? the-date the-date))
      (check-true (date<=? other-date the-date)))

    (test-case
      "date>?"
      (check-false (date>? other-date the-date))
      (check-false (date>? the-date the-date))
      (check-true (date>? the-date other-date)))

    (test-case
      "date>=?"
      (check-false (date>=? other-date the-date))
      (check-true (date>=? the-date the-date))
      (check-true (date>=? the-date other-date)))

    (test-case
      "date->jdn"
      (check-eqv? (date->jdn the-date) 2451650)
      (check-eqv? (date->jdn other-date) 2440588))

    (test-case
      "jdn->date"
      (check-equal? (jdn->date 2451650) the-date)
      (check-equal? (jdn->date 2440588) other-date))

    (test-case
      "date->wday"
      (check-eqv? (date->wday the-date) 6)
      (check-eqv? (date->wday other-date) 4))

    (test-case
      "date->iso-wday"
      (check-eqv? (date->iso-wday the-date) 6)
      (check-eqv? (date->iso-wday other-date) 4)
      (check-eqv? (date->iso-wday (date 2017)) 7))

    (test-case
      "date->yday"
      (check-eqv? (date->yday the-date) 106)
      (check-eqv? (date->yday other-date) 1))

    (test-case
      "date->quarter"
      (check-eqv? (date->quarter the-date) 2)
      (check-eqv? (date->quarter other-date) 1))

    (test-case
      "date->iso-week+wyear"
      (let-values ([(w1 y1) (date->iso-week+wyear the-date)]
                   [(w2 y2) (date->iso-week+wyear other-date)]
                   [(w3 y3) (date->iso-week+wyear (date 2017 1 1))])
        (check-eqv? w1 15)
        (check-eqv? y1 2000)
        (check-eqv? w2 1)
        (check-eqv? y2 1970)
        (check-eqv? w3 52)
        (check-eqv? y3 2016)))

    (test-case
      "date->iso-week"
      (check-eqv? (date->iso-week the-date) 15)
      (check-eqv? (date->iso-week (date 2017 1 1)) 52))

    (test-case
      "date->iso-wyear"
      (check-eqv? (date->iso-wyear the-date) 2000)
      (check-eqv? (date->iso-wyear (date 2017 1 1)) 2016))

    (test-case
      "date-add-years"
      (check-equal? (date-add-years (date 1970) 4) (date 1974))
      (check-equal? (date-add-years (date 1970) -4) (date 1966)))
      
    (test-case
      "date-add-months"
      (check-equal? (date-add-months (date 1970) 4) (date 1970 5))
      (check-equal? (date-add-months (date 1970) -4) (date 1969 9)))

    (test-case
      "date-add-days"
      (check-equal? (date-add-days (date 1980) (* 365 2)) (date 1981 12 31)))

    (test-case
      "date-months-between"
      (check-eqv? (date-months-between (date 1970 1 1) (date 1968 6 20)) -18))

    (test-case
      "date-days-between"
      (check-eqv? (date-days-between (date 2000) (date 2001)) 366))

    (test-case
      "leap-year?"
      (check-true (leap-year? 2000))
      (check-false (leap-year? 1900))
      (check-true (leap-year? 1980))
      (check-false (leap-year? 1983)))

    (test-case
      "days-in-year"
      (check-eqv? (days-in-year 2000) 366)
      (check-eqv? (days-in-year 1900) 365)
      (check-eqv? (days-in-year 1980) 366)
      (check-eqv? (days-in-year 1983) 365))

    (test-case
      "days-in-month"
      (check-eqv? (days-in-month 2000 2) 29)
      (check-eqv? (days-in-month 1900 2) 28)
      (check-eqv? (days-in-month 14 1) 31)
      (check-eqv? (days-in-month 100 4) 30))

    (test-case
      "iso-weeks-in-year"
      (check-eqv? (iso-weeks-in-year 2004) 53)
      (check-eqv? (iso-weeks-in-year 2005) 52)
      (check-eqv? (iso-weeks-in-year 2007) 52))

    (test-case
      "[serialization]"
      (check-equal? the-date (deserialize (serialize the-date))))

    (test-case "last-day-of-month"
      (check-equal? (last-day-of-month 2000 1) (date 2000 1 31))
      (check-equal? (last-day-of-month 2000 2) (date 2000 2 29)))

    (test-case "nth-wday-in-month"
      ;; 1st Tuesday March 2017
      (check-equal? (nth-wday-in-month 2017 3 2 1) (date 2017 3 7))
      ;; 2nd Thursday July 2017
      (check-equal? (nth-wday-in-month 2017 7 4 2) (date 2017 7 13))
      ;; Last Wednesday December 2017 (strange API)
      (check-equal? (nth-wday-in-month 2018 1 3 0) (date 2017 12 27))
      ;; 20th Sunday of the year, 2017
      (check-equal? (nth-wday-in-month 2017 1 0 20) (date 2017 5 14)))

    (test-case "next-wday"
      (let ([d (date 2011 1 15)])
        (check-equal? (next-wday d 1) (date 2011 1 17))
        (check-equal? (next-wday d 3) (date 2011 1 19))
        (check-equal? (next-wday d 6) (date 2011 1 22))

        (check-equal? (next-wday d 1 #:include-current? #t) (date 2011 1 17))
        (check-equal? (next-wday d 3 #:include-current? #t) (date 2011 1 19))
        (check-equal? (next-wday d 6 #:include-current? #t) d)))

    (test-case "prev-wday"
      (let ([d (date 2011 1 15)])
        (check-equal? (prev-wday d 1) (date 2011 1 10))
        (check-equal? (prev-wday d 3) (date 2011 1 12))
        (check-equal? (prev-wday d 6) (date 2011 1 8))
      
        (check-equal? (prev-wday d 1 #:include-current? #t) (date 2011 1 10))
        (check-equal? (prev-wday d 3 #:include-current? #t) (date 2011 1 12))
        (check-equal? (prev-wday d 6 #:include-current? #t) d)))))
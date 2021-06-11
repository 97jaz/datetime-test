#lang racket/base

(require rackunit
         racket/match
         racket/serialize
         datetime/date
         datetime/time
         datetime/datetime)

(module+ test
  (require rackunit/text-ui)
  (run-tests datetime-tests))

(define dt (datetime 2001 2 15 13 20 40 12))

(define datetime-tests
  (test-suite
    "datetime functions"

    (test-case
      "datetime"
      (match (datetime 2000)
        [(datetime y mo d h mi s n)
         (check-equal? (list y mo d h mi s n) (list 2000 1 1 0 0 0 0))]
        [_ (fail "datetime match-expander")])

      (match (datetime 2000)
        [(datetime d t)
         (check-equal? (list d t) (list (date 2000) MIDNIGHT))]
        [_ (fail "datetime match-expander")]))

    (test-case
      "datetime?"
      (check-false (datetime? 3))
      (check-true (datetime? dt)))

    (test-case
      "date->datetime"
      (check-equal? (date->datetime (date 2017 1 15)) (datetime 2017 1 15))
      (check-equal? (date->datetime (date 2001 2 15) (time 13 20 40 12))
                    dt))

    (test-case
      "datetime->iso8601"
      (check-equal? (datetime->iso8601 dt) "2001-02-15T13:20:40.000000012"))

    (test-case
      "datetime=?"
      (check-true (datetime=? dt (datetime 2001 2 15 13 20 40 12)))
      (check-false (datetime=? dt (datetime 2001))))

    (test-case
      "datetime<?"
      (check-true (datetime<? dt (datetime 2001 2 15 13 20 40 13)))
      (check-false (datetime<? dt dt))
      (check-false (datetime<? dt (datetime 2001 2 15 13 20 40 11))))

    (test-case
      "datetime<=?"
      (check-true (datetime<=? dt (datetime 2001 2 15 13 20 40 13)))
      (check-true (datetime<=? dt dt))
      (check-false (datetime<=? dt (datetime 2001 2 15 13 20 40 11))))

    (test-case
      "datetime>?"
      (check-false (datetime>? dt (datetime 2001 2 15 13 20 40 13)))
      (check-false (datetime>? dt dt))
      (check-true (datetime>? dt (datetime 2001 2 15 13 20 40 11))))

    (test-case
      "datetime>=?"
      (check-false (datetime>=? dt (datetime 2001 2 15 13 20 40 13)))
      (check-true (datetime>=? dt dt))
      (check-true (datetime>=? dt (datetime 2001 2 15 13 20 40 11))))

    (test-case
      "datetime->date"
      (check-equal? (datetime->date dt) (date 2001 2 15)))

    (test-case
      "datetime->time"
      (check-equal? (datetime->time dt) (time 13 20 40 12)))

    (test-case
      "datetime->jd"
      (check-equal? (datetime->jd (datetime -4713 11 24 12)) 0)
      (check-equal? (datetime->jd (datetime 1970)) (+ 2440587 1/2)))

    (test-case
      "jd->datetime"
      (check-equal? (jd->datetime 0) (datetime -4713 11 24 12))
      (check-equal? (jd->datetime (+ 2440587 1/2)) (datetime 1970)))

    (test-case
      "datetime-add-months"
      (check-equal? (datetime-add-months dt -23) (datetime 1999 3 15 13 20 40 12)))

    (test-case
      "datetime-add-days"
      (check-equal? (datetime-add-days dt 50) (datetime 2001 4 6 13 20 40 12)))

    (test-case
      "datetime-add-nanoseconds"
      (check-equal? (datetime-add-nanoseconds dt -12)
                    (datetime 2001 2 15 13 20 40))
      (check-equal? (datetime-add-nanoseconds dt (* 1000000000 60 60))
                    (datetime 2001 2 15 14 20 40 12)))

    (test-case
      "datetime-months-between"
      (check-equal? (datetime-months-between dt (datetime 2001 3 12)) 0)
      (check-equal? (datetime-months-between (datetime 2002 9 13) dt) -18))

    (test-case
      "datetime-days-between"
      (check-equal? (datetime-days-between (datetime 2000) (datetime 2001)) 366)
      (check-equal? (datetime-days-between (datetime 2000 3 1) (datetime 2000 2 28)) -2))

    (test-case
      "datetime-nanoseconds-between"
      (check-equal? (datetime-nanoseconds-between dt dt) 0)
      (check-equal? (datetime-nanoseconds-between (datetime 2000) (datetime 2000 1 1 1))
                    3600000000000))
      
    (test-case
      "datetime->posix"
      (check-equal? (datetime->posix (datetime 1970)) 0)
      (check-equal? (datetime->posix (datetime 2017 1 14 16 35 8 569083000))
                    (+ 1484411708 569083000/1000000000)))

    (test-case
      "posix->datetime"
      (check-equal? (posix->datetime 0) (datetime 1970))
      (check-equal? (posix->datetime (+ 1484411708 569083000/1000000000))
                    (datetime 2017 1 14 16 35 8 569083000)))

    (test-case
      "posix->jd"
      (check-equal? (posix->jd -1)
                    (- (+ 2440587 1/2) (/ 1 (* 60 60 24)))))

    (test-case
      "jd->posix"
      (check-equal? (jd->posix 2457770)
                    (datetime->posix (datetime 2017 1 16 12))))

    (test-case
      "[serialization]"
      (check-equal? dt (deserialize (serialize dt))))))


    
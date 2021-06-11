#lang racket/base

(require rackunit
         racket/match
         racket/serialize
         datetime/date
         datetime/time
         datetime/datetime
         datetime/datetime/offset
         datetime/zone)

(module+ test
  (require rackunit/text-ui)
  (run-tests datetime/offset-tests))

(define dto (datetime/offset 2001 2 15 13 20 40 12 #:offset -18000))

(define datetime/offset-tests
  (test-suite
    "datetime/offset functions"

    (test-case
      "datetime/offset"
      (match (datetime/offset 2017 #:offset -18000)
        [(datetime/offset y mo d h mi s n o)
         (check-equal? (list y mo d h mi s n o) (list 2017 1 1 0 0 0 0 -18000))])

      (match (datetime/offset 1017 #:offset -18000)
        [(datetime/offset d t o)
         (check-equal? (list d t o) (list (date 2017) MIDNIGHT -18000))])

      (match dto
        [(datetime/offset dt o)
         (check-equal? (list dt o) (list (datetime 2001 2 15 13 20 40 12) -18000))]))

    (test-case
      "datetime/offset?"
      (check-true (datetime/offset? dto))
      (check-false (datetime/offset? (datetime 2000))))

    (test-case
      "datetime/offset->date"
      (check-equal? (datetime/offset->date dto) (date 2001 2 15)))

    (test-case
      "datetime/offset->time"
      (check-equal? (datetime/offset->time dto) (time 13 20 40 12)))

    (test-case
      "datetime/offset->utc-offset"
      (check-equal? (datetime/offset->utc-offset dto) -18000))

    (test-case
      "datetime/offset-at-utc-offset"
      (check-equal? (datetime/offset-at-utc-offset dto 0)
                    (datetime/offset 2001 2 15 18 20 40 12 #:offset 0))
      (check-equal? (datetime/offset-at-utc-offset dto) dto)
      (check-equal? (datetime/offset-at-utc-offset (datetime/offset 2000 #:offset -1))
                    (datetime/offset 2000 #:offset -1)))

    (test-case
      "datetime->datetime/offset"
      (check-equal? (datetime->datetime/offset (datetime 2001) #:offset 3600)
                    (datetime/offset 2001 #:offset 3600))
      (check-equal? (datetime->datetime/offset (datetime 2001 2 15 13 20 40 12) #:offset -18000)
                    dto))

    (test-case
      "datetime/offset->posix"
      (check-equal? (datetime/offset->posix (datetime/offset 1970 #:offset 0)) 0)
      (check-equal? (datetime/offset->posix (datetime/offset 1970 #:offset -18000)) 18000))

    (test-case
      "datetime/offset->jd"
      (check-equal? (datetime/offset->jd (datetime/offset -4713 11 24 12 #:offset -18000))
                    (/ 18000 (* 60 60 24)))
      (check-equal? (datetime/offset->jd (datetime/offset 1970 #:offset -18000))
                    (+ 2440587 1/2 (/ 18000 (* 60 60 24)))))

    (test-case
      "datetime/offset->iso8601"
      (check-equal? (datetime/offset->iso8601 dto) "2001-02-15T13:20:40.000000012-05:00")
      (check-equal? (datetime/offset->iso8601 (datetime/offset 1970 #:offset 0))
                    "1970-01-01T00:00:00Z"))

    (test-case
      "datetime/offset->string"
      (check-equal? (datetime/offset->string (datetime/offset 1970 #:offset 0))
                    "1970-01-01T00:00:00Z")
      (check-equal? (datetime/offset->string (datetime/offset 1970 #:offset 22))
                    "1970-01-01T00:00:00+00:00:22"))

    (test-case
      "datetime/offset->utc-datetime"
      (check-equal? (datetime/offset->utc-datetime (datetime/offset 1970 #:offset 0))
                    (datetime 1970))
      (check-equal? (datetime/offset->utc-datetime (datetime/offset 1970 #:offset -18000))
                    (datetime 1970 1 1 5)))

    (test-case
      "datetime/offset->local-datetime"
      (check-equal? (datetime/offset->local-datetime (datetime/offset 1970 #:offset -18000))
                    (datetime 1970)))

    (test-case
      "datetime/offset-add-months"
      (check-equal? (datetime/offset-add-months (datetime/offset 2015 4 8 2 #:offset -14400) -1)
                    (datetime/offset 2015 3 8 2 #:offset -14400)))

    (test-case
      "datetime/offset-add-days"
      (check-equal? (datetime/offset-add-days (datetime/offset 2015 3 7 2 #:offset -25200) 1)
                    (datetime/offset 2015 3 8 2 #:offset -25200)))

    (test-case
      "datetime/offset-add-nanoseconds"
      (check-equal? (datetime/offset-add-nanoseconds (datetime/offset 1970 #:offset 0) -12)
                    (datetime/offset 1969 12 31 23 59 59 999999988 #:offset 0)))

    (test-case
      "datetime/offset-months-between"
      (check-equal? (datetime/offset-months-between (datetime/offset 2000 #:offset 0)
                                                    (datetime/offset 2000 4 #:offset -18000))
                    3))

    (test-case
      "datetime/offset-days-between"
      (check-equal? (datetime/offset-days-between (datetime/offset 2015 3 29 #:offset 3600)
                                                  (datetime/offset 2015 3 30 #:offset 7200))
                    0))

    (test-case
      "datetime/offset-nanoseconds-between"
      (check-equal? (datetime/offset-nanoseconds-between (datetime/offset 2000 #:offset -18000)
                                                         (datetime/offset 2000 #:offset 0))
                    -18000000000000))

    (test-case
      "datetime/offset=?"
      (let* ([t1 (datetime/offset -1000 #:offset 0)]
             [t2 (datetime/offset 0 #:offset 0)]
             [t3 (datetime/offset 1000 #:offset 0)])
        (check-true (datetime/offset<? t1 t2))
        (check-true (datetime/offset<? t2 t3))
        (check-true (datetime/offset<=? t1 t3))
        (check-true (datetime/offset>? t2 t1))
        (check-true (datetime/offset>=? t3 t2))
        (check-false (datetime/offset=? t1 t3))

        (check-eq? (datetime/offset-order t1 t1) '=)
        (check-eq? (datetime/offset-order t1 t2) '<)
        (check-eq? (datetime/offset-order t2 t1) '>))

      (check-true (datetime/offset=? (datetime/offset 2000 #:offset 0)
                                     (datetime/offset 1999 12 31 19 #:offset -18000))))

    (test-case
      "[serialization]"
      (check-equal? dto (deserialize (serialize dto))))))
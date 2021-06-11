#lang racket/base


(require rackunit
         racket/match
         racket/serialize
         datetime/date
         datetime/exn
         datetime/time
         datetime/datetime
         datetime/datetime/offset
         datetime/datetime/tz
         datetime/offset-resolver
         datetime/zone)

(module+ test
  (require rackunit/text-ui)
  (run-tests datetime/tz-tests))

(define dtz (datetime/tz 2001 2 15 13 20 40 12 #:tz "America/New_York"))

(define datetime/tz-tests
  (test-suite
    "datetime/tz functions"

    (test-case
      "datetime/tz"
      (match (datetime/tz 2000 #:tz "America/New_York")
        [(datetime/tz y mo d h mi s n off tz)
         (check-equal? (list y mo d h mi s n off tz)
                       (list 2000 1 1 0 0 0 0 -18000 "America/New_York"))])

      (match dtz
        [(datetime/tz d t o tz)
         (check-equal? (list d t o tz)
                       (list (date 2001 2 15) (time 13 20 40 12) -18000 "America/New_York"))])

      (match dtz
        [(datetime/tz dt o tz)
         (check-equal? (list dt o tz)
                       (list (datetime 2001 2 15 13 20 40 12) -18000 "America/New_York"))])

      (match dtz
        [(datetime/tz dto tz)
         (check-equal? (list dto tz)
                       (list (datetime/offset 2001 2 15 13 20 40 12 #:offset -18000)
                             "America/New_York"))]))

    (test-case
      "datetime/tz?"
      (check-true (datetime/tz? dtz))
      (check-false (datetime/tz? '(foo))))

    (test-case
      "datetime/tz->string"
      (check-equal? (datetime/tz->string dtz)
                    "2001-02-15T13:20:40.000000012-05:00[America/New_York]"))

    (test-case
      "datetime/tz-at-tz"
      (check-equal? (datetime/tz-at-tz dtz "UTC")
                    (datetime/tz 2001 2 15 18 20 40 12 #:tz "UTC"))
      (check-equal? (datetime/tz-at-tz dtz) dtz)
      (check-equal? (datetime/tz-at-tz dtz "America/New_York") dtz))

    (test-case
      "datetime/tz->datetime/offset"
      (check-equal? (datetime/tz->datetime/offset dtz)
                    (datetime/offset 2001 2 15 13 20 40 12 #:offset -18000))
      (check-equal? (datetime/tz->datetime/offset dtz 0)
                    (datetime/offset 2001 2 15 18 20 40 12 #:offset 0)))

    (test-case
      "datetime/tz->date"
      (check-equal? (datetime/tz->date dtz) (date 2001 2 15)))

    (test-case
      "datetime/tz->time"
      (check-equal? (datetime/tz->time dtz) (time 13 20 40 12)))

    (test-case
      "datetime/tz->local-datetime"
      (check-equal? (datetime/tz->local-datetime dtz)
                    (datetime 2001 2 15 13 20 40 12)))

    (test-case
      "datetime/tz->utc-datetime"
      (check-equal? (datetime/tz->utc-datetime dtz)
                    (datetime 2001 2 15 18 20 40 12)))

    (test-case
      "datetime/tz->tz"
      (let ([tz (datetime/tz->tz dtz)])
        (check-equal? tz "America/New_York")
        (check-true (immutable? tz))))

    (test-case
      "datetime/tz->posix"
      (check-equal? (datetime/tz->posix (datetime/tz 1970 #:tz "UTC")) 0)
      (check-equal? (datetime/tz->posix (datetime/tz 1970 #:tz "America/New_York")) 18000))

    (test-case
      "datetime/tz->jd"
      (check-equal? (datetime/tz->jd (datetime/tz 2014 12 8 7 #:tz "America/New_York")) 2457000))

    (test-case
      "posix->datetime/tz"
      (check-equal? (posix->datetime/tz 0 #:tz "America/New_York")
                    (datetime/tz 1969 12 31 19 #:tz "America/New_York"))

      (parameterize ([current-time-zone "Europe/Moscow"])
        (check-equal? (posix->datetime/tz 0)
                      (datetime/tz 1970 1 1 3 #:tz "Europe/Moscow"))))

    (test-case
      "jd->datetime/tz"
      (check-equal? (jd->datetime/tz -1 #:tz "UTC")
                    (datetime/tz -4713 11 23 12 #:tz "UTC")))

    (test-case
      "datetime->datetime/tz"
      (parameterize ([current-time-zone "Europe/London"])
        (check-equal? (datetime->datetime/tz (datetime 2000))
                      (datetime/tz 2000 #:tz "Europe/London"))
        (check-equal? (datetime->datetime/tz (datetime 2000) #:tz "America/New_York")
                      (datetime/tz 2000 #:tz "America/New_York"))
        (check-exn
         exn:datetime:invalid-offset?
         (λ ()
           (datetime->datetime/tz (datetime 2015 3 8 2) #:tz "America/New_York")))
        (check-equal? (datetime->datetime/tz (datetime 2015 3 8 2)
                                             #:tz "America/New_York"
                                             #:offset-resolver resolve/retain)
                      (datetime/tz 2015 3 8 3 #:tz "America/New_York"))))

    (test-case
      "datetime/offset->datetime/tz"
      (parameterize ([current-time-zone "Europe/Moscow"])
        (check-equal? (datetime/offset->datetime/tz (datetime/offset 2000 #:offset 10800))
                      (datetime/tz 2000 #:tz "Europe/Moscow"))
        (check-equal? (datetime/offset->datetime/tz (datetime/offset 2000 #:offset -18000))
                      (datetime/tz 2000 1 1 8 #:tz "Europe/Moscow"))
        (check-equal? (datetime/offset->datetime/tz (datetime/offset 2000 #:offset -18000)
                                                    #:tz "UTC")
                      (datetime/tz 2000 1 1 5 #:tz "UTC"))))

    (test-case
      "datetime/tz-add-months"
      (check-equal? (datetime/tz-add-months dtz -30)
                    (datetime/tz 1998 8 15 13 20 40 12 #:tz "America/New_York"))
      
      ;; default offset resolution in a gap
      (check-equal? (datetime/tz-add-months (datetime/tz 2015 2 8 2 30 #:tz "America/New_York") 1)
                    (datetime/tz 2015 3 8 3 30 #:tz "America/New_York"))
      
      ;; explicit offset resolution in a gap
      (check-exn
       exn:datetime:invalid-offset?
       (λ ()
         (datetime/tz-add-months (datetime/tz 2015 4 8 2 #:tz "America/New_York")
                                 -1
                                 #:offset-resolver resolve/raise)))

      ;; default offset resolution in an overlap
      (let ([result (datetime/tz-add-months (datetime/tz 2014 10 2 1 30 #:tz "America/New_York") 1)])
        (check-equal? result
                      (datetime/tz 2014 11 2 1 30
                                   #:tz "America/New_York"
                                   #:offset-resolver resolve/retain))
        (check-equal? (datetime/tz->utc-offset result) -14400))

      ;; parameterized offset resolution in an overlap
      (check-exn
       exn:datetime:invalid-offset?
       (λ ()
         (parameterize ([current-date-arithmetic-offset-resolver resolve/raise])
           (datetime/tz-add-months (datetime/tz 2014 10 2 1 30 #:tz "America/New_York") 1)))))

    (test-case
      "datetime/tz-add-days"
      (check-equal? (datetime/tz-add-days dtz 15)
                    (datetime/tz 2001 3 2 13 20 40 12 #:tz "America/New_York"))
      
      ;; default offset resolution in a gap
      (check-equal? (datetime/tz-add-days (datetime/tz 2015 3 7 2 30 #:tz "America/New_York") 1)
                    (datetime/tz 2015 3 8 3 30 #:tz "America/New_York"))
      
      ;; explicit offset resolution in a gap
      (check-exn
       exn:datetime:invalid-offset?
       (λ ()
         (datetime/tz-add-days (datetime/tz 2015 3 9 2 #:tz "America/New_York")
                               -1
                               #:offset-resolver resolve/raise)))

      ;; default offset resolution in an overlap
      (let ([result (datetime/tz-add-days (datetime/tz 2014 11 1 1 30 #:tz "America/New_York") 1)])
        (check-equal? result
                      (datetime/tz 2014 11 2 1 30
                                   #:tz "America/New_York"
                                   #:offset-resolver resolve/retain))
        (check-equal? (datetime/tz->utc-offset result) -14400))

      ;; parameterized offset resolution in an overlap
      (check-exn
       exn:datetime:invalid-offset?
       (λ ()
         (parameterize ([current-date-arithmetic-offset-resolver resolve/raise])
           (datetime/tz-add-days (datetime/tz 2014 11 1 1 30 #:tz "America/New_York") 1)))))

    (test-case
      "datetime/tz-add-nanoseconds"
      (check-equal? (datetime/tz-add-nanoseconds dtz (* -1000000000 60))
                    (datetime/tz 2001 2 15 13 19 40 12 #:tz "America/New_York"))

      ;; skipping a gap
      (check-equal? (datetime/tz-add-nanoseconds
                     (datetime/tz 2014 3 9 1 59 59 999999999 #:tz "America/New_York")
                     1)
                    (datetime/tz 2014 3 9 3 #:tz "America/New_York"))

      ;; skipping a gap in reverse
      (check-equal? (datetime/tz-add-nanoseconds
                     (datetime/tz 2014 3 9 3 #:tz "America/New_York")
                     -1)
                    (datetime/tz 2014 3 9 1 59 59 999999999 #:tz "America/New_York"))

      ;; entering an overlap
      (let* ([ns/h (* 1000000000 60 60)]
             [t1 (datetime/tz-add-nanoseconds (datetime/tz 2014 11 2 #:tz "America/New_York")
                                              ns/h)]
             [t2 (datetime/tz-add-nanoseconds t1 ns/h)])
        (check-equal? t1
                      (datetime/offset->datetime/tz (datetime/offset 2014 11 2 1 #:offset -14400)
                                                    #:tz "America/New_York"))
        (check-equal? t2
                      (datetime/offset->datetime/tz (datetime/offset 2014 11 2 1 #:offset -18000)
                                                    #:tz "America/New_York"))
        (check-not-equal? t1 t2))

      ;; entering an overlap in reverse
      (let* ([ns/h (* 1000000000 60 60)]
             [t2 (datetime/tz-add-nanoseconds (datetime/tz 2014 11 2 2 #:tz "America/New_York")
                                              (- ns/h))]
             [t1 (datetime/tz-add-nanoseconds t2 (- ns/h))])
        (check-equal? t2
                      (datetime/offset->datetime/tz (datetime/offset 2014 11 2 1 #:offset -18000)
                                                    #:tz "America/New_York"))
        (check-equal? t1
                      (datetime/offset->datetime/tz (datetime/offset 2014 11 2 1 #:offset -14400)
                                                    #:tz "America/New_York"))
        (check-not-equal? t2 t1)))

    (test-case
      "datetime/tz-months-between"
      (check-equal? (datetime/tz-months-between (datetime/tz 2000 #:tz "UTC")
                                                (datetime/tz 2000 4 #:tz "America/New_York"))
                    3)
      (check-equal? (datetime/tz-months-between (datetime/tz 2000 4 #:tz "UTC")
                                                (datetime/tz 2000 #:tz "UTC"))
                    -3)
      (check-equal? (datetime/tz-months-between (datetime/tz 2000 4 #:tz "America/New_York")
                                                (datetime/tz 2000 #:tz "UTC"))
                    -3))

    (test-case
      "datetime/tz-days-between"
      (check-equal? (datetime/tz-days-between (datetime/tz 2000 3 1 #:tz "Etc/UTC")
                                              (datetime/tz 2000 2 28 #:tz "Etc/UTC"))
                    -2)
      ;; https://github.com/97jaz/gregor/issues/2
      (check-equal? (datetime/tz-days-between (datetime/tz 2015 3 29 #:tz "Europe/Berlin")
                                              (datetime/tz 2015 3 30 #:tz "Europe/Berlin"))
                    1))

    (test-case
      "datetime/tz-nanoseconds-between"
      (check-equal? (datetime/tz-nanoseconds-between (datetime/tz 2000 #:tz "America/New_York")
                                                     (datetime/tz 2000 #:tz "Etc/UTC"))
                    -18000000000000))

    (test-case
      "[order]"
      (let* ([t1 (datetime/tz -1000 #:tz "UTC")]
             [t2 (datetime/tz 0 #:tz "UTC")]
             [t3 (datetime/tz 1000 #:tz "UTC")])
        (check-true (datetime/tz<? t1 t2))
        (check-true (datetime/tz<? t2 t3))
        (check-true (datetime/tz<=? t1 t3))
        (check-true (datetime/tz>? t2 t1))
        (check-true (datetime/tz>=? t3 t2))
        (check-false (datetime/tz=? t1 t3))
        
        (check-eq? (datetime/tz-order t1 t1) '=)
        (check-eq? (datetime/tz-order t1 t2) '<)
        (check-eq? (datetime/tz-order t2 t1) '>))

      (check-true (datetime/tz=? (datetime/tz 2000 #:tz "UTC")
                                 (datetime/tz 1999 12 31 19 #:tz "America/New_York"))))

    (test-case
      "[serialization]"
      (check-equal? dtz (deserialize (serialize dtz))))))


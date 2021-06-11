#lang racket/base

(require rackunit
         racket/match
         racket/serialize
         datetime/time)

(module+ test
  (require rackunit/text-ui)
  (run-tests time-tests))

(define the-time (time 2 27 43 123456789))

(define time-tests
  (test-suite
    "time functions"

    (test-case
      "time"
      (match (time 12)
        [(time h m s n) (check-equal? (list h m s n) (list 12 0 0 0))]
        [_ (fail "time match-expander")])

      (match (time 12 30)
        [(time h m s n) (check-equal? (list h m s n) (list 12 30 0 0))]
        [_ (fail "time match-expander")])

      (match (time 12 30 28)
        [(time h m s n) (check-equal? (list h m s n) (list 12 30 28 0))]
        [_ (fail "time match-expander")])

      (match (time 12 30 28 123456789)
        [(time h m s n) (check-equal? (list h m s n) (list 12 30 28 123456789))]
        [_ (fail "time match-expander")])

      (for ([fn (in-list (list (λ () (time -1))
                               (λ () (time 24))
                               (λ () (time 0 -10))
                               (λ () (time 0 60))
                               (λ () (time 0 30 -1))
                               (λ () (time 0 30 60))
                               (λ () (time 0 30 30 -1))
                               (λ () (time 0 30 30 1000000000))))])
        (check-exn exn:fail:contract? fn)))

    (test-case
      "time?"
      (check-false (time? 3))
      (check-true (time? the-time)))

    (test-case
      "time->hours"
      (check-eqv? (time->hours the-time) 2))

    (test-case
      "time->minutes"
      (check-eqv? (time->minutes the-time) 27))

    (test-case
      "time->seconds"
      (check-eqv? (time->seconds the-time) 43))

    (test-case
      "time->milliseconds"
      (check-eqv? (time->milliseconds the-time) 123))

    (test-case
      "time->microseconds"
      (check-eqv? (time->microseconds the-time) 123456))

    (test-case
      "time->nanoseconds"
      (check-eqv? (time->nanoseconds the-time) 123456789))

    (test-case
      "time->iso8601"
      (check-equal? (time->iso8601 the-time) "02:27:43.123456789"))

    (test-case
      "time=?"
      (check-true (time=? the-time (time 2 27 43 123456789)))
      (check-false (time=? the-time (time 2))))

    (test-case
      "time<?"
      (check-true (time<? the-time (time 2 27 44)))
      (check-false (time<? the-time the-time))
      (check-false (time<? the-time (time 1))))

    (test-case
      "time>?"
      (check-true (time>? the-time (time 1)))
      (check-false (time>? the-time the-time))
      (check-false (time>? the-time (time 2 27 44))))

    (test-case
      "time<=?"
      (check-true (time<=? the-time (time 2 27 44)))
      (check-true (time<=? the-time the-time))
      (check-false (time<=? the-time (time 1))))

    (test-case
      "time>=?"
      (check-true (time>=? the-time (time 1)))
      (check-true (time>=? the-time the-time))
      (check-false (time>=? the-time (time 2 27 44))))

    (test-case
      "time->day-ns"
      (check-eqv? (time->day-ns (time 0)) 0)
      (check-eqv? (time->day-ns (time 23 59 59 999999999)) 86399999999999))

    (test-case
      "day-ns->time"
      (check-equal? (day-ns->time 0) (time 0))
      (check-equal? (day-ns->time 86399999999999) (time 23 59 59 999999999)))

    (test-case
      "time-add-nanoseconds"
      (check-equal? (time-add-nanoseconds the-time 1) (time 2 27 43 123456790))
      (check-equal? (time-add-nanoseconds the-time -86400000000000) the-time))

    (test-case
      "time-nanoseconds-between"
      (check-eqv? (time-nanoseconds-between the-time the-time) 0)
      (check-eqv? (time-nanoseconds-between
                   the-time
                   (time-add-nanoseconds the-time -45000))
                  -45000))

    (test-case
      "MIDNIGHT"
      (check-equal? MIDNIGHT (time 0)))

    (test-case
      "NOON"
      (check-equal? NOON (time 12)))

    (test-case
      "[serialization]"
      (check-equal? the-time (deserialize (serialize the-time))))))

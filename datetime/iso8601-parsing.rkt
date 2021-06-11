#lang racket/base

(require rackunit
         datetime/clock
         datetime/date
         datetime/datetime
         datetime/datetime/offset
         datetime/datetime/tz
         datetime/exn
         datetime/iso8601-parsing
         datetime/time
         datetime/zone)

(module+ test
  (require rackunit/text-ui)
  (run-tests iso8601-parsing-tests))

(define iso8601-parsing-tests
  (test-suite
    "ISO8601 parsing"

    (parameterize ([current-clock (λ () 0)]
                   [current-time-zone "America/New_York"])

      (test-case "iso8601->date"
        (check-equal? (iso8601->date "2017") (date 2017))
        (check-equal? (iso8601->date "2017-10") (date 2017 10))
        (check-equal? (iso8601->date "2017-10-03") (date 2017 10 3))
        (check-equal? (iso8601->date "2017-10-03T13") (date 2017 10 3))
        (check-equal? (iso8601->date "2017-10-03T13:20") (date 2017 10 3))
        (check-equal? (iso8601->date "2017-10-03T13:20:45") (date 2017 10 3))
        (check-equal? (iso8601->date "2017-10-03T13:20:45Z") (date 2017 10 3))
        (check-equal? (iso8601->date "2017-10-03T13:20:45.934Z") (date 2017 10 3))
        (check-equal? (iso8601->date "2017-10-03T13:20:45.934+13:00") (date 2017 10 3))
        (check-equal? (iso8601->date "2017-10-03T13:20:45.934+13:00[Europe/Paris]") (date 2017 10 3))
        (check-exn exn:datetime:parse? (λ () (iso8601->date "12:00:00"))))

      (test-case "iso8601->datetime"
        (check-exn exn:datetime:parse? (λ () (iso8601->datetime "2017")))
        (check-exn exn:datetime:parse? (λ () (iso8601->datetime "2017-10")))
        (check-exn exn:datetime:parse? (λ () (iso8601->datetime "2017-10-03")))
        (check-equal? (iso8601->datetime "2017-10-03T13") (datetime 2017 10 3 13))
        (check-equal? (iso8601->datetime "2017-10-03T13:20") (datetime 2017 10 3 13 20))
        (check-equal? (iso8601->datetime "2017-10-03T13:20:45") (datetime 2017 10 3 13 20 45))
        (check-equal? (iso8601->datetime "2017-10-03T13:20:45Z") (datetime 2017 10 3 13 20 45))
        (check-equal? (iso8601->datetime "2017-10-03T13:20:45.934Z") (datetime 2017 10 3 13 20 45 934000000))
        (check-equal? (iso8601->datetime "2017-10-03T13:20:45.934+13:00") (datetime 2017 10 3 13 20 45 934000000))
        (check-equal? (iso8601->datetime "2017-10-03T13:20:45.934+13:00[Europe/Paris]") (datetime 2017 10 3 2 20 45 934000000))
        (check-exn exn:datetime:parse? (λ () (iso8601->datetime "12:00:00"))))

      (test-case "iso8601->datetime/offset"
        (check-exn exn:datetime:parse? (λ () (iso8601->datetime/offset "2017")))
        (check-exn exn:datetime:parse? (λ () (iso8601->datetime/offset "2017-10")))
        (check-exn exn:datetime:parse? (λ () (iso8601->datetime/offset "2017-10-03")))
        (check-exn exn:datetime:parse? (λ () (iso8601->datetime/offset "2017-10-03T13")))
        (check-exn exn:datetime:parse? (λ () (iso8601->datetime/offset "2017-10-03T13:20")))
        (check-exn exn:datetime:parse? (λ () (iso8601->datetime/offset "2017-10-03T13:20:45")))
        (check-equal? (iso8601->datetime/offset "2017-10-03T13:20:45Z")
                      (datetime/offset 2017 10 3 13 20 45 #:offset 0))
        (check-equal? (iso8601->datetime/offset "2017-10-03T13:20:45.934Z")
                      (datetime/offset 2017 10 3 13 20 45 934000000 #:offset 0))
        (check-equal? (iso8601->datetime/offset "2017-10-03T13:20:45.934+13:00")
                      (datetime/offset 2017 10 3 13 20 45 934000000 #:offset 46800))
        (check-equal? (iso8601->datetime/offset "2017-10-03T13:20:45.934+13:00[Europe/Paris]")
                      (datetime/offset 2017 10 3 2 20 45 934000000 #:offset 7200))
        (check-exn exn:datetime:parse? (λ () (iso8601->datetime/offset "12:00:00"))))

      (test-case "iso8601->datetime/tz"
        (check-exn exn:datetime:parse? (λ () (iso8601/tz->datetime/tz "2017")))
        (check-exn exn:datetime:parse? (λ () (iso8601/tz->datetime/tz "2017-10")))
        (check-exn exn:datetime:parse? (λ () (iso8601/tz->datetime/tz "2017-10-03")))
        (check-exn exn:datetime:parse? (λ () (iso8601/tz->datetime/tz "2017-10-03T13")))
        (check-exn exn:datetime:parse? (λ () (iso8601/tz->datetime/tz "2017-10-03T13:20")))
        (check-exn exn:datetime:parse? (λ () (iso8601/tz->datetime/tz "2017-10-03T13:20:45")))
        (check-exn exn:datetime:parse? (λ () (iso8601/tz->datetime/tz "2017-10-03T13:20:45Z")))
        (check-exn exn:datetime:parse? (λ () (iso8601/tz->datetime/tz "2017-10-03T13:20:45.934Z")))
        (check-exn exn:datetime:parse? (λ () (iso8601/tz->datetime/tz "2017-10-03T13:20:45.934+13:00")))
        (check-equal? (iso8601/tz->datetime/tz "2017-10-03T13:20:45.934+02:00[Europe/Paris]")
                      (datetime/tz 2017 10 3 13 20 45 934000000 #:tz "Europe/Paris"))
        (check-exn exn:datetime:parse? (λ () (iso8601/tz->datetime/tz "12:00:00")))))))
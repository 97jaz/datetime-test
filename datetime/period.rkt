#lang racket/base

(require rackunit
         datetime/period)

(module+ test
  (require rackunit/text-ui)
  (run-tests period-tests))

(define period-tests
  (test-suite "Period"

    (test-case "period?"
      (check-true (period? (years 5)))
      (check-false (period? 2)))

    (test-case "period-empty?"
      (check-true (period-empty? empty-period))
      (check-true (period-empty? (period)))
      (check-false (period-empty? (years 5))))

    (test-case "date-period?"
      (check-true (date-period? (years 7)))
      (check-true (date-period? (period [days 9])))
      (check-false (date-period? (seconds 2))))

    (test-case "time-period?"
      (check-true (time-period? (period [hours 8] [minutes 10])))
      (check-false (time-period? (years 8))))

    (test-case "period-negate"
      (check-equal? (period-negate (period [years 7] [minutes -2] [milliseconds 0]))
                    (period [years -7] [minutes 2])))

    (test-case "period-scale"
      (check-equal? (period-scale (period [years 7] [minutes -2] [milliseconds 0]) -3)
                    (period [years -21] [minutes 6])))

    (test-case "period->date-period"
      (check-equal? (period->date-period (period [years 7] [minutes -2] [milliseconds 0]))
                    (years 7)))

    (test-case "period->time-period"
      (check-equal? (period->time-period (period [years 7] [minutes -2] [milliseconds 0]))
                    (period [minutes -2])))

    (test-case "time-period->nanoseconds"
      (check-equal? (time-period->nanoseconds (period [seconds -8] [microseconds 987]))
                    (+ (* -8 1000000000)
                       (* 987 1000))))

    (test-case "empty-period"
      (check-equal? empty-period (period)))

    (test-case "period-ref"
      (check-equal? (period-ref (period [years 9] [seconds 20]) 'seconds) 20)
      (check-equal? (period-ref (years 20) 'seconds) 0))

    (test-case "period-set"
      (check-equal? (period-set (period [years 9] [seconds 20]) 'seconds 9)
                    (period [years 9] [seconds 9]))
      (check-equal? (period-set (period [years 9] [seconds 20]) 'days 9)
                    (period [years 9] [days 9] [seconds 20])))

    (test-case "period-add-years"
      (check-equal? (period-add-years (years 10) -7) (years 3)))

    (test-case "period-add-months"
      (check-equal? (period-add-months (years 10) -7) (period [years 10] [months -7])))

    (test-case "period-add-weeks"
      (check-equal? (period-add-weeks (years 10) -7) (period [years 10] [weeks -7])))

    (test-case "period-add-days"
      (check-equal? (period-add-days (years 10) -7) (period [years 10] [days -7])))

    (test-case "period-add-date-period"
      (check-equal? (period-add-date-period (years 10) (days -7))
                    (period [years 10] [days -7])))

    (test-case "period-add-hours"
      (check-equal? (period-add-hours (years 10) -7) (period [years 10] [hours -7])))

    (test-case "period-add-minutes"
      (check-equal? (period-add-minutes (years 10) -7) (period [years 10] [minutes -7])))

    (test-case "period-add-seconds"
      (check-equal? (period-add-seconds (years 10) -7) (period [years 10] [seconds -7])))

    (test-case "period-add-milliseconds"
      (check-equal? (period-add-milliseconds (years 10) -7)
                    (period [years 10] [milliseconds -7])))

    (test-case "period-add-microseconds"
      (check-equal? (period-add-microseconds (years 10) -7)
                    (period [years 10] [microseconds -7])))

    (test-case "period-add-nanoseconds"
      (check-equal? (period-add-nanoseconds (years 10) -7)
                    (period [years 10] [nanoseconds -7])))

    (test-case "period-add-time-period"
      (check-equal? (period-add-time-period (years 10) (seconds -7))
                    (period [years 10] [seconds -7])))

    (test-case "time-period-normalize"
      (let ([p (period [seconds 10]
                       [milliseconds -2]
                       [microseconds 200000000]
                       [nanoseconds -150])])
        (check-equal? (time-period-normalize p '(seconds microseconds))
                      (let* ([ns (time-period->nanoseconds p)]
                             [s (quotient ns 1000000000)]
                             [ns (- ns (* s 1000000000))]
                             [μs (quotient ns 1000)])
                        (period [seconds s] [microseconds μs])))))

    (test-case "period->list"
      (check-equal? (period->list (period [years 2] [hours 20] [milliseconds -100]))
                    '((years . 2)
                      (months . 0)
                      (weeks . 0)
                      (days . 0)
                      (hours . 20)
                      (minutes . 0)
                      (seconds . 0)
                      (milliseconds . -100)
                      (microseconds . 0)
                      (nanoseconds . 0)))
      (check-equal? (period->list (period [years 2] [hours 20] [milliseconds -100])
                                  '(years milliseconds))
                    '((years . 2) (milliseconds . -100))))

    (test-case "list->period"
      (check-equal? (list->period '((years . 2) (hours . 20) (milliseconds . -100)))
                    (period [years 2] [hours 20] [milliseconds -100])))

    (test-case "years"
      (check-equal? (years 20) (period-set empty-period 'years 20)))

    (test-case "months"
      (check-equal? (months 20) (period-set empty-period 'months 20)))

    (test-case "weeks"
      (check-equal? (weeks 20) (period-set empty-period 'weeks 20)))

    (test-case "days"
      (check-equal? (days 20) (period-set empty-period 'days 20)))

    (test-case "hours"
      (check-equal? (hours 20) (period-set empty-period 'hours 20)))

    (test-case "minutes"
      (check-equal? (minutes 20) (period-set empty-period 'minutes 20)))

    (test-case "seconds"
      (check-equal? (seconds 20) (period-set empty-period 'seconds 20)))

    (test-case "milliseconds"
      (check-equal? (milliseconds 20) (period-set empty-period 'milliseconds 20)))

    (test-case "microseconds"
      (check-equal? (microseconds 20) (period-set empty-period 'microseconds 20)))

    (test-case "nanoseconds"
      (check-equal? (nanoseconds 20) (period-set empty-period 'nanoseconds 20)))))
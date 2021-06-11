#lang racket/base

(require rackunit
         datetime/date
         datetime/datetime
         datetime/datetime/offset
         datetime/datetime/tz
         datetime/generic/date-provider
         datetime/private/time/util
         (prefix-in base: (only-in racket/base date*)))

(module+ test
  (require rackunit/text-ui)
  (run-tests date-provider-tests))

(define d (date 2000))
(define dt (datetime 1900))
(define dto (datetime/offset 0 #:offset -18000))
(define dtz (datetime/tz 1970 #:tz "Etc/UTC"))
(define bd (base:date* 0 0 0 1 1 2017 0 0 #f 0 0 "whatev"))
(define bd-leap-second (base:date* 60 0 0 1 1 2017 0 0 #f 0 0 "whatev"))

(define date-provider-tests
  (test-suite
    "date provider generics"

    (test-case "date-provider?"
      (check-true (date-provider? d))
      (check-true (date-provider? dt))
      (check-true (date-provider? dto))
      (check-true (date-provider? dtz))
      (check-true (date-provider? bd))
      (check-false (date-provider? bd-leap-second)))

    (test-case "->date"
      (check-equal? (->date d) d)
      (check-equal? (->date dt) (date 1900))
      (check-equal? (->date dto) (date 0))
      (check-equal? (->date dtz) (date 1970))
      (check-equal? (->date bd) (date 2017))
      (check-exn exn:fail:contract? (λ () (->date bd-leap-second))))

    (test-case "->jdn"
      (check-equal? (->jdn (date 2017 1 22)) 2457776)
      (check-equal? (->jdn (datetime 2017 1 23)) 2457777)
      (check-equal? (->jdn (datetime/offset 2017 1 24 #:offset 0)) 2457778)
      (check-equal? (->jdn (datetime/tz 2017 1 25)) 2457779))
    
    (test-case "->year"
      (check-equal? (->year d) 2000)
      (check-equal? (->year dt) 1900)
      (check-equal? (->year dto) 0)
      (check-equal? (->year dtz) 1970)
      (check-equal? (->year bd) 2017))

    (test-case "->quarter"
      (check-equal? (->quarter d) 1)
      (check-equal? (->quarter (datetime 2000 4)) 2)
      (check-equal? (->quarter (datetime/offset 2000 7 #:offset 0)) 3)
      (check-equal? (->quarter (datetime/tz 2000 10)) 4)
      (check-equal? (->quarter bd) 1))

    (test-case "->month"
      (check-equal? (->month (date 2017)) 1)
      (check-equal? (->month (datetime 2017 2)) 2)
      (check-equal? (->month (datetime/offset 2017 3 #:offset 0)) 3)
      (check-equal? (->month (datetime/tz 2017 4 #:tz "UTC")) 4)
      (check-equal? (->month (base:date* 0 0 0 1 5 2017 0 0 #f 0 0 "whatev")) 5))

    (test-case "->day"
      (check-equal? (->day (date 2017)) 1)
      (check-equal? (->day (datetime 2017 1 2)) 2)
      (check-equal? (->day (datetime/offset 2017 1 3 #:offset 0)) 3)
      (check-equal? (->day (datetime/tz 2017 1 4)) 4)
      (check-equal? (->day (base:date* 0 0 0 5 1 2017 0 0 #f 0 0 "whatev")) 5))
    
    (test-case "->wday"
      (check-equal? (->wday (date 2017)) 0)
      (check-equal? (->wday (datetime 2017 1 2)) 1)
      (check-equal? (->wday (datetime/offset 2017 1 3 #:offset 0)) 2)
      (check-equal? (->wday (datetime/tz 2017 1 4)) 3)
      (check-equal? (->wday (base:date* 0 0 0 5 1 2017 0 0 #f 0 0 "whatev")) 4))

    (test-case "->yday"
      (check-equal? (->yday (date 2017 2)) 32)
      (check-equal? (->yday (datetime 2017 2 2)) 33)
      (check-equal? (->yday (datetime/offset 2017 2 3 #:offset 0)) 34)
      (check-equal? (->yday (datetime/tz 2017 2 4)) 35)
      (check-equal? (->yday (base:date* 0 0 0 5 2 2017 0 0 #f 0 0 "whatev")) 36))

    (test-case "->iso-week"
      (check-equal? (->iso-week (date 2005 1 1)) 53)
      (check-equal? (->iso-week (datetime 2007 1 1)) 1)
      (check-equal? (->iso-week (datetime/offset 2008 12 31 #:offset 0)) 1)
      (check-equal? (->iso-week (datetime/tz 2008 12 19)) 51)
      (check-equal? (->iso-week (base:date* 0 0 0 5 2 2017 0 0 #f 0 0 "whatev")) 5))

    (test-case "->iso-wyear"
      (check-equal? (->iso-wyear (date 2005 1 1)) 2004)
      (check-equal? (->iso-wyear (datetime 2007 1 1)) 2007)
      (check-equal? (->iso-wyear (datetime/offset 2008 12 31 #:offset 0)) 2009)
      (check-equal? (->iso-wyear (datetime/tz 2008 6 15)) 2008)
      (check-equal? (->iso-wyear (base:date* 0 0 0 5 2 2017 0 0 #f 0 0 "whatev")) 2017))

    (test-case "->iso-wday"
      (check-equal? (->iso-wday (date 2017)) 7)
      (check-equal? (->iso-wday (datetime 2017 1 2)) 1)
      (check-equal? (->iso-wday (datetime/offset 2017 1 3 #:offset 0)) 2)
      (check-equal? (->iso-wday (datetime/tz 2017 1 4)) 3)
      (check-equal? (->iso-wday (base:date* 0 0 0 5 1 2017 0 0 #f 0 0 "whatev")) 4))

    (test-case "sunday?"
      (check-true (sunday? (date 1942 5 3)))
      (check-false (sunday? (datetime 1776 7 4)))
      (check-true (sunday? (datetime/offset 2100 1 31 #:offset 0)))
      (check-false (sunday? (datetime/tz 1970)))
      (check-true (sunday? (base:date* 0 0 0 14 5 2017 0 0 #f 0 0 ""))))

    (test-case "monday?"
      (check-true (monday? (date 1942 5 4)))
      (check-false (monday? (datetime 1776 7 4)))
      (check-true (monday? (datetime/offset 2100 2 1 #:offset 0)))
      (check-false (monday? (datetime/tz 1970)))
      (check-true (monday? (base:date* 0 0 0 15 5 2017 0 0 #f 0 0 ""))))

    (test-case "tuesday?"
      (check-true (tuesday? (date 1942 5 5)))
      (check-false (tuesday? (datetime 1776 7 4)))
      (check-true (tuesday? (datetime/offset 2100 2 2 #:offset 0)))
      (check-false (tuesday? (datetime/tz 1970)))
      (check-true (tuesday? (base:date* 0 0 0 16 5 2017 0 0 #f 0 0 ""))))

    (test-case "wednesday?"
      (check-true (wednesday? (date 1942 5 6)))
      (check-false (wednesday? (datetime 1776 7 4)))
      (check-true (wednesday? (datetime/offset 2100 2 3 #:offset 0)))
      (check-false (wednesday? (datetime/tz 1970)))
      (check-true (wednesday? (base:date* 0 0 0 17 5 2017 0 0 #f 0 0 ""))))

    (test-case "thursday?"
      (check-true (thursday? (date 1942 5 7)))
      (check-true (thursday? (datetime 1776 7 4)))
      (check-true (thursday? (datetime/offset 2100 2 4 #:offset 0)))
      (check-true (thursday? (datetime/tz 1970)))
      (check-false (thursday? (base:date* 0 0 0 17 5 2017 0 0 #f 0 0 ""))))

    (test-case "friday?"
      (check-true (friday? (date 1942 5 8)))
      (check-false (friday? (datetime 1776 7 4)))
      (check-true (friday? (datetime/offset 2100 2 5 #:offset 0)))
      (check-false (friday? (datetime/tz 1970)))
      (check-true (friday? (base:date* 0 0 0 19 5 2017 0 0 #f 0 0 ""))))

    (test-case "saturday?"
      (check-true (saturday? (date 1942 5 9)))
      (check-false (saturday? (datetime 1776 7 4)))
      (check-true (saturday? (datetime/offset 2100 2 6 #:offset 0)))
      (check-false (saturday? (datetime/tz 1970)))
      (check-true (saturday? (base:date* 0 0 0 20 5 2017 0 0 #f 0 0 ""))))

    (test-case "years-between"
      (check-equal? (years-between d d) 0)
      (check-equal? (years-between d (date-add-months d 14)) 1)
      (check-equal? (years-between dt (datetime-add-months dt -14)) -1)
      (check-equal? (years-between (datetime 2000 12 31 23 59 59)
                                   (datetime 2001 1 1))
                    0)
      (check-equal? (years-between dto (datetime/offset-add-months dto -27)) -2)
      (check-equal? (years-between (datetime/offset 2000 12 31 23 59 59 #:offset 0)
                                   (datetime/offset 2001 1 1 #:offset 0))
                    0)
      (check-equal? (years-between (datetime/offset 2000 #:offset -18000)
                                   (datetime/offset 2001 1 1 4 #:offset 0))
                    0)
      (check-equal? (years-between (datetime/offset 2000 #:offset -18000)
                                   (datetime/offset 2001 1 1 5 #:offset 0))
                    1)
      (check-equal? (years-between (base:date* 0 0 0 1 1 2000 0 0 #f -18000 0 "")
                                   (base:date* 0 0 4 1 1 2001 0 0 #f 0 0 ""))
                    0)
      (check-equal? (years-between (base:date* 0 0 0 1 1 2000 0 0 #f -18000 0 "")
                                   (base:date* 0 0 5 1 1 2001 0 0 #f 0 0 ""))
                    1)
      (check-equal? (years-between dtz (datetime/tz-add-months dtz 24)) 2)
      (check-equal? (years-between (datetime/tz 2000 12 31 23 59 59)
                                   (datetime/tz 2001 1 1))
                    0)
      (check-equal? (years-between (datetime/tz 2000 #:tz "America/New_York")
                                   (datetime/tz 2001 1 1 4 #:tz "UTC"))
                    0)
      (check-equal? (years-between (datetime/tz 2000 #:tz "America/New_York")
                                   (datetime/tz 2001 1 1 5 #:tz "UTC"))
                    1)
      (check-exn exn:fail:contract? (λ () (years-between d dtz))))

    (test-case "months-between"
      (check-equal? (months-between d d) 0)
      (check-equal? (months-between d (date-add-months d 1)) 1)
      (check-equal? (months-between d (date-add-months d -2)) -2)
      (check-equal? (months-between d (date-add-days d 15)) 0)
      (check-equal? (months-between dt dt) 0)
      (check-equal? (months-between dt (datetime-add-months dt 6)) 6)
      (check-equal? (months-between dt (datetime-add-months dt -24)) -24)
      (check-equal? (months-between dt (datetime-add-days dt -15)) 0)

      (check-equal? (months-between (datetime/offset 2000 #:offset 0)
                                    (datetime/offset 2000 4 #:offset -18000))
                    3)
      (check-equal? (months-between (base:date* 0 0 0 1 1 2000 0 0 #f 0 0 "")
                                    (base:date* 0 0 0 1 4 2000 0 0 #f -18000 0 ""))
                    3)
      (check-equal? (months-between (datetime/tz 2000 #:tz "Etc/UTC")
                                    (datetime/tz 2000 4 #:tz "America/New_York"))
                    3)
      (check-exn exn:fail:contract? (λ () (months-between d dt))))

    (test-case "weeks-between"
      (check-equal? (weeks-between d (date-add-days d -14)) -2)
      (check-equal? (weeks-between (datetime 2015 3 15) (datetime 2015 1 4))
                    -10)
      (check-equal? (weeks-between (datetime/offset 2015 3 15 #:offset -14400)
                                   (datetime/offset 2015 5 29 #:offset -14400))
                    10)
      (check-equal? (weeks-between (base:date* 0 0 0 15 3 2015 0 0 #f -14400 0 "")
                                   (base:date* 0 0 0 29 5 2015 0 0 #f -14400 0 ""))
                    10)
      (check-equal? (weeks-between (datetime/tz 2015 3 15 #:tz "America/New_York")
                                   (datetime/tz 2015 5 29 #:tz "America/New_York"))
                    10)
      (check-exn exn:fail:contract? (λ () (weeks-between d dt))))

    (test-case "days-between"
      (check-equal? (days-between d d) 0)
      (check-equal? (days-between d (date-add-days d 400)) 400)
      (check-equal? (days-between dt (datetime-add-days dt -525)) -525)
      (check-equal? (days-between dt (datetime-add-nanoseconds dt (* NS/HOUR 23))) 0)
      (check-equal? (days-between dt (datetime-add-nanoseconds dt (* NS/HOUR 24))) 1)
      (check-equal? (days-between dto dto) 0)
      (check-equal? (days-between (datetime/offset 2015 3 29 #:offset 3600)
                                  (datetime/offset 2015 3 30 #:offset 7200))
                    0)
      (check-equal? (days-between (base:date* 0 0 0 29 3 2015 0 0 #f 3600 0 "")
                                  (base:date* 0 0 0 30 3 2015 0 0 #f 7200 0 ""))
                    0)
      ;; https://github.com/97jaz/gregor/issues/2
      (check-equal? (days-between (datetime/tz 2015 3 29 #:tz "Europe/Berlin")
                                  (datetime/tz 2015 3 30 #:tz "Europe/Berlin"))
                    1)
      (check-exn exn:fail:contract? (λ () (days-between d dt))))))

(import (chicken foreign)
        (only (chicken string) string-split ->string)
        (only miscmacros while)
        fmt
        (only srfi-13 string-upcase string-delete)
        (only srfi-14 char-set)
        srfi-19-date
        (only srfi-19-time make-time*)
        (only srfi-19-io date->string)
        srfi-69)



#>
#include "heating.h"
<#

(define pin_init (foreign-lambda int "pin_init" int int int))
(define pin_close (foreign-lambda void "pin_close" int))

(define dht(foreign-lambda int "dht" (c-pointer float) (c-pointer float)))
(define heat (foreign-lambda int "heat" int))

(define (*cmd-on*) (heat 1))
(define (*cmd-off*) (heat 0))

(define *temperature* 999) ;; idle until reading first valid temperature value
(define *humidity* 999)

(define *min-temp* 10)
(define *max-temp* 10.4)

(define *forever* #t)
(define *heating-needed* #f)
(define *heating-started* #f)
(define *heating-paused* #f)

(define *idle-at* #f)
(define *heating-resumed-at* #f)
(define *heating-paused-at* #f)
(define *state-at* #f)

(define *idle-duration* (make-time* #:seconds (* 10 60)))
(define *heating-duration* (make-time* #:seconds (* 5 60)))
(define *heating-pause-duration* (make-time* #:seconds (* 5 60)))
(define *state-duration* (make-time* #:seconds (* 6 60 60)))

(define (idle) (lambda () #f))
(define (heat-until-pause) (lambda () #f))
(define (wait-to-resume) (lambda () #f))

(define *curr-fn* idle)

(define *temperature-gpio-pin* 17)
(define *heating-gpio-pin* 18)

;; (let ((temperature-gpio (location (c-pointer)))
;;       (heating-gpio (location (c-pointer))))
;;   (define (gpio-init pin val))
;;   (define (gpio-close pin))
;;   (define (heating-init)
;;     (set! temperature-gpio (gpio-init *temperature-gpio-pin* 1))
;;     (set! heating-gpio (gpio-init *heating-gpio-pin* 0)))
;;   (define (heating-close)
;;     (gpio-close temperature-gpio)
;;     (gpio-close heating-gpio))
;;   (define (heating-on))
;;   (define (heating-off)))
  
(define (num/2 i)
  (if (number? i)
      (if (< i 10) (fmt #f "0" i) (fmt #f i))
      "--"))

(define (kw->string kw)
  (string-upcase (string-delete (char-set #\: #\#) (fmt #f kw))))

(define (ts-info kw)
  (let ((d (current-date)))
    (values
     (fmt #f
         (num (date-year d)) "-"
         (num/2 (date-month d)) "-"
         (num/2 (date-day d)) " "
         (num/2 (date-hour d)) ":"
         (num/2 (date-minute d)) ":"
         (num/2 (date-second d)) " "
         (if (number? *temperature*) (fix 1 *temperature*) "-.-") "*C "
         (if (number? *humidity*) (fix 1 *humidity*) "-.-") "% "
         (kw->string kw))
     d)))

(define (update-db)
  ;; (execute *db* "insert ...
  ;; database lookup:
  ;; (map-row (lambda (a b c d e) (list a b c d e))
  ;;          *db* "select * from heating limit 3;")
  #t)

(define (print-now kw)
  (update-db)
  (let-values (((s _) (ts-info kw))) (print s)))

(define (run-shell-command cmd) (print (fmt #f cmd)))

(define (fetch-temperature)
  ;;(run-shell-command "fetch-temperature")
  (let ((d (current-date)))
        (set! *temperature* (+ 10 (sin (date-minute d))))
        (set! *humidity* (+ 50 (sin (date-second d))))
         (values *temperature* *humidity*)))

(define (heating mode)
  (case mode
    ((#:on) (heat 1))
    ((#:off) (heat 0))))

(set! wait-to-resume
      (lambda ()
        (fetch-temperature)
        (cond
         ((not (number? *temperature*)) #:ignore)
         ((> *temperature* *max-temp*)
          (set! *heating-needed* #f)
	  (set! *heating-started* #f)
	  (set! *heating-paused* #f)
	  (set! *curr-fn* idle)
          (print-now #:heating-off))
         (else (let ((ts (current-date)))
                 (when (date>? ts (date-add-duration *heating-paused-at*
                                                     *heating-pause-duration*))
                   (heating #:on)
                   (print-now #:heating-resumed)
                   (set! *heating-resumed-at* ts)
                   (set! *curr-fn* heat-until-pause)))))))

(set! heat-until-pause
      (lambda ()
        (let ((ts (current-date)))
          (when (date>? ts (date-add-duration *heating-resumed-at*
                                              *heating-duration*))
            (heating #:off)
            (print-now #:heating-paused)
            (set! *heating-paused-at* ts)
            (set! *curr-fn* wait-to-resume)))))

(set! start-heating
      (lambda ()
        (heating #:on)
        (print-now #:start-heating)
        (set! *heating-started* #t)
        (set! *heating-resumed-at* (current-date))
        (set! *curr-fn* heat-until-pause)))

(set! idle
      (lambda ()
        (let ((ts (current-date)))
          (when (date>? ts (date-add-duration *idle-at*
                                              *idle-duration*))
            (set! *idle-at* ts)
            (fetch-temperature)
            (let ((tp *temperature*))
	      (cond ((and tp (< tp *min-temp*))
	             (set! *heating-needed* #t)
	             (start-heating))
	            ((and tp (> tp *max-temp*))
	             (when *heating-needed*
                       (set! *heating-needed* #f)
                       (print-now #:stop-heating))
                     (heating #:off))
	            (else (print-now #:idle))))))))

(define (function-name fn)
  (cadr (string-split (string-upcase (->string fn)) "()")))

(define (control-heating-init)
  (let ((r 0))
    (set! r (pin_init 0 17 1))
    (display r)
    ;;(if (zero? r)
    (set! r (pin_init 1 18 0))          ;)
    r)
  (heat 1)
  (let-location
   ([t float] [h float])
   ;;(while (and (> tries 0)(< r 0))
   (set! r (dht (location h) (location t)))
   ;;       (set! tries (- tries 1)))
   (if (zero? r)
       (print "t = "
              (fmt #f (num t 10 1))
              " h = "
              (fmt #f (num h 10 1)))
       (print "errno " r))))

(define (control-heating-close)
  (pin_close 0)
  (pin_close 1))

(define (control-heating)
  (control-heating-init)
  (set! *curr-fn* idle)
  (set! *idle-at* (date-subtract-duration (current-date)
                                          (make-time* #:seconds (* 12 60))))
  (fetch-temperature)
  (while *forever*
         (*curr-fn*)
         ;;(print (fmt #f (ts-info (function-name *curr-fn*))))
         (flush-output)
         (sleep 60))
  (pin_close)
  (print (fmt #f (ts-info #:stop))))

(control-heating-init)

(import (chicken foreign)
        (only (chicken string) string-split ->string)
        (only miscmacros while)
        fmt
        (only srfi-13 string-upcase string-delete)
        (only srfi-14 char-set)
        srfi-19-date
        (only srfi-19-time make-time*)
        (only srfi-19-io date->string)
        srfi-69
        nrepl srfi-18
        srfi-12
        (only posix-utils get-shell-variable)
        sqlite3)



#>
#include "heating.h"
<#


(define pin_init (foreign-lambda int "pin_init" int int int))
(define pin_close (foreign-lambda void "pin_close" int))

(define dht(foreign-lambda int "dht" (c-pointer float) (c-pointer float)))
(define heat (foreign-lambda int "heat" int))

(define *db* (open-database
              (fmt #f (get-shell-variable "HOME") "/data/heating.db")))

(define *temperature-gpio-pin* 17)
(define *heating-gpio-pin* 18)

(define (*cmd-on*) (heat 1))
(define (*cmd-off*) (heat 0))

(define *temperature* 999) ;; IDLE until first valid temperature measurement
(define *humidity* 999)

(define *min-temp* 10)
(define *max-temp* 10.4)

(define *forever* #t)
(define *heating-needed* #f)
(define *heating-started* #f)
(define *heating-paused* #f)

(define *idle-duration* (make-time* #:seconds (* 10 60)))
(define *heating-duration* (make-time* #:seconds (* 5 60)))
(define *heating-pause-duration* (make-time* #:seconds (* 5 60)))
(define *state-duration* (make-time* #:seconds (* 10 60)))

(define *idle-at* (date-subtract-duration (current-date) *idle-duration*))
(define *heating-resumed-at* #f)
(define *heating-paused-at* #f)
(define *state-at* (date-subtract-duration (current-date) *state-duration*))

(define (idle) (lambda () #f))
(define (heat-until-pause) (lambda () #f))
(define (wait-to-resume) (lambda () #f))

(define *curr-fn* idle)



(define (try-catch body handler)
  (call/cc
    (lambda (k)
      (parameterize ((current-exception-handler
                      (lambda (ex)
                        (k (handler ex)))))
        (body)))))

(define (num/2 i)
  (if (number? i)
      (if (< i 10) (fmt #f "0" i) (fmt #f i))
      "--"))

(define (kw->string kw)
  (string-upcase (string-delete (char-set #\: #\#) (fmt #f kw))))

(define (ts->string d)
  (fmt #f
       (num (date-year d)) "-"
       (num/2 (date-month d)) "-"
       (num/2 (date-day d)) " "
       (num/2 (date-hour d)) ":"
       (num/2 (date-minute d)) ":"
       (num/2 (date-second d))))

(define (ts-info kw)
  (let* ((d (current-date))
         (ts-str (ts->string d))
         (kw-str (kw->string kw)))
    (values
     (fmt #f
          ts-str " "
          (if (number? *temperature*) (fix 1 *temperature*) "-.-") "*C "
          (if (number? *humidity*) (fix 1 *humidity*) "-.-") "% "
          kw-str)
     d ts-str kw-str)))

(define (update-db ts-str kw-str)
  (let ([sql (fmt #f "insert into heating (ts,temp,hum,state) values (\""
                  ts-str "\",round(" *temperature* ",2),round(" *humidity* ",2),\""
                  kw-str "\");")])
    (execute *db* sql)))
  ;;
  ;; database lookup:
  ;; (map-row (lambda (a b c d e) (list a b c d e))
  ;;          *db* "select * from heating limit 3;")

(define (print-now kw)
  (let ((ts (current-date)))
    (when (date>? ts (date-add-duration *state-at*
                                        *state-duration*))
      (set! *state-at* ts)
      (let-values (((s _ ts-str kw-str) (ts-info kw)))
        (update-db ts-str kw-str)
        (print s)
        (flush-output)))))

(define (fetch-temperature)
  (let ([r 0])
    (let-location
     ([t float] [h float])
     (set! r (dht (location h) (location t)))
     (if (zero? r)
         (begin
           (set! *temperature* t)
           (set! *humidity* h)
           (print "t = " (fmt #f (num t 10 1))
                  " h = " (fmt #f (num h 10 1))))
         (fmt #t "DEBUG 'fetch-temperature' error " r "\n"))))
  (values *temperature* *humidity*))

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
                 (when (date>? ts
                               (date-add-duration *heating-paused-at*
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
    (if (zero? r)
        (set! r (pin_init 1 18 0)))
    r))

(define (control-heating-close)
  (pin_close 0)
  (pin_close 1))

(define (control-heating)
  (unless (> 0 (control-heating-init))
    (set! *curr-fn* idle)
    (while *forever*
           (fetch-temperature)
           (*curr-fn*)
           (print-now (function-name *curr-fn*))
           (flush-output)
           (sleep 60)))
  (control-heating-close)
  (let-values (((s _ _ _) (ts-info #:stop))) (print s)))

(define with-main-mutex
  (let ((main-mutex (make-mutex)))
    (lambda (proc)
      (dynamic-wind (lambda () (mutex-lock! main-mutex))
                    proc
                    (lambda () (mutex-unlock! main-mutex))))))

(define (run-repl)
  (thread-start!
   (lambda ()
     (nrepl 1234
            #:host "0.0.0.0"
            #:spawn (lambda ()
                      (thread-start!
                       (lambda ()
                         (nrepl-loop
                          eval: (lambda (x)
                                  (with-main-mutex
                                   (lambda () (eval x))))))))))))

(define (run-heating-repl)
  (try-catch
   (lambda ()
     (run-repl)
     (with-main-mutex (control-heating)))
   (lambda () #f)))

(control-heating)

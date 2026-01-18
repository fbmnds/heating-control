(import (only (chicken string) string-split ->string)
        (only miscmacros while)
        fmt
        (only srfi-13 string-upcase string-delete)
        (only srfi-14 char-set)
        srfi-19-date
        (only srfi-19-time make-time*)
        (only srfi-19-io date->string)
        srfi-69)


(define *heating-gpio-pin* 75)
(define *cmd-on* (fmt #f "/usr/local/bin/gpio" *heating-gpio-pin* " 1"))
(define *cmd-off* (fmt #f "/usr/local/bin/gpio" *heating-gpio-pin* " 0"))

(define *temperature* #f)
(define *humidity* #f)

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
  ;;(print "update db")
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
    ((#:on) (run-shell-command *cmd-on*))
    ((#:off) (run-shell-command *cmd-off*))))

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

(define (control-heating)
  (set! *curr-fn* idle)
  (set! *idle-at* (date-subtract-duration (current-date)
                                          (make-time* #:seconds (* 12 60))))
  (fetch-temperature)
  (while *forever*
         (*curr-fn*)
         ;;(print (fmt #f (ts-info (function-name *curr-fn*))))
         (flush-output)
         (sleep 60))
  (print (fmt #f (ts-info #:stop))))

(control-heating)

;;#!/bin/sh
#|
exec sbcl --script "$0" "$@"
|#


;;#-quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;(ql:quickload :dexador)
(ql:quickload :uiop)
(ql:quickload :parse-float)
(ql:quickload :cl-json)
(ql:quickload :bt-semaphore)
(ql:quickload :local-time)
(ql:quickload :sqlite)
(ql:quickload :slynk)

(use-package '(:parse-float :sqlite))

(defmacro str+ (&rest rest) `(concatenate 'string ,@rest))

(defparameter *path* (str+ (uiop:getenv :home) "/projects/heating-control/"))

(defvar *cmd* (make-hash-table))
(setf (gethash :temperature *cmd*)
      (list "/usr/bin/python3" (str+ *path* "temperature.py")))
(setf (gethash :toggle *cmd*)
      '("/usr/bin/curl" "http://192.168.178.70/r1"))
(setf (gethash :state *cmd*)
      '("/usr/bin/curl" "http://192.168.178.70/?"))

(defparameter *min-temp* 10)
(defparameter *max-temp* 11.2)

(defparameter *forever* t)
(defparameter *heating-needed* nil)
(defparameter *heating-started* nil)
(defparameter *heating-paused* nil)

(defparameter *idle-at* nil)
(defparameter *heating-resumed-at* nil)
(defparameter *heating-paused-at* nil)

(defparameter *idle-duration* (* 10 60))
(defparameter *heating-duration* (* 5 60))
(defparameter *heating-pause-duration* (* 5 60))

(defvar *curr-fn*)

(defparameter *temperature* nil)
(defparameter *humidity* nil)

(defparameter *db*
  (connect (merge-pathnames #p"data/heating.db" *path*)))
(defvar *db-create* '(execute-non-query *db*
		      "create table heating (id integer primary key, 
                                          ts text not null, 
                                          temp float null,
                                          hum float null
                                          state text null)"))

;;(defparameter *slynk-port* 4006)

;;(slynk:create-server :port *slynk-port*  :dont-close t)
(setf slynk:*use-dedicated-output-stream* nil) 

(defun print-now (kw)
  (let* ((ts (format nil "~a"
		     (local-time:format-timestring
		      nil (local-time:now)
		      :format '(:year "-" (:month 2) "-" (:day 2) " "
				(:hour 2) ":" (:min 2) ":" (:sec 2)))))
	 (ts-info (format nil
			  "~%~a ~a*C ~a% ~a" ts *temperature* *humidity* kw)))
    (ignore-errors
     (execute-non-query
      *db* (concatenate 'string "insert into heating (ts,temp,hum,state)"
                                " values (?,round(?,2),round(?,2),?)")
      ts *temperature* *humidity* (format nil "~a" kw)))
    (princ ts-info)))

(defun round-2 (x) (when (numberp x) (float (/ (round (* 100 x)) 100))))

(defun fetch-temperature ()
  (let ((th (ignore-errors
	     (uiop:split-string (uiop:run-program (gethash :temperature *cmd*)
						  :output '(:string :stripped t))
				:separator " "))))
    (setf *temperature* (round-2 (parse-float (car th) :junk-allowed t)))
    (setf *humidity* (round-2 (parse-float (cadr th) :junk-allowed t))))
  (values *temperature* *humidity*))

(defun heating-op (op)
  (flet ((cmd () (uiop:run-program
		  (gethash op *cmd*)
		  :output '(:string :stripped t))))
    (ignore-errors
     (cdr (assoc :r-1
		 (with-input-from-string (s (cmd))
		   (json:decode-json s)))))))

(defun heating (mode)
  (let ((state (heating-op :state)))
    (case mode
      (:on (when (and state (= 0 state))
	     (setf state (heating-op :toggle))))
      (:off (when (and state (= 1 state))
	      (setf state (heating-op :toggle))))
      (otherwise state))
    (case state
      (0 :off)
      (1 :on)
      (otherwise nil))))

;; *heating-needed* *heating-started* *heating-paused*
;;         T                T                 T         wait-to-resume
;;         T                T                NIL        heat-until-pause
;;         T               NIL                .         start-heating
;;        NIL               T                 .         stop-heating
;;        NIL              NIL                .         idle

(defun wait-to-resume ()
  ;;(print :wait-to-resume)
  (let ((temperature (fetch-temperature)))
    (cond ((not temperature) :ignore) 
	  ((> temperature *max-temp*)
	   (setf *heating-needed* nil)
	   (setf *heating-started* nil)
	   (setf *heating-paused* nil)
	   (setf *curr-fn* #'idle))
	  (t (let ((ts (get-universal-time)))
	       (when (> ts (+ *heating-paused-at* *heating-pause-duration*))
		 (heating :on)
		 (print-now :heating-on)
		 (setf *heating-resumed-at* ts)
		 (setf *curr-fn* #'heat-until-pause)))))))

(defun heat-until-pause ()
  (let ((ts (get-universal-time)))
    (when (> ts (+ *heating-resumed-at* *heating-duration*))
      (heating :off)
      (print-now :heating-off)
      (setf *heating-paused-at* ts)
      (setf *curr-fn* #'wait-to-resume))))

(defun start-heating ()
  (heating :on)
  (print-now :start-heating)
  (setf *heating-started* t)
  (setf *heating-resumed-at* (get-universal-time))
  (setf *curr-fn* #'heat-until-pause))

(defun idle ()
  (let ((ts (get-universal-time)))
    (when (> ts (+ *idle-at* *idle-duration*))
      (setf *idle-at* ts)
      (let ((tp (fetch-temperature)))
	(cond ((and tp (< tp *min-temp*))
	       (setf *heating-needed* t)
	       (setf *curr-fn* #'start-heating))
	      ((and tp (> tp *max-temp*))
	       (when *heating-needed*
                  (setf *heating-needed* nil)
                  (print-now :stop-heating))
               (heating :off))
	      (t nil))
      (print-now :idle)))))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun control-heating ()
  (setf *curr-fn* #'idle)
  (setf *idle-at* (- (get-universal-time) (* 20 60)))
  (fetch-temperature)
  (while *forever* (funcall *curr-fn*) (sleep 60)))

(defun run-control-heating ()
  (bt:make-thread #'control-heating))

(run-control-heating)





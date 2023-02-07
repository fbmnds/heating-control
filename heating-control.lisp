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

(ql:quickload :dexador)
(ql:quickload :uiop)
(ql:quickload :parse-float)
(ql:quickload :cl-json)
(ql:quickload :bt-semaphore)
(ql:quickload :local-time)
(ql:quickload :sqlite)
(ql:quickload :named-readtables)
(ql:quickload :slynk)

(use-package '(:parse-float :sqlite))

(defparameter *control-heating-thread* nil)

(defmacro str+ (&rest rest) `(concatenate 'string ,@rest))

<<<<<<< HEAD
(defparameter *path* (str+ (uiop:getenv :home) "/projects/heating-control/"))
(load (str+ *path* "secrets/secrets.lisp"))

=======
(defparameter *path* (str+ (uiop:getenv "HOME") "/projects/heating-control/"))
(load (str+ *path* "secrets/secrets.lisp"))
#| Pi Zero Parameter
>>>>>>> bdf89d7 (Backup of a64)
(defparameter *heating-gpio-pin* 21)
(defparameter *cmd-on*
  (format nil
          "/usr/bin/echo 1 >/sys/class/gpio/gpio~a/value" *heating-gpio-pin*))
(defparameter *cmd-off*
  (format nil
          "/usr/bin/echo 0 >/sys/class/gpio/gpio~a/value" *heating-gpio-pin*))
(defparameter *cmd-state*
  (format nil "/usr/bin/cat /sys/class/gpio/gpio~a/value" *heating-gpio-pin*))

(defparameter *cmd-th* (list "/usr/bin/python3" (str+ *path* "temperature.py")))
<<<<<<< HEAD
=======
|#

(defparameter *heating-gpio-pin* 75)
(defparameter *cmd-on* (format nil "/usr/local/bin/gpio~a 1" *heating-gpio-pin*))
(defparameter *cmd-off* (format nil "/usr/local/bin/gpio~a 0" *heating-gpio-pin*))
(defparameter *cmd-state*
  (format nil "/usr/bin/cat /sys/class/gpio/gpio~a/value" *heating-gpio-pin*))
(defparameter *cmd-th* (list "/usr/local/bin/dht22" ""))

>>>>>>> bdf89d7 (Backup of a64)

(defparameter *min-temp* 10)
(defparameter *max-temp* 11.2)

(defparameter *forever* t)
(defparameter *heating-needed* nil)
(defparameter *heating-started* nil)
(defparameter *heating-paused* nil)

(defparameter *idle-at* nil)
(defparameter *heating-resumed-at* nil)
(defparameter *heating-paused-at* nil)
(defparameter *state-at* nil)

(defparameter *idle-duration* (* 10 60))
(defparameter *heating-duration* (* 5 60))
(defparameter *heating-pause-duration* (* 5 60))
<<<<<<< HEAD
(defparameter *state-duration* (* 12 60 60))
=======
(defparameter *state-duration* (* 6 60 60))
>>>>>>> bdf89d7 (Backup of a64)

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

(defparameter *slynk-port* 4006)
<<<<<<< HEAD
(slynk:create-server :port *slynk-port*  :dont-close t)
=======
;;(slynk:create-server :port *slynk-port*  :dont-close t)
>>>>>>> bdf89d7 (Backup of a64)
;;(setf slynk:*use-dedicated-output-stream* nil) 

(defun ts-info (kw)
  (let ((ts (format nil "~a"
		    (local-time:format-timestring
		     nil (local-time:now)
		     :format '(:year "-" (:month 2) "-" (:day 2) " "
			       (:hour 2) ":" (:min 2) ":" (:sec 2))))))
    (values
     (format nil
	     "~a ~a*C ~a% ~a" ts *temperature* *humidity* kw)
     ts)))

(defun chat (text &optional local)
  (ignore-errors
<<<<<<< HEAD
   (let ((host (if local
                   *server*
                   (format nil
                        "https://api.telegram.org/bot~a/sendMessage?chat_id=~a"
                        *bot-token* *chat-id*))))
     (dex:request host
                :method :post
                :headers '(("Content-Type" . "application/json"))
                :content (format nil "{\"text\": \"~a\"}" text)))))
=======
    (let ((host
           (if local
               *server*
             (format nil
                     "https://api.telegram.org/bot~a/sendMessage?chat_id=~a"
                     *bot-token* *chat-id*))))
      (dex:request host
                   :method :post
                   :headers '(("Content-Type" . "application/json"))
                   :content (format nil "{\"text\": \"~a\"}" text)))))

(defun chat-now (fn)
  (let ((ts (get-universal-time)))
    (unless *state-at* (setf *state-at* ts))
    (when (> ts (+ *state-at* *state-duration*))
      (setf *state-at* ts)
      (chat (ts-info fn)))))
>>>>>>> bdf89d7 (Backup of a64)

(defun print-now (kw)
  (multiple-value-bind (ts-info ts)
      (ts-info kw)
    (ignore-errors
     (execute-non-query
      *db* (concatenate 'string "insert into heating (ts,temp,hum,state)"
                        " values (?,round(?,2),round(?,2),?)")
      ts *temperature* *humidity* (format nil "~a" kw)))
    (terpri)
    (princ ts-info)
    (chat ts-info t)
    (unless (eql kw :idle) (chat ts-info))))

(defun round-2 (x) (when (numberp x) (float (/ (round (* 100 x)) 100))))

(defun fetch-temperature ()
<<<<<<< HEAD
  (let ((th (ignore-errors
	     (uiop:split-string (uiop:run-program *cmd-th* 
=======
  (ignore-errors
   (let ((th (uiop:split-string (uiop:run-program *cmd-th* 
>>>>>>> bdf89d7 (Backup of a64)
						  :output '(:string :stripped t))
			        :separator " ")))
     (setf *temperature* (round-2 (parse-float (car th) :junk-allowed t)))
     (setf *humidity* (round-2 (parse-float (cadr th) :junk-allowed t)))))
  (values *temperature* *humidity*))

<<<<<<< HEAD
(defun heating-op (op)
  (unless
      (ignore-errors
       (case op
         (:on
          (uiop:run-program *cmd-on*)
          (sleep 1)
          (heating-op :state))
         (:off
          (uiop:run-program *cmd-off*)
          (sleep 1)
          (heating-op :state))
         (:state
          (parse-integer
           (uiop:run-program *cmd-state*
                             :output '(:string :stripped t)) :junk-allowed t))))
    (chat (format nil "Heating ~a failed." :op))))

=======
>>>>>>> bdf89d7 (Backup of a64)
(defun heating (mode)
  (case mode
      (:on (uiop:run-program *cmd-on*))
      (:off (uiop:run-program *cmd-off*))
      (otherwise (chat (format nil "Heating mode ~a unknown." mode)))))

;; *heating-needed* *heating-started* *heating-paused*
;;         T                T                 T         wait-to-resume
;;         T                T                NIL        heat-until-pause
;;         T               NIL                .         start-heating
;;        NIL               T                 .         stop-heating
;;        NIL              NIL                .         idle

(declaim (ftype function idle))
(declaim (ftype function heat-until-pause))

(defun wait-to-resume ()
  ;;(print :wait-to-resume)
  (fetch-temperature)
  (cond ((not *temperature*) :ignore) 
	 ((> *temperature* *max-temp*)
	  (setf *heating-needed* nil)
	  (setf *heating-started* nil)
	  (setf *heating-paused* nil)
	  (setf *curr-fn* #'idle))
	 (t (let ((ts (get-universal-time)))
	      (when (> ts (+ *heating-paused-at* *heating-pause-duration*))
		(heating :on)
		(print-now :heating-on)
		(setf *heating-resumed-at* ts)
		(setf *curr-fn* #'heat-until-pause))))))

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
      (fetch-temperature)
      (let ((tp *temperature*))
	(cond ((and tp (< tp *min-temp*))
	       (setf *heating-needed* t)
	       (start-heating)
               (return-from idle))
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

<<<<<<< HEAD

(defun control-loop ()
  (handler-case
      (progn
        (funcall *curr-fn*)
        (chat-now (ts-info (function-name *curr-fn*)))
        (sleep 60))
    (error (e) (chat (ts-info (format nil "~a" e))))))
=======
#+sbcl
(defun function-name (f)
  (intern (cadr (uiop:split-string (format nil "~a" f))) "KEYWORD"))
>>>>>>> bdf89d7 (Backup of a64)

(defun control-heating ()
  (setf *curr-fn* #'idle)
  (setf *idle-at* (- (get-universal-time) (* 20 60)))
  (fetch-temperature)
  (while *forever*
<<<<<<< HEAD
    (control-loop))
=======
         (funcall *curr-fn*)
         (chat-now (ts-info (function-name *curr-fn*)))
         (sleep 60))
>>>>>>> bdf89d7 (Backup of a64)
  (chat (ts-info :stop)))

(defun run-control-heating ()
  (setf *forever* t)
  (bt:make-thread #'control-heating :name "control-heating"))

<<<<<<< HEAD
(setf *control-heating-thread* (run-control-heating))
=======
;;(run-control-heating)

>>>>>>> bdf89d7 (Backup of a64)





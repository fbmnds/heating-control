;;#!/bin/sh
#|
exec sbcl --script "$0" "$@"
|#

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
(ql:quickload '(:websocket-driver :websocket-driver-client))


(defpackage :heating-control
  (:use :cl :parse-float :sqlite)
  (:local-nicknames (#:lt #:local-time)
                    (#:ws #:websocket-driver)
                    (#:wsd #:websocket-driver-client)
                    (#:bt #:bordeaux-threads)
                    (#:ws #:websocket-driver)
                    (#:wsd #:websocket-driver-client))
  (:export #:run-heating-control
           #:stop-heating-control))

(in-package :heating-control)

(defmacro str+ (&rest rest) `(concatenate 'string ,@rest))


(defparameter *path* (str+ (uiop:getenv "HOME") "/projects/heating-control/"))
(load (str+ *path* "secrets/secrets.lisp"))

(defparameter *host* (uiop:hostname))

(defparameter *control-thread* nil)

(defparameter *heating-gpio-pin* 21)      
(defparameter *cmd-on*                    
  (format nil "/usr/bin/echo 1 >/sys/class/gpio/gpio~a/value" *heating-gpio-pin*))   
(defparameter *cmd-off*                   
  (format nil "/usr/bin/echo 0 >/sys/class/gpio/gpio~a/value" *heating-gpio-pin*))   
(defparameter *cmd-state*                 
  (format nil "/usr/bin/cat /sys/class/gpio/gpio~a/value" *heating-gpio-pin*))
(defparameter *cmd-th* (list "/usr/bin/python3" (str+ *path* "temperature.py")))   

(when (equal *host* "a64.fritz.box")
  (setf *heating-gpio-pin* 75)
  (setf *cmd-on* (format nil "/usr/local/bin/gpio~a 1" *heating-gpio-pin*))
  (setf *cmd-off* (format nil "/usr/local/bin/gpio~a 0" *heating-gpio-pin*))
  (setf *cmd-state*
        (format nil "/usr/bin/cat /sys/class/gpio/gpio~a/value" *heating-gpio-pin*))
  (setf *cmd-th* (list "/usr/local/bin/dht22" "")))

(defparameter *chat* nil)
(defparameter *control-ui-backend* (list "192.168.178.6"))
(defparameter *control-ui-n* 200)

(defparameter *min-temp* 10)
(defparameter *max-temp* 10.5)

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
(defparameter *state-duration* (* 6 60 60))

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
;;(slynk:create-server :port *slynk-port*  :dont-close t)
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
  (when *chat*
    (ignore-errors
      (let ((host
             (if local
                 *server*
               (format nil
                       "https://api.telegram.org/bot~a/sendMessage?chat_id=~a"
                       *bot-token* *chat-id*))))
        (dex:request host
                     :method :post
                     :headers '(("Content-Type" . "application/json"))
                     :content (format nil "{\"text\": \"~a\"}" text))))))

(defun chat-now (fn)
  (let ((ts (get-universal-time)))
    (unless *state-at* (setf *state-at* ts))
    (when (> ts (+ *state-at* *state-duration*))
      (setf *state-at* ts)
      (chat (ts-info fn)))))

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

(defun select-data (&optional (n *control-ui-n*))
  (str+ "sqlite3 -json "
        " ~/projects/heating-control/data/heating.db"
        " 'select * from heating "
        " where not temp is null and not hum is null "
        " order by ts "
        (format nil " desc limit ~a;'" n)))

(defun send-data(data url)
  (let* ((url (str+ "ws://" url ":7700/"))
         (client (wsd:make-client url)))
    (progn
      ;;(print data)
      ;;(ws:on :open client (lambda () (format t "~&connected~%")))
      (ws:start-connection client)
      (ws:on :message client (lambda (message) (format t "~a" message)))
      (ws:send client data)
      (sleep 1))
    (ws:close-connection client)))

(defun broadcast-data ()
  (handler-case
      (let ((data (uiop:run-program (select-data) :force-shell t
                                    :output '(:string :stripped t))))
        (loop for url in *control-ui-backend*
              do (send-data data url)))
    (condition (c) (format t "broadcast error: ~a" c))))

(defun fetch-temperature ()
  (ignore-errors
   (let ((th (uiop:split-string (uiop:run-program *cmd-th* 
						  :output '(:string :stripped t))
			        :separator " ")))
     (setf *temperature* (round-2 (parse-float (car th) :junk-allowed t)))
     (setf *humidity* (round-2 (parse-float (cadr th) :junk-allowed t)))))
  (broadcast-data)
  (values *temperature* *humidity*))

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

#+sbcl
(defun function-name (f)
  (intern (cadr (uiop:split-string (format nil "~a" f))) "KEYWORD"))

(defun control-heating ()
  (setf *curr-fn* #'idle)
  (setf *idle-at* (- (get-universal-time) (* 20 60)))
  (fetch-temperature)
  (while *forever*
         (funcall *curr-fn*)
         (chat-now (ts-info (function-name *curr-fn*)))
         (sleep 60))
  (chat (ts-info :stop)))

(defun run-control-heating ()
  (setf *control-thread* (bt:make-thread #'control-heating)))

(defun stop-control-heating ()
  (bt:destroy-thread *control-thread*))

;;(run-control-heating)







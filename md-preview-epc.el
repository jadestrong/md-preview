;;; epcs.el --- EPC Server              -*- lexical-binding: t -*-

;; Copyright (C) 2011,2012,2013  Masashi Sakurai

;; Author: Masashi Sakurai <m.sakurai at kiwanami.net>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; deferred
(cl-defmacro md-preview-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar md-preview-deferred-debug nil
  "Debug output switch.")

(defvar md-preview-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun md-preview-deferred-log (&rest args)
  "[internal] Debug log function."
  (when md-preview-deferred-debug
    (with-current-buffer (get-buffer-create "*md-preview-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" md-preview-deferred-debug-count (apply #'format args)))))
    (cl-incf md-preview-deferred-debug-count)))

(defvar md-preview-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(cl-defmacro md-preview-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`md-preview-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal md-preview-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar md-preview-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar md-preview-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `md-preview-deferred-post-task' and `md-preview-deferred-worker'.")

(defun md-preview-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`md-preview-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack md-preview-deferred-queue)
    (md-preview-deferred-log "QUEUE-POST [%s]: %s" (length md-preview-deferred-queue) pack)
    (run-at-time md-preview-deferred-tick-time nil 'md-preview-deferred-worker)
    d))

(defun md-preview-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when md-preview-deferred-queue
    (let* ((pack (car (last md-preview-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq md-preview-deferred-queue (nbutlast md-preview-deferred-queue))
      (condition-case err
          (setq value (md-preview-deferred-exec-task d which arg))
        (error
         (md-preview-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: md-preview-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `md-preview-deferred-resignal')
;; cancel      : a canceling function (default `md-preview-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct md-preview-deferred-object
  (callback 'identity)
  (errorback 'md-preview-deferred-resignal)
  (cancel 'md-preview-deferred-default-cancel)
  next status value)

(defun md-preview-deferred-resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun md-preview-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (md-preview-deferred-log "CANCEL : %s" d)
  (setf (md-preview-deferred-object-callback d) 'identity)
  (setf (md-preview-deferred-object-errorback d) 'md-preview-deferred-resignal)
  (setf (md-preview-deferred-object-next d) nil)
  d)

(defun md-preview-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (md-preview-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "md-preview-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (md-preview-deferred-object-callback d)
                    (md-preview-deferred-object-errorback d)))
        (next-deferred (md-preview-deferred-object-next d)))
    (cond
     (callback
      (md-preview-deferred-condition-case err
                                         (let ((value (funcall callback arg)))
                                           (cond
                                            ((md-preview-deferred-object-p value)
                                             (md-preview-deferred-log "WAIT NEST : %s" value)
                                             (if next-deferred
                                                 (md-preview-deferred-set-next value next-deferred)
                                               value))
                                            (t
                                             (if next-deferred
                                                 (md-preview-deferred-post-task next-deferred 'ok value)
                                               (setf (md-preview-deferred-object-status d) 'ok)
                                               (setf (md-preview-deferred-object-value d) value)
                                               value))))
                                         (error
                                          (cond
                                           (next-deferred
                                            (md-preview-deferred-post-task next-deferred 'ng err))
                                           (t
                                            (md-preview-deferred-log "ERROR : %S" err)
                                            (message "deferred error : %S" err)
                                            (setf (md-preview-deferred-object-status d) 'ng)
                                            (setf (md-preview-deferred-object-value d) err)
                                            err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (md-preview-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (md-preview-deferred-resignal arg)))))))

(defun md-preview-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (md-preview-deferred-object-next prev) next)
  (cond
   ((eq 'ok (md-preview-deferred-object-status prev))
    (setf (md-preview-deferred-object-status prev) nil)
    (let ((ret (md-preview-deferred-exec-task
                next 'ok (md-preview-deferred-object-value prev))))
      (if (md-preview-deferred-object-p ret) ret
        next)))
   ((eq 'ng (md-preview-deferred-object-status prev))
    (setf (md-preview-deferred-object-status prev) nil)
    (let ((ret (md-preview-deferred-exec-task next 'ng (md-preview-deferred-object-value prev))))
      (if (md-preview-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun md-preview-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-md-preview-deferred-object :callback callback)
    (make-md-preview-deferred-object)))

(defun md-preview-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (md-preview-deferred-exec-task d 'ok arg))

(defun md-preview-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (md-preview-deferred-exec-task d 'ng arg))

(defun md-preview-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (md-preview-deferred-post-task d 'ok arg))

(defun md-preview-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (md-preview-deferred-callback-post (md-preview-deferred-new callback))."
  (let ((d (if callback
               (make-md-preview-deferred-object :callback callback)
             (make-md-preview-deferred-object))))
    (md-preview-deferred-callback-post d arg)
    d))

(defun md-preview-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-md-preview-deferred-object :callback callback)))
    (md-preview-deferred-set-next d nd)))

(defun md-preview-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-md-preview-deferred-object :errorback callback)))
    (md-preview-deferred-set-next d nd)))

(defvar md-preview-epc-debug nil)

(defun md-preview-epc-log (&rest args)
  (when md-preview-epc-debug
    (with-current-buffer (get-buffer-create "*md-preview-epc-log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n\n\n"))))

(defun md-preview-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar md-preview-epc-uid 1)

(defun md-preview-epc-uid ()
  (cl-incf md-preview-epc-uid))

(defvar md-preview-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct md-preview-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun md-preview-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return md-preview-epc-connection object."
  (md-preview-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (md-preview-epc-uid))
         (connection-name (format "md-preview-epc con %s" connection-id))
         (connection-buf (md-preview-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-md-preview-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (md-preview-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (md-preview-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (md-preview-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun md-preview-epc-process-sentinel (connection process msg)
  (md-preview-epc-log "!! Process Sentinel [%s] : %S : %S"
                     (md-preview-epc-connection-name connection) process msg)
  (md-preview-epc-disconnect connection))

(defun md-preview-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (md-preview-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (md-preview-epc-connection-process connection)))
    (md-preview-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun md-preview-epc-disconnect (connection)
  (let ((process (md-preview-epc-connection-process connection))
        (buf (md-preview-epc-connection-buffer connection))
        (name (md-preview-epc-connection-name connection)))
    (md-preview-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (md-preview-epc-log "!! Disconnected finished [%s]" name)))

(defun md-preview-epc-process-filter (connection process message)
  (md-preview-epc-log "INCOMING: [%s] [%S]" (md-preview-epc-connection-name connection) message)
  (with-current-buffer (md-preview-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (md-preview-epc-process-available-input connection process)))

(defun md-preview-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (md-preview-deferred-new callback)
             (md-preview-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun md-preview-epc-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given,
observers can get the values by following code:

  (lambda (event)
    (destructuring-bind
     (event-sym (args))
     event ... ))
"
  (let ((observers (cddr channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (md-preview-deferred-callback-post d event))))

(defun md-preview-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (md-preview-epc-net-have-input-p)
      (let ((event (md-preview-epc-net-read-or-lose process))
            (ok nil))
        (md-preview-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'md-preview-epc-signal-send
                         (cons (md-preview-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (md-preview-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (md-preview-epc-process-available-input connection process)))))))

(defun md-preview-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (md-preview-epc-net-decode-length))))

(defun md-preview-epc-net-read-or-lose (_process)
  (condition-case error
      (md-preview-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun md-preview-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (md-preview-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun md-preview-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun md-preview-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct md-preview-epc-manager
  "Root object that holds all information related to an EPC activity.

`md-preview-epc-start-epc' returns this object.

title          : instance name for displaying on the `md-preview-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : md-preview-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct md-preview-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar md-preview-epc-live-connections nil
  "[internal] A list of `md-preview-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun md-preview-epc-server-process-name (uid)
  (format "md-preview-epc-server:%s" uid))

(defun md-preview-epc-server-buffer-name (uid)
  (format " *%s*" (md-preview-epc-server-process-name uid)))

(defun md-preview-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (md-preview-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (md-preview-epc-disconnect (md-preview-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 md-preview-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq md-preview-epc-live-connections (delete mngr md-preview-epc-live-connections))
    ))

(defun md-preview-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun md-preview-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an md-preview-epc-connection instance."
  (let* ((mngr mngr)
         (conn (md-preview-epc-manager-connection mngr))
         (channel (md-preview-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (md-preview-epc-log "SIG CALL: %S" args)
                    (apply 'md-preview-epc-handler-called-method ,mngr (md-preview-epc-args args))))
               (return
                . (lambda (args)
                    (md-preview-epc-log "SIG RET: %S" args)
                    (apply 'md-preview-epc-handler-return ,mngr (md-preview-epc-args args))))
               (return-error
                . (lambda (args)
                    (md-preview-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'md-preview-epc-handler-return-error ,mngr (md-preview-epc-args args))))
               (epc-error
                . (lambda (args)
                    (md-preview-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'md-preview-epc-handler-epc-error ,mngr (md-preview-epc-args args))))
               (methods
                . (lambda (args)
                    (md-preview-epc-log "SIG METHODS: %S" args)
                    (md-preview-epc-handler-methods ,mngr (caadr args))))
               ) do
             (md-preview-epc-signal-connect channel method body))
    (push mngr md-preview-epc-live-connections)
    mngr))

(defun md-preview-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (md-preview-epc-manager-connection mngr)))
    (md-preview-epc-net-send conn (cons method messages))))

(defun md-preview-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (md-preview-epc-manager-methods mngr)
           if (eq method-name (md-preview-epc-method-name i))
           do (cl-return i)))

(defun md-preview-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (md-preview-epc-manager-methods mngr)
                  collect
                  (list
                   (md-preview-epc-method-name i)
                   (or (md-preview-epc-method-arg-specs i) "")
                   (or (md-preview-epc-method-docstring i) "")))))
    (md-preview-epc-manager-send mngr 'return uid info)))

(defun md-preview-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (md-preview-epc-manager-methods mngr))
           (method (md-preview-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (md-preview-epc-log "ERR: No such method : %s" name)
        (md-preview-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (md-preview-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((md-preview-deferred-object-p ret)
                (md-preview-deferred-nextc ret
                                          (lambda (xx) (md-preview-epc-manager-send mngr 'return uid xx))))
               (t (md-preview-epc-manager-send mngr 'return uid ret))))
          (error
           (md-preview-epc-log "ERROR : %S" err)
           (md-preview-epc-manager-send mngr 'return-error uid err))))))))

(defun md-preview-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (md-preview-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (md-preview-epc-manager-sessions mngr) ret)))

(defun md-preview-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (md-preview-epc-manager-sessions mngr))))
    (cond
     (pair
      (md-preview-epc-log "RET: id:%s [%S]" uid args)
      (md-preview-epc-manager-remove-session mngr uid)
      (md-preview-deferred-callback (cdr pair) args))
     (t                                 ; error
      (md-preview-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun md-preview-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (md-preview-epc-manager-sessions mngr))))
    (cond
     (pair
      (md-preview-epc-log "RET-ERR: id:%s [%S]" uid args)
      (md-preview-epc-manager-remove-session mngr uid)
      (md-preview-deferred-errorback (cdr pair) (format "%S" args)))
     (t                                 ; error
      (md-preview-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun md-preview-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (md-preview-epc-manager-sessions mngr))))
    (cond
     (pair
      (md-preview-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (md-preview-epc-manager-remove-session mngr uid)
      (md-preview-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t                                 ; error
      (md-preview-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun md-preview-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (md-preview-epc-uid))
        (sessions (md-preview-epc-manager-sessions mngr))
        (d (md-preview-deferred-new)))
    (push (cons uid d) sessions)
    (setf (md-preview-epc-manager-sessions mngr) sessions)
    (md-preview-epc-manager-send mngr 'call uid method-name args)
    d))

(defun md-preview-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-md-preview-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (md-preview-epc-manager-methods mngr))))
    (setf (md-preview-epc-manager-methods mngr) methods)
    method))

(defun md-preview-epc-sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'md-preview-epc-nothing))
    (md-preview-deferred-chain
     d
     (md-preview-deferred-nextc it
                               (lambda (x) (setq result x)))
     (md-preview-deferred-error it
                               (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'md-preview-epc-nothing)
      (save-current-buffer
        (accept-process-output
         (md-preview-epc-connection-process (md-preview-epc-manager-connection mngr))
         0 md-preview-epc-accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun md-preview-epc-call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (md-preview-epc-sync mngr (md-preview-epc-call-deferred mngr method-name args)))

(defun md-preview-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (md-preview-epc-connection-process (md-preview-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

;; epcs
(defvar md-preview-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`md-preview-epc-manager' instance]).
When the server process accepts the client connection, the
`md-preview-epc-manager' instance is created and stored in this variable
`md-preview-epc-server-client-processes'. This variable is used for the management
purpose.")

;; md-preview-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `md-preview-epc-manager' instances
(cl-defstruct md-preview-epc-server name process port connect-function)

(defvar md-preview-epc-server-processes nil
  "[internal] A list of ([process object] . [`md-preview-epc-server' instance]).
This variable is used for the management purpose.")

(defun md-preview-epc-server-get-manager-by-process (proc)
  "[internal] Return the md-preview-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in md-preview-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun md-preview-epc-server-accept (process)
  "[internal] Initialize the process and return md-preview-epc-manager object."
  (md-preview-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (md-preview-epc-uid))
         (connection-name (format "md-preview-epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-md-preview-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (md-preview-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (md-preview-epc-process-filter connection p m)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p e)
                            (md-preview-epc-process-sentinel connection p e)))
    (make-md-preview-epc-manager :server-process process :port t
                                :connection connection)))

(defun md-preview-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (md-preview-epc-log "LSPBRIDGE-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (md-preview-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (md-preview-epc-server-accept process)))
            (push (cons process mngr) md-preview-epc-server-client-processes)
            (md-preview-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (md-preview-epc-log "LSPBRIDGE-EPC-SERVER- Protocol error: %S" err)
         (md-preview-epc-log "LSPBRIDGE-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process md-preview-epc-server-client-processes)) _d)
        (when pair
          (md-preview-epc-log "LSPBRIDGE-EPC-SERVER- DISCONNECT %S" process)
          (md-preview-epc-stop-epc (cdr pair))
          (setq md-preview-epc-server-client-processes
                (assq-delete-all process md-preview-epc-server-client-processes))
          ))
      nil))))

(defun md-preview-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "MD-PREVIEW EPC Server %s" (md-preview-epc-uid)))
       (buf (md-preview-epc-make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
         :noquery t
         :sentinel
         (lambda (process message)
           (md-preview-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-md-preview-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          md-preview-epc-server-processes)
    main-process))

(provide 'md-preview-epc)
;;; md-preview-epc.el ends here

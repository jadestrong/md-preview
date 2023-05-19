;;; md-preview.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 JadeStrong
;;
;; Author: JadeStrong <jadestrong@163.com>
;; Maintainer: JadeStrong <jadestrong@163.com>
;; Created: May 16, 2023
;; Modified: May 16, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jadestrong/md-preview
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
;; (require 'md-preview-epc)
(require 'epcs)

;; (defvar md-preview-server nil
;;   "The Md-Preview Server.")

(defvar md-preview-process nil
  "The Md-Preview Process")

(defvar md-preview-node-file (expand-file-name "md_preview.mjs" (if load-file-name
                                                                    (file-name-directory load-file-name)
(defun md-preview--is-dark-theme ()
  "Return t if the current Emacs theme is a dark theme."
  (eq (frame-parameter nil 'background-mode) 'dark))

(defvar md-preview-server-port nil)

(cl-defmacro md-preview--with-file-buffer (filename &rest body)
  "Evaluate BODY in buffer with FILEPATH."
  (declare (indent 1))
  `(cl-dolist (buffer (buffer-list))
     (when-let* ((file-name (buffer-file-name buffer))
                 (match-buffer (or (string-equal file-name ,filename)
                                   (string-equal (file-truename file-name) ,filename))))
       (with-current-buffer buffer
         ,@body)
       (cl-return))))

;; DEPRECATED
;; (defun md-preview--start-epc-server ()
;;   "Function to start the EPC server."
;;   (unless (process-live-p md-preview-server)
;;     (setq md-preview-server
;;           (md-preview-epc-server-start
;;            (lambda (mngr)
;;              (let ((mngr mngr))
;;                (md-preview-epc-define-method mngr 'eval-in-emacs 'md-preview--eval-in-emacs-func)
;;                (md-preview-epc-define-method mngr 'get-emacs-var 'md-preview--get-emacs-var-func)
;;                (md-preview-epc-define-method mngr 'get-emacs-vars 'md-preview--get-emacs-vars-func)
;;                (md-preview-epc-define-method mngr 'get-user-emacs-directory 'md-preview--user-emacs-directory)
;;                ))))
;;     (if md-preview-server
;;         (setq md-preview-server-port (process-contact md-preview-server :service))
;;       (error "[Md-Preview] md-preview-server failed to start")))
;;   md-preview-server)

(defun md-preview--get-emacs-func-result-func (sexp-string)
  (eval (read sexp-string)))

(defun md-preview--eval-in-emacs-func (sexp-string)
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun md-preview--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun md-preview--get-emacs-vars-func (&rest vars)
  (mapcar #'md-preview--get-emacs-var-func vars))

;; (defvar md-preview-epc-process nil)

(defvar md-preview-internal-process nil)
;; (defvar md-preview-internal-process-prog nil)
;; (defvar md-preview-internal-process-args nil)

(defcustom md-preview-name "*md-preview*"
  "Name of Md-Preview buffer."
  :type 'string)

(defcustom md-preview-node-command "node"
  "The Python interpreter used to run md_preview.js."
  :type 'string)

(defcustom md-preview-enable-debug nil
  "If you got segfault error, please turn this option.
Then Md-Preview will start by gdb, please send new issue with `*md-preview*' buffer content when next crash."
  :type 'boolean)

;; (defcustom md-preview-enable-profile nil
;;   "Enable this option to output performance data to ~/md-preview.prof."
;;   :type 'boolean)

(defun md-preview--user-emacs-directory ()
  "Get lang server with project path, file path or file extension."
  (expand-file-name user-emacs-directory))

(defvar md-preview-is-starting nil)
(defvar md-preview-first-call-method nil)
(defvar md-preview-first-call-args nil)

(defun md-preview--start-epc ()
  "Function to start the EPC."
  (unless (epc:live-p md-preview-process)
    (setq md-preview-process (epc:start-epc
                              md-preview-node-command
                              (list md-preview-node-file)))
    (epc:define-method md-preview-process 'eval-in-emacs 'md-preview--eval-in-emacs-func)
    (epc:define-method md-preview-process 'get-emacs-var 'md-preview--get-emacs-var-func)
    (epc:define-method md-preview-process 'get-emacs-vars 'md-preview--get-emacs-vars-func)
    (epc:define-method md-preview-process 'get-user-emacs-directory 'md-preview--user-emacs-directory)
    ;; (setq md-preview-process
    ;;       (epcs:server-start (lambda (mngr)
    ;;                            (epc:define-method mngr 'eval-in-emacs 'md-preview--eval-in-emacs-func)
    ;;                            (epc:define-method mngr 'get-emacs-var 'md-preview--get-emacs-var-func)
    ;;                            (epc:define-method mngr 'get-emacs-vars 'md-preview--get-emacs-vars-func)
    ;;                            (epc:define-method mngr 'get-user-emacs-directory 'md-preview--user-emacs-directory)
    ;;                            )))
    ;; (if md-preview-process
    ;;     (setq md-preview-server-port (process-contact md-preview-process :service))
    ;;   (error "[MD-PREVIEW] md-preview-process failed to start"))
    )
  md-preview-process)


(defun md-preview-call-async (method &rest args)
  "Call NODE EPC function METHOD and ARGS asynchronously."
  (if (epc:live-p md-preview-process)
      (deferred:$
       (epc:call-deferred md-preview-process (read method) args))
    ;; (error "[MD-PREVIEW] md-preview-process not live!")
    (setq md-preview-first-call-method method)
    (setq md-preview-first-call-args args)
    (md-preview-start-process)))

;; DEPRECATED 我们应该不需要从连接另一端
;; (defun md-preview-call-async (method &rest args)
;;   "Call Python EPC function METHOD and ARGS asynchronously."
;;   (if (md-preview-epc-live-p md-preview-epc-process)
;;       (md-preview-deferred-chain
;;         (md-preview-epc-call-deferred md-preview-epc-process (read method) args))
;;     (setq md-preview-first-call-method method)
;;     (setq md-preview-first-call-args args)
;;     (md-preview-start-process)))

(defun md-preview-restart-process ()
  "Stop and restart Md-Preview process."
  (interactive)
  (setq md-preview-is-starting nil)

  (md-preview-kill-process)
  (md-preview-start-process)
  (message "[Md-Preview] Process restarted."))

(defun md-preview-start-process ()
  "Start Md-Preview process if it isn't started."
  (setq md-preview-is-starting t)
  (unless (epc:live-p md-preview-process)
    ;; start epc server and set `md-preview-server-port'
    ;; (let* ((md-preview-process (md-preview))))
    (md-preview--start-epc)
    (setq md-preview-is-starting nil)
    (when (and md-preview-first-call-method
               md-preview-first-call-args)
      (deferred:$
       (epc:call-deferred md-preview-process
                          (read md-preview-first-call-method)
                          md-preview-first-call-args)
       (setq md-preview-first-call-method nil)
       (setq md-preview-first-call-args nil)))
    ;; (let* ((md-preview-args (append
    ;;                          (list md-preview-node-file)
    ;;                          (list (number-to-string md-preview-server-port))
    ;;                          )))
    ;;   (let ((process-connection-type t))
    ;;     (setq md-preview-internal-process
    ;;           (apply 'start-process
    ;;                  md-preview-name md-preview-name
    ;;                  md-preview-node-command md-preview-args)))
    ;; (set-process-query-on-exit-flag md-preview-internal-process nil)

    ;; ;; (setq md-preview-process (epc:start-epc
    ;; ;;                           md-preview-node-command
    ;; ;;                           (list md-preview-node-file)))
    ;; )
    ))

;; (defun md-preview-start-process ()
;;   "Start Md-Preview process if it isn't started."
;;   (setq md-preview-is-starting t)
;;   (unless (md-preview-epc-live-p md-preview-epc-process)
;;     ;; start epc server and set `md-preview-server-port'
;;     (md-preview--start-epc-server)
;;     (let* ((md-preview-args (append
;;                             (list md-preview-python-file)
;;                             (list (number-to-string md-preview-server-port))
;;                             (when md-preview-enable-profile
;;                               (list "profile"))
;;                             )))

;;       ;; Set process arguments.
;;       (if md-preview-enable-debug
;;           (progn
;;             (setq md-preview-internal-process-prog "gdb")
;;             (setq md-preview-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" md-preview-python-command) md-preview-args)))
;;         (setq md-preview-internal-process-prog md-preview-python-command)
;;         (setq md-preview-internal-process-args md-preview-args))

;;       ;; Start python process.
;;       (let ((process-connection-type t))
;;         (setq md-preview-internal-process
;;               (apply 'start-process
;;                      md-preview-name md-preview-name
;;                      md-preview-internal-process-prog md-preview-internal-process-args)))
;;       (set-process-query-on-exit-flag md-preview-internal-process nil))))

(defvar md-preview-stop-process-hook nil)

(defun md-preview-kill-process ()
  "Stop Md-Preview process and kill all Md-Preview buffers."
  (interactive)
  ;; Run stop process hooks.
  (run-hooks 'md-preview-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (md-preview--kill-node-process))

                                        ;；DEPRECATED
;; (defun md-preview-kill-process ()
;;   "Stop Md-Preview process and kill all Md-Preview buffers."
;;   (interactive)

;;   ;; Run stop process hooks.
;;   (run-hooks 'md-preview-stop-process-hook)

;;   ;; Kill process after kill buffer, make application can save session data.
;;   (md-preview--kill-python-process))

(add-hook 'kill-emacs-hook #'md-preview-kill-process)

(defun md-preview--kill-node-process ()
  "Kill Md-Preview background python process."
  (when (epc:live-p md-preview-process)
    ;; Cleanup before exit Md-Preview server process.
    (md-preview-call-async "cleanup")
    ;; Delete Md-Preview server process.
    (epc:stop-epc md-preview-process)
    ;; Kill *md-preview* buffer.
    (when (get-buffer md-preview-name)
      (kill-buffer md-preview-name))
    (setq md-preview-process nil)
    (message "[Md-Preview] Process terminated.")))

                                        ;；DEPRECATED
;; (defun md-preview--kill-python-process ()
;;   "Kill Md-Preview background python process."
;;   (when (md-preview-epc-live-p md-preview-epc-process)
;;     ;; Cleanup before exit Md-Preview server process.
;;     (md-preview-call-async "cleanup")
;;     ;; Delete Md-Preview server process.
;;     (md-preview-epc-stop-epc md-preview-epc-process)
;;     ;; Kill *md-preview* buffer.
;;     (when (get-buffer md-preview-name)
;;       (kill-buffer md-preview-name))
;;     (setq md-preview-epc-process nil)
;;     (message "[Md-Preview] Process terminated.")))

;; (defun md-preview--first-start (md-preview-epc-port)
;;   "Call `md-preview--open-internal' upon receiving `start_finish' signal from server."
;;   ;; Make EPC process.
;;   (setq md-preview-epc-process (make-md-preview-epc-manager
;;                                :server-process md-preview-internal-process
;;                                :commands (cons md-preview-internal-process-prog md-preview-internal-process-args)
;;                                :title (mapconcat 'identity (cons md-preview-internal-process-prog md-preview-internal-process-args) " ")
;;                                :port md-preview-epc-port
;;                                :connection (md-preview-epc-connect "localhost" md-preview-epc-port)
;;                                ))
;;   (md-preview-epc-init-epc-layer md-preview-epc-process)
;;   (setq md-preview-is-starting nil)

;;   ;; 解决第一次调用的时候还没启动起来的问题，记录并等成功后再执行
;;   (when (and md-preview-first-call-method
;;              md-preview-first-call-args)
;;     (md-preview-deferred-chain
;;       (md-preview-epc-call-deferred md-preview-epc-process
;;                                    (read md-preview-first-call-method)
;;                                    md-preview-first-call-args)
;;       (setq md-preview-first-call-method nil)
;;       (setq md-preview-first-call-args nil)
;;       )))

(defun md-preview-test ()
  (interactive)
  (message "start test...")
  (deferred:$
   (md-preview-call-async "echo" (buffer-name))
   (deferred:nextc it
                   (lambda (x) (message "Return : %S" x))))
  ;; (message (md-preview-call-async "echo" (buffer-name)))
  )

(provide 'md-preview)
;;; md-preview.el ends here

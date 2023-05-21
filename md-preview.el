;;; md-preview.el --- MD Preview -*- lexical-binding: t; -*-
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
;;; Code:
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
;; (require 'md-preview-epc)
(require 'epc)

(defvar md-preview-process nil
  "The Md-Preview Process.")

(defvar md-preview-node-file (expand-file-name "md_preview.mjs" (if load-file-name
                                                                    (file-name-directory load-file-name)
                                                                  default-directory)))
(defun md-preview--is-dark-theme ()
  "Return t if the current Emacs theme is a dark theme."
  (eq (frame-parameter nil 'background-mode) 'dark))

(defvar md-preview-server-port nil)

(cl-defmacro md-preview--with-file-buffer (filename &rest body)
  "Evaluate BODY in buffer with FILENAME."
  (declare (indent 1))
  `(cl-dolist (buffer (buffer-list))
     (when-let* ((file-name (buffer-file-name buffer))
                 (match-buffer (or (string-equal file-name ,filename)
                                   (string-equal (file-truename file-name) ,filename))))
       (with-current-buffer buffer
         ,@body)
       (cl-return))))

(defun md-preview--get-emacs-func-result-func (sexp-string)
  "Eval SEXP-STRING, and return the result."
  (eval (read sexp-string)))

(defun md-preview--eval-in-emacs-func (sexp-string)
  "Eval SEXP-STRING."
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun md-preview--get-emacs-var-func (var-name)
  "Get the VAR-NAME variable and return the value."
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun md-preview--get-emacs-vars-func (&rest vars)
  "Get VARS and return values."
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
    (epc:define-method md-preview-process 'get-emacs-func-result 'md-preview--get-emacs-func-result-func))
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
    (md-preview--start-epc)
    (setq md-preview-is-starting nil)
    (when (and md-preview-first-call-method
               md-preview-first-call-args)
      (deferred:$
       (epc:call-deferred md-preview-process
                          (read md-preview-first-call-method)
                          md-preview-first-call-args)
       (setq md-preview-first-call-method nil)
       (setq md-preview-first-call-args nil)))))

(defvar md-preview-stop-process-hook nil)

(defun md-preview-kill-process ()
  "Stop Md-Preview process and kill all Md-Preview buffers."
  (interactive)
  ;; Run stop process hooks.
  (run-hooks 'md-preview-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (md-preview--kill-node-process))

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

(defun md-preview ()
  "Preview current buffer."
  (interactive)
  (let* ((file-path (buffer-file-name)))
    (md-preview-call-async "preview" file-path)))

(defun md-preview-show-preview-window (file-path)
  "Preview the FILE-PATH in other window."
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (xwidget-webkit-browse-url file-path))


(defun md-preview-open (url)
  "Open md file URL and preview it."
  (interactive "G[MD] Open: ")
  (if-let* ((url (expand-file-name url))
            (extension-name (downcase (file-name-extension url)))
            ((string= extension-name "md")))
      (progn
        (message "opening...")
        (deferred:$
         (md-preview-call-async "open" url)
         (deferred:nextc it (lambda (preview-url)
                              (delete-other-windows)
                              (xwidget-webkit-browse-url preview-url)))
         (message "opened.")))
    (error "Invalid file %s" url)))


(provide 'md-preview)
;;; md-preview.el ends here

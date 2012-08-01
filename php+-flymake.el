;;; php+-flymake.el --- Flymake integration for php+-mode

;; Version: 1.0
;; Created: 07-27-2012
;; Copyright © 2009 Brian Zwahr
;; Author(s):
;; Michael Dwyer <mdwyer@ehtech.in>

;; *************************************************************************

;;; *****
;;; About
;;; *****

;; In-buffer results from the php-compile functions (php linter, phpcs and phpmd).

;;; *****
;;; Usage
;;; *****

;; Just enable flymake-mode.

;; *************************************************************************

;;; ************
;;; REQUIREMENTS
;;; ************

(require 'flymake)

;;; *********
;;; CUSTOMIZE
;;; *********

(defcustom php+-flymake-enable nil
  "Whether to enable flymake in php+-mode buffers automatically."
  :group 'php-test
  :type 'boolean)

(defcustom php+-flymake-wrapper (executable-find 
                                 (concat
                                  (file-name-directory 
                                   (locate-library "php+-mode.el"))
                                  "bin/php+-flymake.sh"))
  "Location of php+-flymake wrapper"
  :group 'php-test
  :type 'string)

(defcustom php+-flymake-tests '(lint phpcs phpmd)
  "Which tests to include during php-compile."
  :group 'php-test
  :type '(set (const lint)
              (const phpcs)
              (const phpmd)))

(defun php+-flymake-init ()
  (when php+-flymake-enable
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       (if (fboundp 'flymake-create-temp-copy)
                           'flymake-create-temp-copy
                         'flymake-create-temp-intemp)))
           (args (append (when (member 'lint php+-flymake-tests) '("-l"))
                         (when (member 'lint php+-flymake-tests) 
                           `("-c" ,phpcs-standard))
                         (when (member 'lint php+-flymake-tests) 
                           `("-m" ,(symbol-name phpmd-format) 
                             ,(mapconcat 'symbol-name phpmd-rulesets ",")))
                         `(,temp-file)))
           (dir (file-name-directory temp-file))
           (command `(,php+-flymake-wrapper ,args ,dir)))
      (flymake-log 3 "php+-flymake command: %s" command)
      command)))

(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking
   FILE-NAME. This is a replacement for
   `flymake-create-temp-inplace'. The difference is that it gives
   a file name in `temporary-file-directory' instead of the same
   directory as FILE-NAME.

   For the use of PREFIX see that function.

   Note that not making the temporary file in another directory
   \(like here) will not if the file you are checking depends on
   relative paths to other files \(for the type of checks flymake
   makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (file-truename (make-temp-file name nil ext))))
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

(eval-after-load "flymake"
  '(progn
     (add-to-list 'flymake-err-line-patterns
                  '("\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)" 1 2 3 4))
     (add-to-list 'flymake-err-line-patterns
                  '("\\([^:]*\\):\\([0-9]+\\)\\s-+\\(.*\\)" 1 2 3 3))
     (setf (car (flymake-get-file-name-mode-and-masks "test.php")) 
           'php+-flymake-init)
     (add-to-list 'flymake-allowed-file-name-masks 
                  '("\\.inc\\'" php+-flymake-init))
     (add-hook 'php+-mode-hook 'php+-flymake-hook t)))

(defun php+-flymake-hook ()
  (when php+-flymake-enable
    ;; We are seeing crashes on Emacs 24 Cocoa when this is enabled.
    ;; Reference: 
    ;; http://superuser.com/questions/125569/how-to-fix-emacs-popup-dialogs-on-mac-os-x
    (setf flymake-gui-warnings-enabled nil)
    (flymake-mode 1)))

(provide 'php+-flymake)

;;; zf-mode.el

;; Version: 2.0
;; Created: 8-25-2009
;; Last modified: Time-stamp: "2012-03-22 15:18:49 mdwyer"
;; Copyright © 2009 Brian Zwahr
;; Author(s):
;; Brian Zwahr <echosa@gmail.com>

;; *************************************************************************

;;; *****
;;; About
;;; *****

;; Zf mode is an emacs mode that makes PHP programming, especially when using
;; the Zend Framework, easier and faster.

;;; *****
;;; Usage
;;; *****

;; (add-to-list 'load-path "/path/to/zf-mode/")
;; (add-to-list 'load-path "/path/to/zf-mode/bundled")

;; Requires (and is bundled with) php-mode.

;; Optional packages supported:
;; camelCase
;; company
;; hideshow
;; php-electric
;; wrap-region

;; Installation:
;; Ensure that the zf-mode files and all requirements are in your emacs
;; load path and loaded.
;; Place the following in your .emacs setup:
;; (require 'zf-mode)
;; (zf-mode-setup)

;; Keybindings:
;; The ZF menu can be used to call most functions.
;; Use C-h m from a zf-mode buffer to view the keybindings.
;; Look for the zf minor mode section.

;; *************************************************************************

;;; ************
;;; REQUIREMENTS
;;; ************
;; pulled from php-mode
(require 'font-lock)
(require 'cc-mode)
(require 'cc-langs)

(require 'php-completion)
(require 'php-const)
(require 'php-doc)
(require 'php-edit)
(require 'php-format)
(require 'php-funcs)
(require 'php-help)
(require 'php-parse)
(require 'php-project)
(require 'php-refactor)
(require 'php-string)
(require 'php-structure)
(require 'php-tags)
(require 'php-test)
(require 'string-utils)
(require 'zf-font-lock)
(require 'zf-lineup)
(require 'zf-mode-extras)
(require 'zf-utils)

;;; *********
;;; CUSTOMIZE
;;; *********
(defgroup zf-mode nil
  "Customizations for zf-mode."
  :group 'languages)

;; pulled and renamed from php-mode
;;;###autoload
(defcustom php-file-patterns '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'")
  "List of file patterns for which to automatically invoke `zf-mode'."
  :type '(repeat (regexp :tag "Pattern"))
  :set (lambda (sym val)
         (set-default sym val)
         (let ((php-file-patterns-temp val))
           (while php-file-patterns-temp
             (add-to-list 'auto-mode-alist
                          (cons (car php-file-patterns-temp) 'zf-mode))
             (setq php-file-patterns-temp (cdr php-file-patterns-temp)))))
  :group 'zf-mode)

;; pulled and renamed from php-mode
(defcustom zf-default-face 'default
  "Default face in `zf-mode' buffers."
  :type 'face
  :group 'zf-mode)

(defcustom zf-mode-php-compile-on-save nil
  "Whether or not to run php-compile on files when they are
saved."
  :type 'boolean
  :group 'zf-mode)

(defcustom zf-mode-js-compile-on-save nil
  "Whether or not to run js-compile on files when they are
saved."
  :type 'boolean
  :group 'zf-mode)

(defcustom zf-mode-css-compile-on-save nil
  "Whether or not to run css-compile on files when they are
saved."
  :type 'boolean
  :group 'zf-mode)

(defcustom zf-mode-show-trailing-whitespace nil
  "Whether or not to turn show trailing whitespace."
  :type 'boolean
  :group 'zf-mode)

(defcustom zf-mode-delete-trailing-whitespace nil
  "Whether or not to trailing whitespace from files. If non-nil, the all
trailing whitespace will be deleted from files prior to saving."
  :type 'boolean
  :group 'zf-mode)

(defcustom zf-mode-protected-underscore nil
  "Whether or not protected properties and methods begin with an
underscore."
  :type 'boolean
  :group 'zf-mode)

(defcustom zf-mode-use-hyphens-in-viewscript-urls nil
  "Whether or not to use hyphens when generating viewscript urls
from controller names."
  :type 'boolean
  :group 'zf-mode)

(defcustom zf-mode-show-project-in-modeline nil
  "Whether or not to show the buffer's project in the modeline."
  :type 'boolean
  :group 'zf-mode)

;;; *********
;;; Variables
;;; *********
(defvar zf-mode-map nil
  "Keymap for zf-mode.")

;;; *********
;;; Constants
;;; *********
(defconst *zf-directory-structure* (list "application"
                                         "application/configs"
                                         "application/controllers"
                                         "application/forms"
                                         "application/layouts"
                                         "application/models"
                                         "application/models/DbTable"
                                         "application/modules"
                                         "application/resources"
                                         "application/views"
                                         "application/views/helpers"
                                         "application/views/scripts"
                                         "docs"
                                         "library"
                                         "public"
                                         "public/css"
                                         "public/js"
                                         "scripts"
                                         "tests")
  "A standard empty Zend Framework project directory structure.")

(defconst *zf-module-directory-structure* (list "configs"
                                                "controllers"
                                                "forms"
                                                "layouts"
                                                "models"
                                                "models/DbTable"
                                                "views"
                                                "views/helpers"
                                                "views/scripts")
  "A standard empty Zend Framework module directory structure.")

;;; **********************
;;; Zend Framework Project
;;; **********************
(defun zf-create-directory-structure (dir)
  "Creates a Zend Framework directory structure based on
*zf-directory-structure* in the current project directory. This
is used for starting a new project."
  (interactive "DRoot directory? ")
  (when (yes-or-no-p (concat "Are you sure you want to create a "
                             "Zend Framework directory structure in " dir "? "))
    (if (not (file-accessible-directory-p dir))
        (message "Cannot access the directory!")
      (dolist (directory *zf-directory-structure*)
        (make-directory (concat dir directory)))
      (message (concat "Project directory structure created in " dir))
      (when (yes-or-no-p "Add new directory to projects list? ")
        (php-project-add (read-string "Nickname? ") dir)))))

(defun zf-create-module (name)
  "Creates a module in the current project."
  (interactive "sModule name: ")
  (when (php-project-directory)
    (zf-create-module-structure
     (convert-standard-filename 
      (concat (php-project-directory) "application/modules/" name "/")))))

(defun zf-create-module-structure (dir)
  "Creates a Zend Framework module directory structure based on
*zf-module-directory-structure* in the current project directory.

This function is called by zf-create-module."
  (if (file-accessible-directory-p dir)
      (message "Module directory already exists!")
    (make-directory dir)
    (dolist (directory *zf-module-directory-structure*)
      (make-directory (concat dir directory)))
    (message (concat "Module structure created in " dir))))

;;; **************
;;; CODE INSERTION
;;; **************
(defun zf-insert-action (arg name)
  "Inserts a new action named NAME with class SCOPE into the
  buffer.  The prefix argument is currently ignored, but is left
  for possible future use."
  (interactive `(,current-prefix-arg
                 ,(read-string "Action name: ")))
  (apply 'php-insert-method
         (apply 'php-get-insert-method-arguments
                (append `(,(concat name "Action") public)
                        (unless (consp arg)
                          '(:staticp nil :abstractp nil :finalp nil))))))

(defun zf-insert-controller-completion-filterp (x)
  (string-match "^Zend_Controller_" x))

(defun zf-insert-constant (arg &optional name value)
  "Inserts a new constant named NAME with value VALUE into the
buffer.  The prefix argument is currently ignored, but is left
for possible future use."
  (interactive `(,current-prefix-arg))
  (apply 'php-insert-constant
         (apply 'php-get-insert-constant-arguments
                (append `(,name ,value)))))

(defun zf-insert-property (arg &optional name scope value)
  "Inserts a new property named NAME with class SCOPE into the
  buffer.  SCOPE may be one of 'public, `u', 'private, `i',
  'protected or `o'.  The default VALUE may be given.  Unless
  prefix argument is given, assume that the property is not
  static."
  (interactive `(,current-prefix-arg))
  (apply 'php-insert-property
         (apply 'php-get-insert-property-arguments
                (append `(,name ,scope ,value)
                        (unless (consp arg) '(:staticp nil))))))

(defun zf-insert-method (arg &optional name scope)
  "Inserts a new method named NAME with class SCOPE into the
  buffer.  SCOPE may be one of 'public, `u', 'private, `i',
  'protected or `o'.  Unless prefix argument is given, assume
  that the method is neither static, abstract nor final."
  (interactive `(,current-prefix-arg))
  (apply 'php-insert-method
         (apply 'php-get-insert-method-arguments
                (append `(,name ,scope)
                        (unless (consp arg)
                          '(:staticp nil :abstractp nil :finalp nil))))))

(defun zf-insert-interface (arg &optional name)
  "Inserts a new interface named NAME into the buffer.  Unless
  prefix argument is given, assume that the interface doesn't
  extends any interfaces."
  (interactive `(,current-prefix-arg))
  (apply 'php-insert-interface
         (apply 'php-get-insert-interface-arguments
                (append `(,(when (stringp name) (upcase-initials name)))
                        (unless (consp arg)
                          '(:extends nil))))))

(defun zf-insert-class (arg &optional name)
  "Inserts a new class named NAME into the buffer.  Unless prefix
  argument is given, assume that the class doesn't implement any
  interfaces and is neither abstract nor final."
  (interactive `(,current-prefix-arg))
  (apply 'php-insert-class
         (apply 'php-get-insert-class-arguments
                (append `(,(when (stringp name) (upcase-initials name)))
                        (unless (consp arg)
                          '(:implements nil :finalp nil :abstractp nil))))))

(defun* zf-insert-bootstrap (arg &optional (name "Bootstrap")
                                 (module (zf-default-module-name) module-p))
  "Inserts a new bootstrap named NAME into the buffer.  If MODULE
  is given, the model name reflects the module given.  Defaults
  to extending Zend_Application_Bootstrap_Bootstrap.  Unless
  prefix argument is given, assume that the model doesn't
  implement any interfaces and is neither abstract nor final."
  (interactive `(,current-prefix-arg
                 ,(read-string "Bootstrap name: " "Bootstrap")
                 ,(zf-get-module-name)))
  (if (nil-or-blank name)
      (message "You must give the bootstrap a name!")
    (apply 'php-insert-class
           (apply 'php-get-insert-class-arguments
                  (append `(,(concat
                              (when (stringp module)
                                (concat (upcase-initials module) "_"))
                              (upcase-initials name))
                            :extends-default
                            ,(if (stringp module)
                                 "Zend_Application_Module_Bootstrap"
                               "Zend_Application_Bootstrap_Bootstrap"))
                          (unless (consp arg)
                            '(:implements nil :finalp nil :abstractp nil)))))))

(defun* zf-insert-controller (arg name &optional
                                  (module (zf-default-module-name) module-p))
  "Inserts a new controller named NAME into the buffer.  If
  MODULE is given, the model name reflects the module given.
  Defaults to extending Zend_Controller_Action.  Unless prefix
  argument is given, assume that the controller doesn't implement
  any interfaces and is neither abstract nor final."
  (interactive `(,current-prefix-arg
                 ,(read-string "Controller name: ")
                 ,(zf-get-module-name)))
  (if (nil-or-blank name)
      (message "You must give the controller a name!")
    (apply 'php-insert-class
           (apply 'php-get-insert-class-arguments
                  (append `(,(concat
                              (when (and (stringp module)
                                         (not
                                          (string= module
                                                   (zf-default-module-name))))
                                (concat (upcase-initials module) "_"))
                              (upcase-initials name) "Controller")
                            :extends-default "Zend_Controller_Action")
                          (unless (consp arg)
                            '(:implements nil :finalp nil :abstractp nil)))))))

(defun* zf-insert-model (arg name &optional
                             (module (zf-default-module-name) module-p))
  "Inserts a new model named NAME into the buffer.  If MODULE is
  given, the model name reflects the module given.  Unless prefix
  argument is given, assume that the model doesn't implement any
  interfaces and is neither abstract nor final."
  (interactive `(,current-prefix-arg
                 ,(read-string "Model name: ")
                 ,(zf-get-module-name)))
  (if (nil-or-blank name)
      (message "You must give the model a name!")
    (apply 'php-insert-class
           (apply 'php-get-insert-class-arguments
                  (append `(,(concat (upcase-initials (if (stringp module)
                                                          module
                                                        (zf-global-namespace)))
                                     "_Model_"
                                     (upcase-initials name)))
                          (unless (consp arg)
                            '(:implements nil :finalp nil :abstractp nil)))))))

(defun* zf-insert-dbtable-model (arg name &optional
                                     (module (zf-default-module-name) module-p))
  "Inserts a new DbTable model named NAME into the buffer.  If
  MODULE is given, the model name reflects the module given.
  Defaults to extending Zend_Db_Table_Abstract.  Unless prefix
  argument is given, assume that the DbTable model doesn't
  implement any interfaces and is neither abstract nor final."
  (interactive `(,current-prefix-arg
                 ,(read-string "DbTable Model name: ")
                 ,(zf-get-module-name)))
  (if (nil-or-blank name)
      (message "You must give the DbTable model a name!")
    (apply 'php-insert-class
           (apply 'php-get-insert-class-arguments
                  (append `(,(concat (upcase-initials (if (stringp module)
                                                          module
                                                        (zf-global-namespace)))
                                     "_Model_DbTable_"
                                     (upcase-initials name))
                            :extends-default "Zend_Db_Table_Abstract")
                          (unless (consp arg)
                            '(:implements nil :finalp nil :abstractp nil)))))))

(defun* zf-insert-form (arg name &optional
                            (module (zf-default-module-name) module-p))
  "Inserts a new form named NAME into the buffer.  If MODULE
  is given, the model name reflects the module given.  Defaults
  to extending Zend_Form.  Unless prefix argument is given,
  assume that the form doesn't implement any interfaces and is
  neither abstract nor final."
  (interactive `(,current-prefix-arg
                 ,(read-string "Form name: ")
                 ,(zf-get-module-name)))
  (if (nil-or-blank name)
      (message "You must give the form a name!")
    (apply 'php-insert-class
           (apply 'php-get-insert-class-arguments
                  (append `(,(concat (upcase-initials (if (stringp module)
                                                          module
                                                        (zf-global-namespace)))
                                     "_Form_"
                                     (upcase-initials name))
                            :extends-default "Zend_Form")
                          (unless (consp arg)
                            '(:implements nil :finalp nil :abstractp nil)))))))

(defun zf-insert-dump (&optional arg)
  "Inserts a Zend_Debug::dump() statement."
  (interactive "P")
  (insert "Zend_Debug::dump(")
  (save-excursion
    (insert ");")
    (when (consp arg)
      (newline-and-indent)
      (insert "die;"))))

(defun zf-insert-dump-and-die ()
  "Inserts a Zend_Debug::dump() statement with a die afterwards."
  (interactive)
  (zf-insert-dump t))

;;; ***********************
;;; File/directory creation
;;; ***********************
(defun zf-find-file (filename directory)
  (let ((file (convert-standard-filename (concat directory filename))))
    (when (file-exists-p file)
      (find-file file)
      t)))

(defun zf-name->autoload-spec (name &optional type)
  "Takes a PHP class/interface name, applies the autoloader
rules, and returns a cons of (directory . filename).  Optional
argument TYPE is not currently used, but is there in case the
autoloader ever treats classes and interfaces differently."
  (let* ((type (or type 'class))
         (type-string (symbol-name type))
         (parts (split-string name "_"))
         (name (first (last parts)))
         (filename (zf-build-filename type-string name))
         (directory (file-name-as-directory
                     (mapconcat 'identity (butlast parts) "/"))))
    `(,directory . ,filename)))

(defun* zf-get-class/interface-arguments (type &optional name directory &key
                                               (filename nil filename-p)
                                               filename-default)
  "Gather arguments for zf-{class,interface}."
  (let* ((type-string (symbol-name type))
         (name (or name (php-completion-read-class/interface type)))
         (check (if (nil-or-blank name)
                    (error (concat "Must give " type-string " a name!"))))
         (directory
          (or directory (read-directory-name "Directory: "
                                             (php-project-directory))))
         (normal-filename (or filename-default
                              (zf-build-filename type-string name)))
         (filename (if filename-p
                       filename
                     (read-string "File name: " normal-filename))))
    `(,name ,directory ,filename)))

(defun zf-class/interface (arg type &optional name directory filename)
  "Creates a new TYPE (either 'class or 'interface) named NAME in
the given DIRECTORY.  If ARG, ask for filename rather than
assuming the normal generation from TYPE name.  Passes ARG along
to `zf-insert-TYPE.'"
  (let* ((type-string (symbol-name type))
         (insert-func
          (symbol-function (intern (concat "zf-insert-" type-string)))))
    (when (functionp insert-func)
      (let* ((insert-args
              (apply 'zf-get-class/interface-arguments
                     (append `(,type ,name ,directory)
                             (if (consp arg)
                                 `(:filename-default ,filename)
                               `(:filename ,filename)))))
             (name (first insert-args))
             (directory (second insert-args))
             (filename (third insert-args)))
        (unless (zf-find-file filename directory)
          (when (php-create-new-file filename directory)
            (funcall insert-func arg (upcase-initials name))
            (write-file filename)))))))

(defun zf-interface (arg &optional name directory filename)
  "Creates a new interface file named NAME in the given DIRECTORY.
If ARG, ask for filename rather than assuming the normal
generation from interface name.  Passes ARG along to
`zf-insert-interface.'"
  (interactive "P")
  (zf-class/interface arg 'interface name directory filename))

(defun zf-class (arg &optional name directory filename)
  "Creates a new class file named NAME in the given DIRECTORY.
If ARG, ask for filename rather than assuming the normal
generation from class name.  Passes ARG along to
`zf-insert-class.'"
  (interactive "P")
  (zf-class/interface arg 'class name directory filename))

(defun zf-library-class/interface (arg type &optional name directory)
  "Creates a new TYPE file named NAME in the project library
directory.  It will derive the final directory using normal
autoloader rules.  If ARG, prompt for this directory, and pass
ARG on to zf-TYPE."
  (let* ((type-string (symbol-name type))
         (create-func
          (symbol-function (intern (concat "zf-" type-string)))))
    (when (functionp create-func)
      (let* ((name (or name (php-completion-read-class/interface type)))
             (autoload-spec (zf-name->autoload-spec name type))
             (filename (rest autoload-spec))
             (computed-directory
              (concat (php-project-library-directory) (first autoload-spec)))
             (create-args
              `(,arg ,name
                     ,(if (consp arg)
                          (read-directory-name "Directory: " computed-directory)
                        computed-directory)
                     ,filename)))
        (apply create-func create-args)))))

(defun zf-library-interface (arg &optional name directory)
  "Creates a new interface file named NAME in the project library
directory.  It will derive the final directory using normal
autoloader rules.  If ARG, prompt for this directory, and pass
ARG on to zf-interface."
  (interactive "P")
  (zf-library-class/interface arg 'interface  name directory))

(defun zf-library-class (arg &optional name directory)
  "Creates a new class file named NAME in the project library
directory.  It will derive the final directory using normal
autoloader rules.  If ARG, prompt for this directory, and pass
ARG on to zf-class."
  (interactive "P")
  (zf-library-class/interface arg 'class  name directory))

(defun* zf-controller (arg name &optional
                           (module (zf-default-module-name) module-p)
                           project)
  "Creates a new controller file named NAME inside MODULE (if
  given). Defaults to extending Zend_Controller_Action.  Unless
  prefix argument is given, assume that the model doesn't
  implement any interfaces and is neither abstract nor final."
  (interactive (let* ((module-cons (zf-get-module-name))
                      (module (car module-cons))
                      (project (cdr module-cons))
                      (name (zf-get-controller-name module project)))
                 `(,current-prefix-arg ,name ,module ,project)))
  (let ((project (or project (php-project-buffer-project))))
    (if (nil-or-blank name)
        (message "Must give the controller a name!"))
    (setq directory (zf-get-directory "controller" module nil nil project))
    (setq filename (zf-build-filename "controller" name))
    (unless (zf-find-file filename directory)
      (when (php-create-new-file filename directory)
        (zf-insert-controller arg (upcase-initials name) module)
        (write-file filename)))))

(defun* zf-model (arg name &optional (module (zf-default-module-name) module-p)
                      project)
  "Creates a new model file named NAME inside MODULE (if given).
Unless prefix argument is given, assume that the model doesn't
implement any interfaces and is neither abstract nor final."
  (interactive (let* ((module-cons (zf-get-module-name))
                      (module (car module-cons))
                      (project (cdr module-cons))
                      (name (zf-get-model-name module project)))
                 `(,current-prefix-arg ,name ,module ,project)))
  (let ((project (or project (php-project-buffer-project))))
    (if (nil-or-blank name)
        (message "Must give the model a name!")
      (setq directory (zf-get-directory "model" module nil nil project))
      (setq filename (zf-build-filename "model" name))
      (unless (zf-find-file filename directory)
        (when (php-create-new-file filename directory)
          (zf-insert-model arg (upcase-initials name) module)
          (write-file filename))))))

(defun* zf-dbtable-model (arg name &optional
                              (module (zf-default-module-name) module-p)
                              project)
  "Creates a new DbTable model file named NAME inside MODULE (if
  given). Defaults to extending Zend_Db_Table_Abstract.  Unless
  prefix argument is given, assume that the model doesn't
  implement any interfaces and is neither abstract nor final."
  (interactive (let* ((module-cons (zf-get-module-name))
                      (module (car module-cons))
                      (project (cdr module-cons))
                      (name (zf-get-dbtable-model-name module project)))
                 `(,current-prefix-arg ,name ,module ,project)))
  (let ((project (or project (php-project-buffer-project))))
    (if (nil-or-blank name)
        (message "Must give DbTable model a name!")
      (setq directory (zf-get-directory "dbtable" module nil nil project))
      (setq filename (zf-build-filename "model" name))
      (unless (zf-find-file filename directory)
        (when (php-create-new-file filename directory)
          (zf-insert-dbtable-model arg (upcase-initials name) module)
          (write-file filename))))))

(defun* zf-form (arg name &optional
                     (module (zf-default-module-name) module-p) project)
  "Creates a new Zend form file named NAME inside MODULE (if
  given). Defaults to extending Zend_Form.  Unless prefix
  argument is given, assume that the model doesn't implement any
  interfaces and is neither abstract nor final."
  (interactive (let* ((module-cons (zf-get-module-name))
                      (module (car module-cons))
                      (project (cdr module-cons))
                      (name (zf-get-form-name module project)))
                 `(,current-prefix-arg ,name ,module ,project)))
  (let ((project (or project (php-project-buffer-project))))
    (if (nil-or-blank name)
        (message "Must give form a name!")
      (setq directory (zf-get-directory "form" module nil nil project))
      (setq filename (zf-build-filename "form" name))
      (unless (zf-find-file filename directory)
        (when (php-create-new-file filename directory)
          (zf-insert-form arg (upcase-initials name) module)
          (write-file filename))))))

(defun* zf-bootstrap (arg &optional
                          (name "Bootstrap")
                          (module (zf-default-module-name) module-p))
  "Creates a new bootstrap file named NAME (if given) inside
  MODULE (if given). NAME defaults to Bootstrap.  Defaults to
  extending Zend_Application_Bootstrap_Bootstrap (or if MODULE
  given, Zend_Application_Module_Bootstrap).  Unless prefix
  argument is given, assume the name is Bootstrap and assume that
  the model doesn't implement any interfaces and is neither
  abstract nor final."
  (interactive `(,current-prefix-arg
                 ,(if current-prefix-arg
                      (read-string "Bootstrap name: ")
                    "Bootstrap")
                 ,(zf-get-module-name)))
  (let ((project (when (consp module) (cdr module)))
        (module (if (consp module) (car module) module)))
    (setq directory (zf-get-directory "bootstrap" module nil nil project))
    (setq filename (zf-build-filename "bootstrap" name))
    (unless (zf-find-file filename directory)
      (when (php-create-new-file filename directory)
        (zf-insert-bootstrap arg (upcase-initials name) module)
        (write-file filename)))))

(defun zf-view-script (name module controller url &optional project)
  "Creates a new view script file named NAME for CONTROLLER
inside NODULE.  Completion for viewscript name will be provided
for travelling to existing viewscripts.  Optionally lock down to
URL."
  (interactive (let* ((module-cons (zf-get-module-name))
                      (module (car module-cons))
                      (project (cdr module-cons))
                      (controller (zf-get-controller-name module project))
                      (name (zf-get-viewscript-name controller module nil
                                                    project))
                      (name-parts (split-string name "/"))
                      (name (if (> (length name-parts) 1)
                                (second name-parts)
                              (first name-parts)))
                      (url (when (> (length name-parts) 1) (first name-parts))))
                 `(,name ,module ,controller ,url ,project)))
  (let ((project (or project (php-project-buffer-project))))
    (if (nil-or-blank name)
        (message "Must give the view script a name!")
      (if (nil-or-blank controller)
          (message "Must give a controller!")
        (setq directory (zf-get-directory "viewscript" module controller url
                                          project))
        (setq filename (zf-build-filename "viewscript" name))
        (unless (zf-find-file filename directory)
          (when (php-create-new-file filename directory)
            (delete-char -1)
            (write-file filename)))))))

(defun zf-view-script-via-controller-action (arg)
  "Opens the file for the view script where the cursor is located."
  (interactive "P")
  (when (setq action-info (zf-get-view-script-info))
    (let* ((name (first action-info))
           (module (second action-info))
           (controller (third action-info))
           (url (when (consp arg) (zf-get-viewscript-url module controller))))
      (zf-view-script name module controller url))))

(defun* zf-config (name type &optional
                        (module (zf-default-module-name) module-p)
                        project)
  "Creates a new config file named name, in 'module' if
given. Type is either XML or INI."
  (interactive (let* ((module-cons (zf-get-module-name))
                      (module (car module-cons))
                      (project (cdr module-cons))
                      (name (zf-get-config-name module project))
                      (type (completing-read "XML or INI (x/i): " '("i" "x")
                                             nil t)))
                 `(,name ,type ,module ,project)))
  (let ((project (or project (php-project-buffer-project))))
    (if (nil-or-blank name)
        (message "Must give the config file a name!")
      (if (or (nil-or-blank type)
              (and (not (string= type "X"))
                   (not (string= type "x"))
                   (not (string= type "I"))
                   (not (string= type "i"))))
          (message "Must select either 'x' or 'i' for the type!")
        (setq directory (zf-get-directory "config" module nil nil project))
        (setq filename (zf-build-filename "config" name module type))
        (unless (zf-find-file filename directory)
          (when (php-create-new-file filename directory t)
            (write-file filename)))))))

;;; *****************
;;; Utility functions
;;; *****************
(defun zf-get-view-script-info ()
  "Returns the action, controller, and modules names for the view
script where the point is located."
  (when (and buffer-file-name
         (string= (substring buffer-file-name -14 nil) "Controller.php"))
    (let ((module-name (zf-get-current-module-name))
          (controller-name
           (substring (car (last (split-string buffer-file-name "/"))) 0 -14))
          (parse (php-parse-current 'method)))
      (when (php-parse-p parse)
        (let ((name (rest (assoc 'name parse))))
          (when (string= "Action" (substring name -6))
            (let ((action-name (substring name 0 -6)))
              `(,action-name ,module-name ,controller-name))))))))

(defun* zf-get-directory (subject &optional
                                  (module (zf-default-module-name) module-p)
                                  controller url project)
  "Creates and returns the appropriate directory for a given Zend
Framework element (controller, model, etc). The third and fourth
arguments are used when getting the directory for a view script.
If the fourth is omitted the default conversion specified in the
defcustom `zf-mode-use-hyphens-in-viewscript-urls' will be used."
  (if (nil-or-blank subject)
      (message "Must give a subject!")
    (let ((dirappend (if (string= "library" subject)
                         "/library"
                       (concat "/application"
                               (when (stringp module)
                                 (concat "/modules/" module)))))
          (dirtype (cond
                    ((string= subject "controller") "controllers")
                    ((string= subject "model") "models")
                    ((string= subject "dbtable") "DbTable")
                    ((string= subject "form") "forms")
                    ((string= subject "viewscript")
                     (let ((controller
                            (if (stringp url)
                                url
                              (zf-get-default-viewscript-url controller))))
                       (concat "views/scripts/" controller)))
                    ((string= subject "config") "configs"))))
      (concat (php-project-directory project) dirappend "/"
              (when (stringp dirtype)
                (concat dirtype "/"))))))

(defun zf-build-filename (subject &optional name module type)
  "Creates and returns the appropriate file name for a given Zend
Framework element (controller, model, etc."
  (if (nil-or-blank subject)
      (message "Must profile a subject!")
    (if (and (nil-or-blank name)
             (not (string= subject "bootstrap")))
        (message "Must provide a name for the file!")
      (cond
       ((or (string= subject "interface")
            (string= subject "class")
            (string= subject "model")
            (string= subject "form"))
        (concat (upcase-initials name) ".php"))
       ((string= subject "controller")
        (concat (upcase-initials name) "Controller.php"))
       ((string= subject "bootstrap")
        (concat (if (nil-or-blank name)
                    "Bootstrap"
                  (upcase-initials name)) ".php"))
       ((string= subject "viewscript")
        (concat name ".phtml"))
       ((string= subject "config")
        (if (or (string= type "X")
                (string= type "x"))
            (concat name ".xml")
          (when (or (string= type "I")
                    (string= type "i"))
            (concat name ".ini"))))))))

(defun zf-get-application-config ()
  "Opens the application config file for the project."
  (convert-standard-filename
   (concat (php-project-directory) "/application/configs/application.ini")))

(defun zf-open-application-config ()
  "Opens the phpunit application file for the project."
  (interactive)
  (find-file (zf-get-application-config)))

(defun zf-global-namespace ()
  "Returns the value of the global namespace for this project."
  (let ((ns (php-project-zend-global-namespace)))
    (if (nil-or-blank ns) "Default" ns)))

(defun zf-default-module-name ()
  "Returns the default module name as set in php-project-list,
defaulting to nil."
  (let ((dm (php-project-zend-default-module-name)))
    (if (nil-or-blank dm) nil dm)))

(defun zf-module-list ()
  "Returns a list of modules for the current ZF project."
  (let* ((project (or (php-project-buffer-project)
                      (php-project-ask-for-project)))
         (modules-directory
          (expand-file-name (concat (php-project-directory project)
                                    "/application/modules"))))
    (cons (append `("*none*")
                  (when (file-exists-p modules-directory)
                    (remove-if (lambda (x)
                                 (string= "." (substring x 0 1)))
                               (directory-files modules-directory))))
          project)))

(defun zf-get-current-module-name ()
  "Get the module name of the current file."
  (let ((fn (buffer-file-name)))
    (when (stringp fn)
      (let ((parts (split-string fn "/")))
        (catch 'found
          (dotimes (i (length parts))
            (when (string= (elt parts i) "modules")
              (throw 'found (elt parts (1+ i))))))))))

(defun zf-get-module-name ()
  "Read in a ZF module name."
  (let* ((module-cons (zf-module-list))
         (module-list (car module-cons))
         (project (cdr module-cons))
         (default-module (zf-get-current-module-name))
         (default-module (or default-module "*none*"))
         (name (completing-read (concat "Module (" default-module "): ")
                                  module-list nil t nil nil default-module))
         (name (unless (string= name "*none*") name)))
    (cons name project)))

(defun zf-model-list (&optional module project)
  "Returns a list of models for the current ZF project.  MODULE
defaults to the global module."
  (let ((models-directory
         (expand-file-name (zf-get-directory "model" module nil nil project))))
    (when (file-exists-p models-directory)
      (mapcar (lambda (x)
                (substring x 0 -4))
              (remove-if-not
               (lambda (x)
                 (string= ".php"
                          (substring x -4)))
               (remove-if (lambda (x)
                            (string= "." (substring x 0 1)))
                          (directory-files models-directory)))))))

(defun zf-get-model-name (&optional module project)
  "Read in a ZF model name.  Autocompletion from MODULE is
available.  MODULE defaults to the global module."
  (let ((model-list (zf-model-list module project)))
    (completing-read "Model: " model-list)))

(defun zf-dbtable-model-list (&optional module project)
  "Returns a list of DbTable models for the current ZF project.  MODULE
defaults to the global module."
  (let ((dbtable-models-directory
         (expand-file-name (zf-get-directory "dbtable" module nil nil
                                             project))))
    (when (file-exists-p dbtable-models-directory)
      (mapcar (lambda (x)
                (substring x 0 -4))
              (remove-if-not
               (lambda (x)
                 (string= ".php"
                          (substring x -4)))
               (remove-if (lambda (x)
                            (string= "." (substring x 0 1)))
                          (directory-files dbtable-models-directory)))))))

(defun zf-get-dbtable-model-name (&optional module project)
  "Read in a ZF DbTable model name.  Autocompletion from MODULE is
available.  MODULE defaults to the global module."
  (let ((dbtable-model-list (zf-dbtable-model-list module project)))
    (completing-read "DbTable Model: " dbtable-model-list)))

(defun zf-form-list (&optional module project)
  "Returns a list of DbTable models for the current ZF project.
MODULE defaults to the global module."
  (let ((forms-directory
         (expand-file-name (zf-get-directory "form" module nil nil project))))
    (when (file-exists-p forms-directory)
      (mapcar (lambda (x)
                (substring x 0 -4))
              (remove-if-not
               (lambda (x)
                 (string= ".php"
                          (substring x -4)))
               (remove-if (lambda (x)
                            (string= "." (substring x 0 1)))
                          (directory-files forms-directory)))))))

(defun zf-get-form-name (&optional module project)
  "Read in a ZF DbTable model name.  Autocompletion from MODULE is
available.  MODULE defaults to the global module."
  (let ((form-list (zf-form-list module project)))
    (completing-read "Form: " form-list)))

(defun zf-config-list (&optional module project)
  "Returns a list of DbTable models for the current ZF project.
MODULE defaults to the global module."
  (let ((configs-directory
         (expand-file-name (zf-get-directory "config" module nil nil project))))
    (when (file-exists-p configs-directory)
      (mapcar (lambda (x)
                (substring x 0 -4))
              (remove-if-not
               (lambda (x)
                 (string-match "\\.\\(xml\\|ini\\)$" x))
               (remove-if (lambda (x)
                            (string= "." (substring x 0 1)))
                          (directory-files configs-directory)))))))

(defun zf-get-config-name (&optional module project)
  "Read in a ZF DbTable model name.  Autocompletion from MODULE is
available.  MODULE defaults to the global module."
  (let ((config-list (zf-config-list module project)))
    (completing-read "Config: " config-list)))

(defun zf-controller-list (&optional module project)
  "Returns a list of controllers for the current ZF project.
MODULE defaults to the global module."
  (let ((controllers-directory
         (expand-file-name (zf-get-directory "controller" module nil nil
                                             project))))
    (when (file-exists-p controllers-directory)
      (mapcar (lambda (x)
                (substring x 0 -14))
              (remove-if-not
               (lambda (x)
                 (string= "Controller.php"
                          (substring x -14)))
               (remove-if (lambda (x)
                            (string= "." (substring x 0 1)))
                          (directory-files controllers-directory)))))))

(defun zf-get-controller-name (&optional module project)
  "Read in a ZF controller name.  Autocompletion from MODULE is
available.  MODULE defaults to the global module."
  (let ((controller-list (zf-controller-list module project)))
    (completing-read "Controller: " controller-list)))

(defun zf-get-default-viewscript-url (controller)
  "Retrun the default viewscript url for a controller.  Respects
`zf-mode-use-hyphens-in-viewscript-urls'."
  (if zf-mode-use-hyphens-in-viewscript-urls
      (camelcase->hyphenated controller)
    (downcase controller)))

(defun zf-get-viewscripts-dir (module controller &optional project)
  "Return the top-level viewscripts director for MODULE and
CONTROLLER."
  (let ((dir (zf-get-directory "viewscript" module controller nil project)))
    (when (file-exists-p dir)
      (file-name-directory (substring (expand-file-name dir) 0 -1)))))

(defun zf-get-viewscript-dirs (module controller &optional url)
  "Get a list of viewscript directories that can be mapped from
CONTROLLER of MODULE.  Optionally lock return down to a certain
url."
  (when (stringp controller)
    (let* ((viewscripts-directory (zf-get-viewscripts-dir module controller)))
      (when viewscripts-directory
        (remove-if (lambda (x)
                     (or (string= "." (substring x 0 1))
                         (and (stringp url) (not (string= x url)))
                         (not (string=
                               (downcase controller)
                               (replace-regexp-in-string "-" ""
                                                         (downcase x))))))
                   (directory-files viewscripts-directory))))))

(defun zf-get-viewscript-url (module controller)
  "Read in a ZF viewscript URL.  Autocompletion for CONTROLLER of
MODULE is provided."
  (let ((url-list (zf-get-viewscript-dirs module controller)))
    (completing-read "URL: " url-list)))

(defun zf-viewscript-list (controller &optional module url project)
  "Returns a list of viewscripts for CONTROLLER in the current ZF project.
MODULE defaults to the global module.  If multiple possible
viewscript directories exist the completion list will include
them all with directories prepended, as well as a non-directory
version for the default."
  (let* ((viewscripts-directory (zf-get-viewscripts-dir module controller
                                                        project))
         (default-url (zf-get-default-viewscript-url controller)))
    (when viewscripts-directory
      (remove-if-not
       'identity
       (apply
        'append
        (apply 'append
               (mapcar (lambda (x)
                         (mapcar (lambda (y)
                                   (let* ((name (substring y 0 -6))
                                          (full-path (concat x "/" name)))
                                     `(,(when (string= x default-url) name)
                                       ,full-path)))
                                 (remove-if
                                  (lambda (x)
                                    (or (string= "." (substring x 0 1))
                                        (not (string= ".phtml"
                                                      (substring x -6)))))
                                  (directory-files
                                   (concat viewscripts-directory x)))))
                       (zf-get-viewscript-dirs module controller))))))))

(defun zf-get-viewscript-name (controller &optional module url project)
  "Read in a ZF viewscript name.  Autocompletion from CONTROLLER
of MODULE is available.  MODULE defaults to the global
module. URL may be specified if it differs from the normal
mapping (controlled by
`zf-mode-use-hyphens-in-viewscript-urls')."
  (let ((viewscript-list (zf-viewscript-list controller module url project)))
    (completing-read "Viewscript: " viewscript-list)))

(defun zf-mode-delete-all-trailing-whitespace ()
  "This is just a wrapper around ``delete-trailing-whitespace''
that checks the value of zf-mode-delete-trailing-whitespace
first."
  (when zf-mode-delete-trailing-whitespace
    (delete-trailing-whitespace)))

;; *************
;;; zf-mode Setup
;;; *************
(defun zf-mode-compile-on-save ()
  (when zf-mode-php-compile-on-save
    (php-compile-if-php))
  (when zf-mode-js-compile-on-save
    (js-compile-if-js))
  (when zf-mode-css-compile-on-save
    (css-compile-if-css)))

(defun zf-mode-setup ()
  "Prepares emacs for zf-mode."
  (add-to-list 'load-path
	       (convert-standard-filename
		(concat (file-name-directory (find-lisp-object-file-name
					      'zf-mode 'function))
			"bundled/")))
  ;; (require 'php-mode)

  ;; (add-hook 'php-mode-hook 'zf-mode-hook)
  ;; (add-hook 'js-mode-hook 'zf-mode-hook)
  ;; (add-hook 'css-mode-hook 'zf-mode-hook)
  ;; (add-hook 'conf-mode-hook 'zf-mode-hook)
)

(defun zf-mode-customize ()
  "Opens the customize buffer for zf-mode."
  (interactive)
  (customize-group "zf-mode"))

(defun zf-mode-reload (&optional only-revert-source)
  "This function reverts all file buffers and reloads the zf-mode lisp files.
Optional argument `only-revert-source` tells the function to only revert open
zf-mode source code file buffers. "
  (interactive "P")
  (save-some-buffers)
  (when (fboundp 'ert-delete-all-tests) (ert-delete-all-tests))
  (dolist (subdir '("" "tests/"))
    (dolist (file (remove-if
                   (lambda (x) (string-match "/\\." x))
                   (file-expand-wildcards
                    (concat (file-name-directory (find-lisp-object-file-name 
                                                  'zf-mode 'function)) subdir
                            "*.el"))))
      (load-file file)))
  (let ((rb (current-buffer)))
    (dolist (b (buffer-list))
      (when (and (buffer-file-name b)
                 (or (not only-revert-source)
                     (string-match
                      (file-name-directory (find-lisp-object-file-name 
                                            'zf-mode 'function))
                      (file-name-directory (buffer-file-name b)))))
        (switch-to-buffer b)
        (when (or (not only-revert-source) (eq major-mode 'emacs-lisp-mode))
          (revert-buffer t t))))
    (switch-to-buffer rb))
  (zf-define-keys)
  (zf-define-menu))

(defun zf-mode-source-line-count ()
  "This function returns the number of lines of code that make up zf-mode, not
including unittests or bundled packages."
  (interactive)
  (shell-command
   (concat "less " 
           (file-name-directory (find-lisp-object-file-name 'zf-mode 'function))
           "*.el | grep -v '^[[:space:]]*;\\\|^[^[:space:]]*$' | wc -l"))
  (let ((b (current-buffer))
        count)
    (switch-to-buffer (get-buffer "*Shell Command Output*"))
    (setq count (replace-regexp-in-string
                 "[ \n]" ""
                 (buffer-substring-no-properties (point-min) (point-max))))
    (switch-to-buffer b)
    (message count)))

;;; ***************
;;; Keymap and Menu
;;; ***************
(defun zf-define-keys ()
  (define-key zf-mode-map "\C-cba" 'php-format-break-at-assignment-operators)
  (define-key zf-mode-map "\C-cbo" 'php-format-break-at-operators)
  (define-key zf-mode-map "\C-cbs" 'php-format-break-statement)
  (define-key zf-mode-map "\C-cb\C-s" 'php-format-break-string)
  (define-key zf-mode-map "\C-cb." 'php-format-break-at-concats)
  (define-key zf-mode-map "\C-cb," 'php-format-break-at-commas)
  (define-key zf-mode-map "\C-cb>" 'php-format-break-at-arrows)
  (define-key zf-mode-map "\C-cb\\" 'php-format-break-current)
  (define-key zf-mode-map "\C-cb\C-\\" 'php-format-break-class/interface)
  (define-key zf-mode-map "\C-cC" 'php-format-clean-up-script)
  (define-key zf-mode-map "\C-cd" 'php-change-string<->doc)
  (define-key zf-mode-map "\C-ch" 'php-change-bare-html<->heredoc)
  (define-key zf-mode-map "\C-ci" 'php-remove-this-concat)
  (define-key zf-mode-map "\C-c\M-i" 'php-implode-concats-in-statement)
  (define-key zf-mode-map "\C-cl" 'align-on)
  (define-key zf-mode-map "\C-cs" 'php-combine-scripts)
  (define-key zf-mode-map "\C-c'" 'php-change-string-quotes)
  (define-key zf-mode-map "\C-c\M-'" 'zf-force-string-quotes-statement)
  (define-key zf-mode-map "\C-c(" 'php-find-current-sexp-begin)
  (define-key zf-mode-map "\C-c)" 'php-find-current-sexp-end)
  (define-key zf-mode-map "\C-c<" 'php-goto-start-of-script/html)
  (define-key zf-mode-map "\C-c>" 'php-goto-end-of-script/html)
  (define-key zf-mode-map [(control return)] 'php-format-break-statement)
  (define-key zf-mode-map "\C-cf" 'php-completion-lookup-at-point->message)
  (define-key zf-mode-map "\C-cza" 'zf-insert-action)
  (define-key zf-mode-map "\C-czb" 'zf-bootstrap)
  (define-key zf-mode-map "\C-czc" 'zf-controller)
  (define-key zf-mode-map "\C-czC" 'zf-insert-class)
  (define-key zf-mode-map "\C-czd" 'php-project-dired-directory)
  (define-key zf-mode-map "\C-czD" 'zf-create-directory-structure)
  (define-key zf-mode-map "\C-cze" 'zf-insert-method)
  (define-key zf-mode-map "\C-czE" 'php-modify-thing)
  (define-key zf-mode-map "\C-czf" 'zf-form)
  (define-key zf-mode-map "\C-czh" 'php-mark-current)
  (define-key zf-mode-map "\C-czi" 'zf-config)
  (define-key zf-mode-map "\C-czI" 'zf-interface)
  (define-key zf-mode-map "\C-czj" 'php-jump-to-thing)
  (define-key zf-mode-map "\C-czk" 'php-kill-current)
  (define-key zf-mode-map "\C-cz\M-k" 'php-kill-sexp-innard)
  (define-key zf-mode-map "\C-czl" 'zf-class)
  (define-key zf-mode-map "\C-czLc" 'zf-library-class)
  (define-key zf-mode-map "\C-czLi" 'zf-library-interface)
  (define-key zf-mode-map "\C-czm" 'zf-model)
  (define-key zf-mode-map "\C-czM" 'zf-create-module)
  (define-key zf-mode-map "\C-czn" 'zf-dbtable-model)
  (define-key zf-mode-map "\C-czN" 'zf-insert-interface)
  (define-key zf-mode-map "\C-czoi" 'zf-open-application-config)
  (define-key zf-mode-map "\C-czoo" 'php-project-open)
  (define-key zf-mode-map "\C-czou" 'php-project-open-phpunit-config)
  (define-key zf-mode-map "\C-czO" 'zf-insert-constant)
  (define-key zf-mode-map "\C-czpk" 'php-project-close)
  (define-key zf-mode-map "\C-czpd" 'php-project-show-directory)
  (define-key zf-mode-map "\C-czpvd" 'php-project-vc-dir)
  (define-key zf-mode-map "\C-czr" 'zf-insert-property)
  (define-key zf-mode-map "\C-czRm" 'php-refactor-move-thing-to-buffer)
  (define-key zf-mode-map "\C-czRM"
    'php-refactor-move-all-things-in-class/interface-to-buffer)
  (define-key zf-mode-map "\C-czRr" 'php-rearrange-current)
  (define-key zf-mode-map "\C-czRR" 'php-rearrange-innards)
  (define-key zf-mode-map "\C-cztt" 'php-compile)
  (define-key zf-mode-map "\C-cztT" 'php-test-full-project)
  (define-key zf-mode-map "\C-cztc" 'phpcs)
  (define-key zf-mode-map "\C-cztg" 'php-compile-again)
  (define-key zf-mode-map "\C-cztm" 'phpmd)
  (define-key zf-mode-map "\C-cztu" 'phpunit)
  (define-key zf-mode-map "\C-cztU" 'phpunit-single-test)
  (define-key zf-mode-map "\C-czv" 'zf-view-script-via-controller-action)
  (define-key zf-mode-map "\C-czV" 'zf-view-script)
  (define-key zf-mode-map "\C-czy" 'php-yank)
  (define-key zf-mode-map "\C-czz" 'zf-insert-dump)
  (define-key zf-mode-map "\C-czZ" 'zf-insert-dump-and-die)
  (define-key zf-mode-map "\C-cz;" 'php-comment-current)
  (define-key zf-mode-map [(control meta ?\>)] 'php-kill-chain-link)
  (define-key zf-mode-map [tab] 'indent-or-expand)
  (define-key zf-mode-map [return] 'php-auto-fill)
  (define-key zf-mode-map "\M-." 'php-find-tag)
  (define-key zf-mode-map "\M-[" 'php-jump-to-previous-thing)
  (define-key zf-mode-map "\M-]" 'php-jump-to-next-thing)
  (define-key zf-mode-map "\M-;" 'php-comment-dwim)
  (define-key zf-mode-map [(super ?\[)] 'php-hide-innards)
  (define-key zf-mode-map [(super ?\{)] 'php-hide-class/interface-doc-blocks)
  (define-key zf-mode-map [(super ?\])] 'hs-show-all)
  (define-key zf-mode-map [(super ?\})] 'php-show-class/interface-doc-blocks)
  ;; pulled from php-mode
  (define-key zf-mode-map "\C-c\C-f" 'php-search-documentation)
  (define-key zf-mode-map "\C-c\C-m" 'php-browse-manual)
  (define-key zf-mode-map [(control .)] 'php-show-arglist)
)

(defun zf-define-menu ()
  (define-key zf-mode-map
    [menu-bar zf]
    (cons "ZF" (make-sparse-keymap "ZF")))

  (define-key (lookup-key zf-mode-map [menu-bar zf])
    [create]
    (cons "Create/Open" (make-sparse-keymap "Create")))
  (define-key (lookup-key zf-mode-map [menu-bar zf create])
    [module]
    '("Module" . zf-create-module))
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [controller]
    '("Controller" . zf-controller)
    'module)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [view-via]
    '("View Via Action" . zf-view-script-via-controller-action)
    'controller)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [view]
    '("View" . zf-view-script)
    'view-via)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [sep1]
    '("--single-line")
    'view)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [model]
    '("Model" . zf-model)
    'sep1)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [dbtable]
    '("Database Table Model" . zf-dbtable-model)
    'model)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [form]
    '("Form" . zf-form)
    'dbtable)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [sep2]
    '("--single-line")
    'form)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [bootstrap]
    '("Bootstrap" . zf-bootstrap)
    'sep2)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [class-file]
    '("Class" . zf-class)
    'bootstrap)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [interface-file]
    '("Interface" . zf-interface)
    'class-file)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [sep4]
    '("--single-line")
    'interface-file)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [config]
    '("Config" . zf-config)
    'sep4)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [open-config]
    '("Open application.ini" . zf-open-application-config)
    'config)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [sep3]
    '("--single-line")
    'open-config)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf create])
    [function]
    '("Jump to Function" . php-jump-to-function)
    'sep3)

  (define-key-after (lookup-key zf-mode-map [menu-bar zf])
    [insert]
    (cons "Insert" (make-sparse-keymap "Insert"))
    'create)
  (define-key (lookup-key zf-mode-map [menu-bar zf insert])
    [action]
    '("Action" . zf-insert-action))
  (define-key-after (lookup-key zf-mode-map [menu-bar zf insert])
    [method]
    '("Method" . zf-insert-method)
    'action)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf insert])
    [constant]
    '("Constant" . zf-insert-constant)
    'method)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf insert])
    [property]
    '("Property" . zf-insert-property)
    'constant)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf insert])
    [class]
    '("Class" . zf-insert-class)
    'property)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf insert])
    [interface]
    '("Interface" . zf-insert-interface)
    'class)

  (define-key-after (lookup-key zf-mode-map [menu-bar zf insert])
    [library]
    `("Library" . ,(make-sparse-keymap "Library"))
    'interface)
  (define-key (lookup-key zf-mode-map [menu-bar zf insert library])
    [library-class]
    '("Class" . zf-library-class))
  (define-key-after (lookup-key zf-mode-map [menu-bar zf insert library])
    [library-interface]
    '("Interface" . zf-library-interface)
    'library-class)

  (define-key-after (lookup-key zf-mode-map [menu-bar zf insert])
    [dump]
    '("Dump" . zf-insert-dump)
    'library)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf insert])
    [dump-die]
    '("Dump and Die" . zf-insert-dump-and-die)
    'dump)

  (define-key-after (lookup-key zf-mode-map [menu-bar zf])
    [edit]
    (cons "Edit" (make-sparse-keymap "Edit"))
    'insert)

  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [refactor]
    `("Refactor" . ,(make-sparse-keymap "Refactor"))
    'edit)
  (define-key (lookup-key zf-mode-map [menu-bar zf edit refactor])
    [move-thing]
    '("Move Thing to Buffer" . php-refactor-move-thing-to-buffer))
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit refactor])
    [move-all-things]
    '("Move All Things in Class/Interface to Buffer" .
      php-refactor-move-all-things-in-class/interface-to-buffer)
    'move-thing)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit refactor])
    [rearrange-current]
    '("Rearrange Current Thing" . php-rearrange-current)
    'move-all-things)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit refactor])
    [rearrange-innards]
    '("Rearrange Innards" . php-rearrange-innards)
    'rearrange-current)

  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [break]
    `("Break" . ,(make-sparse-keymap "Break"))
    'refactor)
  (define-key (lookup-key zf-mode-map [menu-bar zf edit break])
    [break-current]
    '("Current Thing" . php-format-break-current))
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit break])
    [break-class/interface]
    '("Current Class/Interface" . php-format-break-class/interface)
    'break-current)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit break])
    [break-statement]
    '("Statement" . php-format-break-statement) 'break-class/interface)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit break])
    [break-string]
    '("String" . php-format-break-string) 'break-statement)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit break])
    [break-at-assignment-operators]
    '("At Assignment Operators" . php-format-break-at-assignment-operators)
    'break-string)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit break])
    [break-at-operators]
    '("At Operators" . php-format-break-at-operators)
    'break-at-assignment-operators)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit break])
    [break-at-concats]
    '("At Concats" . php-format-break-at-concats) 'break-at-operators)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit break])
    [break-at-commas]
    '("At Commas" . php-format-break-at-commas) 'break-at-concats)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit break])
    [break-at-arrows]
    '("At Arrows" . php-format-break-at-arrows) 'break-at-commas)

  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [mark-current]
    '("Mark Current Thing" . php-mark-current) 'break)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [kill-current]
    '("Kill Current Thing" . php-kill-current) 'mark-current)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [yank-killed]
    '("Yank Killed Thing" . php-yank) 'kill-current)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [kill-sexp-innard]
    '("Kill Current Sexp Innard" . php-kill-sexp-innard)
    'yank-killed)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [cleanup-script]
    '("Clean Up Script" . php-format-clean-up-script)
    'kill-sexp-innard)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [remove-this-concat]
    '("Remove Single Concatenation" . php-remove-this-concat)
    'cleanup-script)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [implode-concat]
    '("Implode This Concatenation" . php-implode-concat)
    'remove-this-concat)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [change-string-quotes]
    '("Toggle String Quoting" . php-change-string-quotes)
    'implode-concat)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [change-string<->doc]
    '("Toggle String/Doc" . php-change-string<->doc)
    'change-string-quotes)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [change-bare-html<->heredoc]
    '("Toggle HTML/Heredoc" . php-change-bare-html<->heredoc)
    'change-string<->doc)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [combine-scripts]
    '("Combine Consecutive Scripts" . php-combine-scripts)
    'change-short-tags)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [align-on]
    '("Align On" . align-on)
    'combine-scripts)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [modify-method]
    '("Modify Thing" . php-modify-thing)
    'align-on)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf edit])
    [modify-method-args]
    '("Modify Method Arguments" . php-modify-method-argument)
    'modify-method)

  (define-key-after (lookup-key zf-mode-map [menu-bar zf])
    [jump]
    '("Jump to Thing" . php-jump-to-thing)
    'edit)

  (define-key-after (lookup-key zf-mode-map [menu-bar zf])
    [sep1]
    '("--single-line")
    'edit)

  (define-key-after (lookup-key zf-mode-map [menu-bar zf])
    [project]
    (cons "Project" (make-sparse-keymap "Project"))
    'sep1)
  (define-key (lookup-key zf-mode-map [menu-bar zf project])
    [show-dir]
    '("Show Project Directory" . php-project-show-directory))
  (define-key-after (lookup-key zf-mode-map [menu-bar zf project])
    [create-dir]
    '("Create Project Directory" . zf-create-directory-structure)
    'show-dir)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf project])
    [dired]
    '("Open Project Directory in Dired" . php-project-dired-directory)
    'create-dir)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf project])
    [project-close]
    '("Close Project" . php-project-close)
    'dired)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf project])
    [project-add]
    '("Add Project" . php-project-add)
    'project-close)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf project])
    [project-remove]
    '("Remove Project" . php-project-remove)
    'project-add)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf project])
    [php-vc-dir]
    '("Project Directory VC" . php-project-vc-dir)
    'project-remove)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf project])
    [customize]
    '("Customize Projects" . php-project-customize)
    'php-vc-dir)

  (define-key-after (lookup-key zf-mode-map [menu-bar zf])
    [tags]
    (cons "Tags" (make-sparse-keymap "Tags"))
    'project)
  (define-key (lookup-key zf-mode-map [menu-bar zf tags])
    [create]
    '("Create Tags File" . php-create-tag-file))
  (define-key-after (lookup-key zf-mode-map [menu-bar zf tags])
    [create-with-dirs]
    '("Create Tags File With Extra Directories" . php-create-tag-file-with-dirs)
    'create)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf tags])
    [load]
    '("Load Tags File" . load-tags)
    'create-with-dirs)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf tags])
    [customize]
    '("Customize Tags" . php-tags-customize)
    'load)

  (define-key-after (lookup-key zf-mode-map [menu-bar zf])
    [test]
    (cons "Test" (make-sparse-keymap "Test"))
    'tags)
  (define-key (lookup-key zf-mode-map [menu-bar zf test])
    [compile]
    '("Compile" . php-compile))
  (define-key-after (lookup-key zf-mode-map [menu-bar zf test])
    [test-full-project]
    '("Test Full Project" . php-test-full-project)
    'compile)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf test])
    [compile-again]
    '("Run Test Again" . php-compile-again)
    'test-full-project)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf test])
    [lint]
    '("PHP Lint" . php-lint)
    'compile-again)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf test])
    [lint-all]
    '("PHP Lint All" . php-lint-all)
    'lint)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf test])
    [phpcs]
    '("PHPCS" . phpcs)
    'lint-all)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf test])
    [phpcs-all]
    '("PHPCS All" . phpcs-all)
    'phpcs)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf test])
    [phpmd]
    '("PHPMD" . phpmd)
    'phpcs-all)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf test])
    [phpmd-all]
    '("PHPMD All" . phpmd-all)
    'phpmd)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf test])
    [phpunit]
    '("PHPUnit" . phpunit)
    'phpmd-all)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf test])
    [phpunit-all]
    '("PHPUnit All" . phpunit-all)
    'phpunit)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf test])
    [phpunit-single]
    '("PHPUnit Single Test" . phpunit-single-test)
    'phpunit-all)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf test])
    [phpunit-open]
    '("Open PHPUnit Config" . php-project-open-phpunit-config)
    'phpunit-single)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf test])
    [phpunit-logging]
    '("Toggle PHPUnit Logging" . phpunit-toggle-logging)
    'phpunit-open)
  (define-key-after (lookup-key zf-mode-map [menu-bar zf test])
    [customize]
    '("Customize Testing" . php-test-customize)
    'phpunit-logging)

  (define-key-after (lookup-key zf-mode-map [menu-bar zf])
    [sep2]
    '("--single-line")
    'test)

  (define-key-after (lookup-key zf-mode-map [menu-bar zf])
    [customize]
    '("Customize Zf-Mode" . zf-mode-customize)
    'sep2)
  ;; pulled from php-mode
  (define-key zf-mode-map [menu-bar zf complete-function]
    '("Complete function name" . php-complete-function))
  (define-key zf-mode-map [menu-bar zf browse-manual]
    '("Browse manual" . php-browse-manual))
  (define-key zf-mode-map [menu-bar zf search-documentation]
    '("Search documentation" . php-search-documentation))
)

(defun zf-set-keymap-and-menu ()
  (setq zf-mode-map (make-sparse-keymap))
  (zf-define-keys)
  (zf-define-menu))

(unless zf-mode-map
  (zf-set-keymap-and-menu))

;;; *******
;;; zf-mode
;;; *******
(define-derived-mode zf-mode c-mode (concat
                                     "zf"
                                     (when (and
                                            zf-mode-show-project-in-modeline
                                            (php-project-nickname))
                                       (concat "[" (php-project-nickname) "]")))
  "Major mode for making developing Zend Framework PHP applications lazier.

\\{zf-mode-map}"
  (zf-mode-setup)
  (c-add-language 'zf-mode 'c-mode)
  (set (make-local-variable 'c-opt-cpp-start) php-tags-key)
  (set (make-local-variable 'c-opt-cpp-prefix) php-tags-key)
  (c-set-offset 'cpp-macro 0)
  (set (make-local-variable 'c-block-stmt-1-key) php-block-stmt-1-key)
  (set (make-local-variable 'c-block-stmt-2-key) php-block-stmt-2-key)
  (set (make-local-variable 'c-doc-comment-style)
       '((zf-mode . javadoc)))
  (set (make-local-variable 'c-class-key) php-class-key)

  (zf-setup-font-locking)

  ;; Do not force newline at end of file.  Such newlines can cause
  ;; trouble if the PHP file is included in another file before calls
  ;; to header() or cookie().
  (set (make-local-variable 'require-final-newline) nil)
  (set (make-local-variable 'next-line-add-newlines) nil)

  ;; (setq indent-line-function 'php-cautious-indent-line)
  ;; (setq indent-region-function 'php-cautious-indent-region)
  (setq c-special-indent-hook nil)

  ;; (set (make-local-variable 'beginning-of-defun-function)
  ;;      'php-beginning-of-defun)
  ;; (set (make-local-variable 'end-of-defun-function)
  ;;      'php-end-of-defun)
  ;; (set (make-local-variable 'open-paren-in-column-0-is-defun-start)
  ;;      nil)
  ;; (set (make-local-variable 'defun-prompt-regexp)
  ;;      "^\\s-*function\\s-+&?\\s-*\\(\\(\\sw\\|\\s_\\)+\\)\\s-*")
  ;; (set (make-local-variable 'add-log-current-defun-header-regexp)
  ;;      php-beginning-of-defun-regexp)

  ;; PULLED FROM PREVIOUS zf-mode-hook
  (turn-on-font-lock)
  ;;(setq php-mode-force-pear t)
  (setq c-basic-offset 4)
  (c-set-offset 'cpp-macro 'zf-cpp-macro-lineup)
  (c-set-offset 'arglist-intro 'zf-arglist-intro-lineup)
  (c-set-offset 'arglist-close 'zf-arglist-close-lineup)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'knr-argdecl 'zf-knr-argdecl-lineup)
  (c-set-offset 'knr-argdecl-intro 'zf-knr-argdecl-lineup)
  (c-set-offset 'topmost-intro 'zf-topmost-intro-lineup)
  (c-set-offset 'topmost-intro-cont 'zf-topmost-intro-cont-lineup)
  (c-set-offset 'c 'zf-comment-lineup)
  (c-set-offset 'comment-intro 'zf-comment-intro-lineup)
  (c-set-offset 'defun-close 'zf-defun-close-lineup)
  (c-set-offset 'statement 'zf-statement-lineup)
  (c-set-offset 'statement-cont 'zf-statement-cont-lineup)
  (c-set-offset 'string 'zf-string-lineup)
  (c-set-offset 'brace-list-intro 'zf-brace-list-intro-lineup)
  (c-set-offset 'brace-list-entry 'zf-brace-list-entry-lineup)
  (c-set-offset 'brace-list-close 'zf-brace-list-close-lineup)

  (when (and zf-mode-show-trailing-whitespace
             (boundp 'show-trailing-whitespace))
    (setq show-trailing-whitespace t))
  (add-hook 'before-save-hook 'zf-mode-delete-all-trailing-whitespace nil t)
  (add-hook 'after-save-hook 'zf-mode-compile-on-save nil t)
  (load-tags)
  (zf-mode-load-extras)
  
  (php-text-struct-cache-initialize)

  (run-hooks 'zf-mode-hook))

(defcustom zf-mode-hook nil
  "List of functions to be executed on entry to `zf-mode'."
  :type 'hook
  :group 'zf-mode)

(provide 'zf-mode)

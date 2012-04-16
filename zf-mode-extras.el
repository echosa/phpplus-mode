;;; zf-mode-extras.el

;; Version: 1.0
;; Created: 9-28-2011
;; Last modified: Time-stamp: "2012-03-23 17:05:35 bzwahr"
;; Copyright © 2009 Brian Zwahr
;; Author(s):
;; Brian Zwahr <echosa@gmail.com>

;; *************************************************************************

;;; *****
;;; About
;;; *****

;; zf-mode-extras brings the functionality of other emacs modes, packages,
;; and functions to zf-mode.

;;; *********
;;; Customize
;;; *********
(defgroup zf-mode-extras nil
  "Customizations for zf-mode."
  :group 'zf-mode)

(defcustom zf-mode-php-electric t
  "Whether or not to turn on php-electric-mode by default."
  :type 'boolean
  :group 'zf-mode-extras)

(defcustom zf-mode-camelCase t
  "Whether or not to turn on camelCase-mode by default."
  :type 'boolean
  :group 'zf-mode-extras)

(defcustom zf-mode-company nil
  "Whether or not to turn on company-mode by default."
  :type 'boolean
  :group 'zf-mode-extras)

(defcustom zf-mode-hideshow nil
  "Whether or not to turn on hideshow."
  :type 'boolean
  :group 'zf-mode-extras)

(defcustom zf-mode-hideshow-hide-on-load nil
  "Whether or not have hideshow hide methods when loading
classes."
  :type 'boolean
  :group 'zf-mode-extras)

(defcustom zf-mode-hideshowvis nil
  "Whether or not to enable hideshowvis, a package places +/- markers for 
hidden/shown blocks."
  :type 'boolean
  :group 'zf-mode-extras)

(defcustom zf-mode-wrap-region nil
  "Whether or not to enable wrap-region."
  :type 'boolean
  :group 'zf-mode-extras)

(defun zf-mode-load-extras ()
  "Load any extras that are enabled."
  ;; php-electric
  (when zf-mode-php-electric
    (require 'php-electric nil t)
    (php-electric-mode t)
    (define-key zf-mode-map [return] 'php-auto-fill))
  ;; camcelCase-mode
  (when zf-mode-camelCase
    (require 'camelCase nil t)
    (camelCase-mode t))
  ;; company-mode
  (when (and zf-mode-company
             (require 'company nil t)
             (require 'company-etags nil t)
             (boundp 'company-etags-modes))
    (unless (memq 'zf-mode company-etags-modes) ;; was php-mode
      (add-to-list 'company-etags-modes 'zf-mode)) ;; was php-mode
    (company-mode t))
  ;; hideshow
  (when zf-mode-hideshow
    (require 'hideshow nil t)
    (hs-minor-mode t)
    (when zf-mode-hideshow-hide-on-load 
      (php-hide-innards))
    (when (and zf-mode-hideshowvis
               (fboundp 'define-fringe-bitmap)
               (require 'hideshowvis nil t))
      (hideshowvis-enable)))
  ;; wrap region
  (when (and zf-mode-wrap-region
             (require 'wrap-region nil t))
    (wrap-region-mode t)))

(provide 'zf-mode-extras)

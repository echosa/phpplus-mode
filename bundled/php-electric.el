;; -*- Emacs-Lisp -*-
;;
;; php-electric.el --- electric submode for the php-mode
;;
;; Version: 1.0
;; Release-Date: Sunday 04 March 2007
;;
;; Copyright (C) 2007
;;   by Nikolay V. Nemshilov aka St. <nemshilov dog gmail . com>
;;
;;
;; License
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;;
;; Features:
;;   * autocompletion of the language contructions
;;     such as if, for, foreach, etc blocks,
;;
;;   * autocompletion of classes, interfaces and functions
;;     definitions
;;
;;   * autocompletion of the paired symbols, like [], (), "",''
;;
;;
;; Usage:
;;   Nothing magical, just place the file in a directory where
;;   Emacs can find it, and write
;;
;;   (require 'php-electric)
;;
;;   in your configuration files ~/.emacs or wherever you keep it.
;;   Then you can switch on/off the mode by the following command
;;
;;   M-x php-electric-mode <RET>
;;
;;   If you like to have it switched on automatically, you should
;;   put the command in your php-mode hook or create new one,
;;   like that
;;
;;   (add-hook 'php-mode-hook '(lambda () (php-electric-mode)))
;;
;;   That's it.
;;
;;
;; Changelog:
;;   Sunday 04 March 2007
;;     The first version 1.0 has been came out.
;;

(defgroup php-electric nil
  "Minor php-electric mode"
  :group 'php)

;; list of keywords which expandible by the {} pair
(defconst php-electric-expandible-simple-re
  "\\(try\\|else\\|do\\)")

;; list of keywords which expandible with the (){} construction
(defconst php-electric-expandible-as-struct-re
  "\\(while\\|for\\|foreach\\|if\\|elseif\\|catch\\)")

;; list of keywords which expandible with the name(){} construction
(defconst php-electric-expandible-as-func-re
  "\\(function\\)")

;; list of keywords which expandible with the name{} construction
(defconst php-electric-expandible-as-class-re
  "\\(class\\|interface\\)")

;; list of the paired chars
(defvar php-electric-matching-delimeter-alist
  '((?\[ . ?\])
    (?\( . ?\))
    (?\" . ?\")
    (?\' . ?\')))

;; the minor-mode definition
(define-minor-mode php-electric-mode
  "Minor electric-mode for the php-mode"
  nil
  "-El"
  nil ;;php-mode-map
  (php-electric-keymap)
)

;; list of accessible keys
;; changed php-mode-map to zf-mode-map
(defun php-electric-keymap()
  (define-key zf-mode-map " " 'php-electric-space)
  (define-key zf-mode-map "{" 'php-electric-curlies)
  (define-key zf-mode-map "(" 'php-electric-brackets)
  (define-key zf-mode-map "[" 'php-electric-matching-char)
  (define-key zf-mode-map "\"" 'php-electric-matching-char)
  (define-key zf-mode-map "\'" 'php-electric-matching-char)
  (define-key zf-mode-map [return] 'newline-and-indent))

;; handler for the spaces insertions
(defun php-electric-space(arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (php-electric-is-code-at-point-p)
      (if (php-electric-line-is-simple-expandible)
          ;; inserting just a pair of curlies
          (progn
            (insert "{")(php-electric-insert-new-line-and-statement-end))
        (if (php-electric-line-is-expandible-as-struct)
            ;; inserting a structure definition
            (progn
              (if (not (char-equal ?\( (preceding-char)))
                  ;; cmd () {  - style construction
                  (progn
                    (insert "(")(set-register 98 (point-marker))(insert ") {"))
                
                ;; cmd(  ){ - style construction
                (progn
                  (insert "")(set-register 98 (point-marker))(insert ") {")))
              (php-electric-insert-new-line-and-statement-end)
              (jump-to-register 98)(set-register 98 nil))
          (if (php-electric-line-is-expandible-as-func)
              ;; inserting the function expanding
              (save-excursion
                (insert "()\n\t{")(php-electric-insert-new-line-and-statement-end))
            (if (php-electric-line-is-expandible-as-class)
                ;; inserting the class expanding
                (save-excursion
                  (insert "\n{")(php-electric-insert-new-line-and-statement-end)(insert "\n"))))))))

;; handler for the { chars
(defun php-electric-curlies(arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (php-electric-is-code-at-point-p)
      (progn
	(php-electric-insert-new-line-and-statement-end))))

;; handler for the ( chars
(defun php-electric-brackets(arg)
  (interactive "P")

  (if (php-electric-is-code-at-point-p)
      ;; checking if it's a statement
      (if (php-electric-line-is-expandible-as-struct)
	  (progn (php-electric-space arg))
	(progn (php-electric-matching-char arg)))
    (self-insert-command (prefix-numeric-value arg))))

;; handler for the paired chars, [], (), "", ''
(defun php-electric-matching-char(arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (php-electric-is-code-at-point-p)
      (save-excursion
	(insert (cdr (assoc last-command-char
			    php-electric-matching-delimeter-alist))))))

;; checks if the current pointer situated in a piece of code
(defun php-electric-is-code-at-point-p()
  (and php-electric-mode
       (let* ((properties (text-properties-at (point))))
	 (and (null (memq 'font-lock-string-face properties))
	      (null (memq 'font-lock-comment-face properties))))))

;; checks if the current line expandible with a simple {} construction
(defun php-electric-line-is-simple-expandible()
  (let* ((php-electric-expandible-simple-real-re
	  (concat php-electric-expandible-simple-re "\\s-$")))
    (save-excursion
      (backward-word 1)
      (looking-at php-electric-expandible-simple-real-re))))

;; checks if the current line expandible with the (){} construction
(defun php-electric-line-is-expandible-as-struct()
  (let* ((php-electric-expandible-as-struct-real-re
	  (concat php-electric-expandible-as-struct-re "[ ]*$"))
	 (php-electric-expandible-as-struct-with-bracket-re
	  (concat php-electric-expandible-as-struct-re "($")))
    (save-excursion
      (backward-word 1)
      (or (looking-at php-electric-expandible-as-struct-real-re)
	  (looking-at php-electric-expandible-as-struct-with-bracket-re)))))

;; checks if the current line expandible with the name(){} construction
(defun php-electric-line-is-expandible-as-func()
  (let* ((php-electric-expandible-as-func-real-re
	  (concat php-electric-expandible-as-func-re "\\s-$")))
    (save-excursion
      (backward-word 1)
      (looking-at php-electric-expandible-as-func-real-re))))

;; checks if the current line expandible with the name{} construction
(defun php-electric-line-is-expandible-as-class()
  (let* ((php-electric-expandible-as-class-real-re
	  (concat php-electric-expandible-as-class-re "\\s-$")))
    (save-excursion
      (backward-word 1)
      (looking-at php-electric-expandible-as-class-real-re))))

;; "shortcut" to insert \n} construction
(defun php-electric-insert-new-line-and-statement-end()
  (newline-and-indent)(set-register 99 (point-marker))
  (insert "\n}")(indent-according-to-mode)
  (jump-to-register 99)(set-register 99 nil))


(provide 'php-electric)

;; end of the file
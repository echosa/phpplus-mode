;;; php-snippet.el --- Snippet functions for php+-mode

;;; Commentary:
;; This file contains functions to make php+-mode work with yasnippet.

;;; Code:
(require 'yasnippet)
(require 'string-utils)

(defvar *php-snippet-dir*
  (concat (file-name-directory (symbol-file 'nil-or-blank)) "snippets/"))
(setq yas/snippet-dirs
      (append yas/snippet-dirs (list (concat *php-snippet-dir*))))
(yas/load-directory *php-snippet-dir*)
(yas/global-mode 1)

(defun php-yas-camelCase2words (text)
  "Change TEXT from camelCased to separate words."
  (while (and (< 0 (length text))
              (string= "_" (substring text 0 1)))
    (setq text (substring text 1)))
  (when (< 0 (length text))
    (let ((case-fold-search nil))
      (while (string-match "[A-Z]+" text 1)
        (let ((matched (match-string 0 text)))
          (if (or (eq 1 (length matched))
                  (and (< 1 (length matched))
                       (string= (substring text (- (length matched))) matched)))
              (setq text (replace-match
                          (concat " " (downcase matched)) 
                          t nil text))
            (setq text (replace-match
                        (concat
                         " "
                         (downcase (substring matched 0 -1))
                         " "
                         (downcase (substring matched -1)))
                        t nil text)))))
      (upcase-first text))))

(defun php-yas-param (text)
  (unless (nil-or-blank text)
    (insert text)
    (php-format-break-statement))
  nil)

(defun php-yas-param-doc (text)
  "Function parameter/docblock syncing.
This function makes it such that, as you type parameters into the function
snippet, proper docblock is generated.
Argument TEXT text sent from yasnippet."
  (unless (nil-or-blank text)
    (save-excursion
      (forward-line 1)
      (beginning-of-line)
      (while (re-search-forward "\\(@param\\)\\|\\(*[ ]?$\\)"
                                (line-end-position)
                                t)
        (beginning-of-line)
        (kill-line)
        (kill-line)))
    (let* ((params (split-string text "," t))
           param-list
           (var-length 0)
           (type-length 0)
           (insert-string "")
           (col (save-excursion
                  (forward-line -1)
                  (beginning-of-line)
                  (search-forward "*" nil t)
                  (1- (current-column))))
           (indent (make-string col ?\ )))
      (dolist (param params)
        (let ((parts (split-string param))
              variable
              (type "mixed"))
          (if (eq 1 (length parts))
              (setq variable (first parts))
            (setq variable (second parts))
            (setq type (first parts)))
          (when variable
            (setq param-list (append param-list (list (cons type variable))))
            (when (> (length variable) var-length)
              (setq var-length (length variable)))
            (when (> (length type) type-length)
              (setq type-length (length type))))))
      (dolist (param-cons param-list)
        (let* ((variable (cdr param-cons))
               (type (car param-cons))
               (type-indent (make-string (1+ (- type-length (length type)))
                                         ?\ ))
               (var-indent (make-string (1+ (- var-length (length variable)))
                                        ?\ )))
          (setq insert-string (concat insert-string "
" indent "* @param " type type-indent variable var-indent (php-yas-camelCase2words (substring variable 1)))))
        )
      (unless (string= "" insert-string)
        (setq insert-string (concat insert-string "
" indent "*"))
        (insert insert-string)))))

(defun php-yas-return ()
  "Prompts for a function return value type."
  (completing-read "Return type (void): "
                   (php-completion-get-type-list t)
                   nil nil nil nil "void"))

(defun php-yas-function-visibility ()
  "Prompts for a function's visibility."
  (let ((choice (completing-read "public, protected, or private: "
                                 '("public" "protected" "private"))))
    (unless (string= choice "")
      (setq choice (concat choice " ")))
    ;;(concat choice "function ")
    choice
))

(defun php-yas-get-author ()
  "Gets the correct author."
  (let ((author (php-doc-get-author)))
    (if (stringp author)
        author
      (concat (car author) " <" (cdr author) ">"))))

(defun php-yas-break ()
  "Breaks after snippet expansion, if needed."
  (when (eq major-mode 'php+-mode)
    (php-format-break-current)))
(add-hook 'yas/after-exit-snippet-hook 'php-yas-break)

(provide 'php-snippets)

;;; php-snippet.el ends here

;; pulled and renamed from php-mode
(defcustom php-manual-url "http://www.php.net/manual/en/"
  "URL at which to find PHP manual.
You can replace \"en\" with your ISO language code."
  :type 'string
  :group 'zf-mode)

;; pulled and renamed from php-mode
(defcustom php-search-url "http://www.php.net/"
  "URL at which to search for documentation on a word."
  :type 'string
  :group 'zf-mode)

;; pulled and renamed from php-mode
(defcustom php-manual-path ""
  "Path to the directory which contains the PHP manual."
  :type 'string
  :group 'zf-mode)

;; Find the pattern we want to complete
;; find-tag-default from GNU Emacs etags.el
;; pulled from php-mode
(defun php-get-pattern ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (or (re-search-backward "\\sw\\|\\s_"
                                (save-excursion (beginning-of-line) (point))
                                t)
            (re-search-forward "\\(\\sw\\|\\s_\\)+"
                               (save-excursion (end-of-line) (point))
                               t))
        (progn (goto-char (match-end 0))
               (buffer-substring-no-properties
                (point)
                (progn (forward-sexp -1)
                       (while (looking-at "\\s'")
                         (forward-char 1))
                       (point))))
      nil)))

;; pulled from php-mode
(defun php-show-arglist ()
  (interactive)
  (let* ((tagname (php-get-pattern))
         (buf (find-tag-noselect tagname nil nil))
         arglist)
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (when (re-search-forward
             (format "function\\s-+%s\\s-*(\\([^{]*\\))" tagname)
             nil t)
        (setq arglist (buffer-substring-no-properties
                       (match-beginning 1) (match-end 1)))))
    (if arglist
        (message "Arglist for %s: %s" tagname arglist)
        (message "Unknown function: %s" tagname))))

;; Define function documentation function
;; pulled from php-mode
(defun php-search-documentation ()
  "Search PHP documentation for the word at point."
  (interactive)
  (browse-url (concat php-search-url (current-word t))))

;; Define function for browsing manual
;; pulled from php-mode
(defun php-browse-manual ()
  "Bring up manual for PHP."
  (interactive)
  (browse-url php-manual-url))

(provide 'php-help)
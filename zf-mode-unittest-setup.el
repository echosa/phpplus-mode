(defun zf-mode-unittests ()
  (interactive)
  (ert "zf-test"))

(defalias 'zf-mode-check-yo-self 'zf-mode-unittests)

(provide 'zf-mode-unittest-setup)

;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I have \"\\(.+\\)\"$"
       (lambda (something)
         ;; Do something
         ))

(When "^I have \"\\(.+\\)\"$"
      (lambda (something)
        ;; Do something
        ))

(Then "^I should have \"\\(.+\\)\"$"
      (lambda (something)
        ;; Do something
        ))

(And "^I have \"\\(.+\\)\"$"
     (lambda (something)
       ;; Do something
       ))

(But "^I should not have \"\\(.+\\)\"$"
     (lambda (something)
       ;; Do something
       ))

(Then "^I should be at column \\([0-9]+\\)$"
      (lambda (column-num)
        (let ((current-col (current-column))
              (column-num (string-to-number column-num)))
          (assert (= current-col column-num) nil
                  "Expected column %d, actually at column %d."
                  column-num current-col))))

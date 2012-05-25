(require 'php+-mode-unittest-setup)

(defvar *php+-mode-test-dir* (concat (file-name-directory
                                    (symbol-file 'php+-mode-unittests)) "tests/"))
(add-to-list 'load-path *php+-mode-test-dir*)
(add-to-list 'load-path (concat *php+-mode-test-dir* "ert/"))
(require 'ert)

(defmacro php+-break-test (filename &optional correct-filename)
  "Macro to setup a php+-mode breaking unit test"
  `(let (test-buffer-string
         correct-buffer-string
         php-format-break-all-method-chain-links
         php+-verbose)
     (with-temp-buffer
       (insert-file (concat *php+-mode-test-dir* ,(or correct-filename filename)))
       (setq correct-buffer-string (buffer-string))
       (delete-region (point-min) (point-max))
       (insert-file (concat *php+-mode-test-dir* ,filename))
       (php+-mode)
       (php-format-clean-up-script)
       (delete-trailing-whitespace)
       (setq test-buffer-string (buffer-string))
       (should (string= test-buffer-string correct-buffer-string)))))

(ert-deftest php+-test-php-initial->scope ()
  "Tests that php-initial->scope returns the correct results."
  (should (and (eq 'public (php-initial->scope "u"))
               (eq 'private (php-initial->scope "i"))
               (eq 'protected (php-initial->scope "o")))))

(ert-deftest php+-test-php-insert-methods ()
  "Tests that php-insert-method at differnt positions in a class
works properly."
  (let* (php-blank-line-at-end-of-class
         (test-buffer-string
          (with-temp-buffer
            (insert-file (concat *php+-mode-test-dir* "method-insert-before.php"))
            (php+-mode)
            (php-insert-method "testMethod" 'public nil nil nil nil 
                               :var-list nil 
                               :doc-args '("test" "test" 
                                           "Michael Dwyer <mdwyer@tamu.edu>" 
                                           "void"))
            (php-insert-method "testMethod3" 'public nil nil nil nil 
                               :var-list nil 
                               :doc-args '("test" "test" 
                                           "Michael Dwyer <mdwyer@tamu.edu>" 
                                           "void"))
            (php-insert-method "testMethod2" 'public nil nil nil nil 
                               :var-list nil 
                               :doc-args '("test" "test" 
                                           "Michael Dwyer <mdwyer@tamu.edu>" 
                                           "void"))
            (delete-trailing-whitespace)
            (buffer-string)))
         (correct-buffer-string (with-temp-buffer
                                  (insert-file 
                                   (concat *php+-mode-test-dir* 
                                           "method-insert-after.php"))
                                  (buffer-string))))
    (should (string= test-buffer-string correct-buffer-string))))

(ert-deftest php+-test-php-get-method-names ()
  "Tests that php-get-thing-names returns methods correctly."
  (with-temp-buffer
    (insert-file (concat *php+-mode-test-dir* "Test_Breaking.php"))
    (should (equal '("method1" "method2" "_method3")
                   (php-get-thing-names 'methods)))))

(ert-deftest php+-test-breaking ()
  "Tests the breaking (or not breaking) of statements."
  (php+-break-test "Test_Breaking.php"))

(ert-deftest php+-test-breaking-2 ()
  "Tests the breaking (or not breaking) of statements."
  (php+-break-test "Test_Breaking2.php"))

(ert-deftest php+-test-breaking-3 ()
  "Tests the breaking (or not breaking) of statements."
  (php+-break-test "Test_Breaking3.php"))

(ert-deftest php+-test-breaking-3-1 ()
  "Tests the breaking (or not breaking) of statements."
  (php+-break-test "Test_Breaking3-1.php"))

(ert-deftest php+-test-breaking-3-2 ()
  "Tests the breaking (or not breaking) of statements."
  (php+-break-test "Test_Breaking3-2.php"))

(ert-deftest php+-test-breaking-4 ()
  "Tests the breaking (or not breaking) of statements."
  (php+-break-test "Test_Breaking4.php"))

(ert-deftest php+-test-breaking-5 ()
  "Tests the breaking (or not breaking) of statements."
  (php+-break-test "Test_Breaking5.php" 
                 "Test_Breaking5-correct.php"))

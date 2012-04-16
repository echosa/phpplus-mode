(require 'zf-mode-unittest-setup)

(defvar *zf-mode-test-dir* (concat (file-name-directory
                                    (symbol-file 'zf-mode-unittests)) "tests/"))
(add-to-list 'load-path *zf-mode-test-dir*)
(add-to-list 'load-path (concat *zf-mode-test-dir* "ert/"))
(require 'ert)

(defmacro zf-break-test (filename &optional correct-filename)
  "Macro to setup a zf-mode breaking unit test"
  `(let (test-buffer-string
         correct-buffer-string
         zf-mode-hideshow-hide-on-load
         php-format-break-all-method-chain-links
         zf-verbose)
     (with-temp-buffer
       (insert-file (concat *zf-mode-test-dir* ,(or correct-filename filename)))
       (setq correct-buffer-string (buffer-string))
       (delete-region (point-min) (point-max))
       (insert-file (concat *zf-mode-test-dir* ,filename))
       (zf-mode)
       (php-format-clean-up-script)
       (delete-trailing-whitespace)
       (setq test-buffer-string (buffer-string))
       (should (string= test-buffer-string correct-buffer-string)))))

(ert-deftest zf-test-php-initial->scope ()
  "Tests that php-initial->scope returns the correct results."
  (should (and (eq 'public (php-initial->scope "u"))
               (eq 'private (php-initial->scope "i"))
               (eq 'protected (php-initial->scope "o")))))

(ert-deftest zf-test-php-insert-methods ()
  "Tests that php-insert-method at differnt positions in a class
works properly."
  (let* (php-blank-line-at-end-of-class
         (test-buffer-string
          (with-temp-buffer
            (insert-file (concat *zf-mode-test-dir* "method-insert-before.php"))
            (zf-mode)
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
                                   (concat *zf-mode-test-dir* 
                                           "method-insert-after.php"))
                                  (buffer-string))))
    (should (string= test-buffer-string correct-buffer-string))))

(ert-deftest zf-test-php-get-method-names ()
  "Tests that php-get-thing-names returns methods correctly."
  (with-temp-buffer
    (insert-file (concat *zf-mode-test-dir* "Zfmode_Test_Breaking.php"))
    (should (equal '("method1" "method2" "_method3")
                   (php-get-thing-names 'methods)))))

(ert-deftest zf-test-breaking ()
  "Tests the breaking (or not breaking) of statements."
  (zf-break-test "Zfmode_Test_Breaking.php"))

(ert-deftest zf-test-breaking-2 ()
  "Tests the breaking (or not breaking) of statements."
  (zf-break-test "Zfmode_Test_Breaking2.php"))

(ert-deftest zf-test-breaking-3 ()
  "Tests the breaking (or not breaking) of statements."
  (zf-break-test "Zfmode_Test_Breaking3.php"))

(ert-deftest zf-test-breaking-3-1 ()
  "Tests the breaking (or not breaking) of statements."
  (zf-break-test "Zfmode_Test_Breaking3-1.php"))

(ert-deftest zf-test-breaking-3-2 ()
  "Tests the breaking (or not breaking) of statements."
  (zf-break-test "Zfmode_Test_Breaking3-2.php"))

(ert-deftest zf-test-breaking-4 ()
  "Tests the breaking (or not breaking) of statements."
  (zf-break-test "Zfmode_Test_Breaking4.php"))

(ert-deftest zf-test-breaking-5 ()
  "Tests the breaking (or not breaking) of statements."
  (zf-break-test "Zfmode_Test_Breaking5.php" 
                 "Zfmode_Test_Breaking5-correct.php"))

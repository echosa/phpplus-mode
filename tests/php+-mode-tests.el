(require 'php+-mode-unittest-setup)

(defvar *php+-mode-test-dir* (concat (file-name-directory
                                    (symbol-file 'php+-mode-unittests)) 
                                     "tests/"))
(add-to-list 'load-path *php+-mode-test-dir*)
(add-to-list 'load-path (concat *php+-mode-test-dir* "ert/"))
(require 'ert)

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
            (insert-file (concat *php+-mode-test-dir* 
                                 "method-insert-before.php"))
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
    (insert 
     "<?php
    class Zfmode_Test_Breaking
    {
        public function method1()
        {
            $person = new Occhealth_Model_Personnel($this->getRequest()->getParams());
        }
        public function method2()
        {
            $select = $this->select()->where('person = ?', $person->pk)->order('date DESC');
        }
        private function _method3()
        {
            $string = 'Is Certified: ' . $this->view->yesOrNo($location->isCertified) . '<br />Date Certified: ';
        }
    }
")
    (should (equal '("method1" "method2" "_method3")
                   (php-get-thing-names 'methods)))))

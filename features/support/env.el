;; This is an example of how you could set up this file. This setup
;; requires a directory called util in the project root and that the
;; util directory contains the testing tools ert and espuds.

(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq phpplus-mode-root-path project-directory)
  (setq phpplus-mode-util-path (expand-file-name "util" 
                                                 phpplus-mode-root-path)))

(add-to-list 'load-path phpplus-mode-root-path)
(add-to-list 'load-path 
             (expand-file-name "features/support/" phpplus-mode-root-path))

(require 'ecukes-env-local)
(require 'php+-mode)
(require 'espuds)
(require 'ert)


(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )

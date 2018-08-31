;;;; barcode-scanner.test..asd
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :cl-user)
(defpackage barcode-scanner.test-asd
  (:use :cl :asdf))
(in-package :barcode-scanner.test-asd)


(defsystem barcode-scanner.test
  :author "Jeremiah LaRocco"
  :mailto "jeremiah_larocco@fastmail.com"
  :description "Test barcode-scanner."
  :license "ISC"
  :depends-on (:barcode-scanner
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(every #'fiveam::TEST-PASSED-P (5am:run :barcode-scanner))"))))

;;;; barcode-scanner.asd
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(asdf:defsystem #:barcode-scanner
  :description "A library for reading barcodes using a webcam."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi #:common-cv #:alexandria #:j-utils #:trivial-main-thread #:zbar #:png)
  :components ((:file "package")
               (:file "barcode-scanner"))
  :in-order-to ((test-op (test-op :barcode-scanner.test))))

;;;; package.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(defpackage #:barcode-scanner
  (:use #:cl #:alexandria)
  (:export #:scan-from-webcam
           #:scan-from-file
           #:show-scan-from-file
           #:show-barcode
           #:filter-image
           #:read-scan-line

           #:show-test-image

           #:zbar
           ))

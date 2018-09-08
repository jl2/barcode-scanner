;;;; package.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(defpackage #:barcode-scanner
  (:use #:cl #:alexandria)
  (:export #:scan-barcodes-from-webcam
           #:scan-barcode-from-file
           #:show-scan-from-file

           #:rle-scan-line
           #:black #:white
           #:show-test-image
           #:minimize-rle
           #:rle-to-number
           ))

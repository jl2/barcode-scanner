;;;; package.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(defpackage #:barcode-scanner
  (:use #:cl #:alexandria)
  (:export #:scan-barcodes-from-webcam
           #:scan-barcode-from-file
           #:show-scan-from-file
           #:segment
           #:segment-start-x
           #:segment-start-y
           #:segment-end-x
           #:segment-end-y
           #:segment-width
           ))

;;;; pacakge.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :cl-user)
(defpackage :barcode-scanner.test
  (:use :cl
        :fiveam
        :alexandria
        :barcode-scanner))

(in-package :barcode-scanner.test)

(def-suite :barcode-scanner)
(in-suite :barcode-scanner)

(test rightside-up
  (let* ((image (cv:load-image (format nil "~a" (asdf/system:system-relative-pathname :barcode-scanner.test "978-1591-586654.png"))))
         (barcode (barcode-scanner:read-barcode-from-image image)))
    (is-true (string= "9781591586654" barcode))))

(test upside-down
  (let* ((image (cv:load-image (format nil "~a" (asdf/system:system-relative-pathname :barcode-scanner.test "978-0-19-920665-0.png"))))
         (barcode (barcode-scanner:read-barcode-from-image image)))
    (is-true (string= "9780199206650" barcode))))

(test rotated-left
  (let* ((image (cv:load-image (format nil "~a" (asdf/system:system-relative-pathname :barcode-scanner.test "978-1-84334-669-2.png"))))
         (barcode (barcode-scanner:read-barcode-from-image image)))
    (is-true (string= "9781843346692" barcode))))

(test rotated-right
  (let* ((image (cv:load-image (format nil "~a" (asdf/system:system-relative-pathname :barcode-scanner.test "0-415-94293-4.png"))))
         (barcode (barcode-scanner:read-barcode-from-image image)))
    (is-true (string= "0415942934" barcode))))


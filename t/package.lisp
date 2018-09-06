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

;; (test rightside-up
;;   (let* ((image (cv:load-image (format nil "~a" (asdf/system:system-relative-pathname :barcode-scanner.test "978-1591-586654.png"))))
;;          (barcode (barcode-scanner:scan-barcode-from-file image)))
;;     (is-true (string= "9781591586654" barcode))
;;     (cv:release-image image)))

;; (test upside-down
;;   (let* ((image (cv:load-image (format nil "~a" (asdf/system:system-relative-pathname :barcode-scanner.test "978-0-19-920665-0.png"))))
;;          (barcode (barcode-scanner:scan-barcode-from-file image)))
;;     (is-true (string= "9780199206650" barcode))
;;     (cv:release-image image)))

;; (test rotated-left
;;   (let* ((image (cv:load-image (format nil "~a" (asdf/system:system-relative-pathname :barcode-scanner.test "978-1-84334-669-2.png"))))
;;          (barcode (barcode-scanner:scan-barcode-from-file image)))
;;     (is-true (string= "9781843346692" barcode))
;;     (cv:release-image image)))

;; (test rotated-right
;;   (let* ((image (cv:load-image (format nil "~a" (asdf/system:system-relative-pathname :barcode-scanner.test "0-415-94293-4.png"))))
;;          (barcode (barcode-scanner:scan-barcode-from-file image)))
;;     (is-true (string= "0415942934" barcode))
;;     (cv:release-image image)))


(test rle
      (let ((image (cv:create-image (cv:size 8 8) 8 1)))
        (dotimes (i 8)
          (cv:set-real-2d image 0 i (if (< i 4) 0 255)))
        (let ((rle (barcode-scanner:rle-scan-line image 0 0 1 0 :threshold 100)))
          (is-true (equal '((4 . :black) (4 . :white)) rle)))
        (cv:release-image image)))

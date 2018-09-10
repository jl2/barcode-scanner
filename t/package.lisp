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
;;     (is (string= "9781591586654" barcode))
;;     (cv:release-image image)))

;; (test upside-down
;;   (let* ((image (cv:load-image (format nil
;;                                        "~a"
;;                                        (asdf/system:system-relative-pathname :barcode-scanner.test
;;                                                                              "978-0-19-920665-0.png"))))
;;          (barcode (barcode-scanner:scan-barcode-from-file image)))
;;     (is (string= "9780199206650" barcode))
;;     (cv:release-image image)))

;; (test rotated-left
;;   (let* ((image (cv:load-image (format
;;                                 nil
;;                                 "~a"
;;                                 (asdf/system:system-relative-pathname :barcode-scanner.test
;;                                                                       "978-1-84334-669-2.png"))))
;;          (barcode (barcode-scanner:scan-barcode-from-file image)))
;;     (is (string= "9781843346692" barcode))
;;     (cv:release-image image)))

;; (test rotated-right
;;   (let* ((image (cv:load-image (format nil "~a" (asdf/system:system-relative-pathname :barcode-scanner.test "0-415-94293-4.png"))))
;;          (barcode (barcode-scanner:scan-barcode-from-file image)))
;;     (is (string= "0415942934" barcode))
;;     (cv:release-image image)))


(test decode-test
  (let ((image (cv:create-image (cv:size 8 8) 8 1)))
    (let ((numbers '(#b10010010
                     #b11011011)))
      (dolist (num numbers)
        (dotimes (i 8)
          (cv:set-real-2d image 0 (- 7 i) (if (= 0 (ldb (byte 1 i) num)) 255 0)))
        (let ((found (barcode-scanner:read-scan-line image 0 :horizontal :bit-size 8 :bit-width 1)))
          (is (= num (aref found 0)))))
    (cv:release-image image))))

(test decode-test-2
  (let ((image (cv:create-image (cv:size 12 12) 8 1)))
    (cv:set-real-2d image 0 0 0)
    (cv:set-real-2d image 0 1 0)
    (cv:set-real-2d image 0 2 255)
    (cv:set-real-2d image 0 3 255)
    (cv:set-real-2d image 0 4 0)
    (cv:set-real-2d image 0 5 0)

    (cv:set-real-2d image 0 6 255)
    (cv:set-real-2d image 0 7 255)
    (cv:set-real-2d image 0 8 255)
    (cv:set-real-2d image 0 9 255)
    (cv:set-real-2d image 0 10 0)
    (cv:set-real-2d image 0 11 0)

    (let ((res (barcode-scanner:read-scan-line image 0 :horizontal :bit-size 3 :bit-width 2)))
      (is (= #b101 (aref res 0)))
      (is (= #b001 (aref res 1))))
    (cv:release-image image)))

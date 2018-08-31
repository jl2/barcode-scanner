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

(test empty
      (is-true nil))

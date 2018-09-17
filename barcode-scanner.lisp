;;;; barcode-scanner.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:barcode-scanner)

(defmacro with-gui-thread (&body body)
  "Wraps BODY in code which masks float traps.
   This is needed in SBCL on OSX because native code often
   generate :inexact traps and land you in the debugger.
   For non SBCL this wraps body in a progn."
  `(trivial-main-thread:call-in-main-thread
    (lambda ()
      #+sbcl (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow)
               ,@body)
      #+ccl (unwind-protect (progn
                              (ccl:set-fpu-mode :invalid nil)
                              ,@body)
              (ccl:set-fpu-mode :invalid t))
      #-(or sbcl ccl)
      ,@body)))

(defun scan-from-webcam (&key (camera 0) (fps 10))
  (with-gui-thread
    (cv:with-named-window ("bar-code-scanner")
      (cv:with-captured-camera (vid :width 800 :height 600 :idx camera)
        (loop
           (let* ((frame (cv:query-frame vid)))
             (unwind-protect
                  (progn
                    (cv:show-image "bar-code-scanner" frame)

                    (let ((barcodes (zbar:scan-cv-image frame))
                          (c (cv:wait-key (floor (/ 1000 fps)))))
                      (when barcodes
                        (format t "~a~%" barcodes))
                      (when (or (= c 27) (= c 1048603))
                        (format t "Exiting~%")
                        (return))))
               (cv:release-image frame))))))))

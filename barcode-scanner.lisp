;;;; barcode-scanner.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:barcode-scanner)

(defun read-scan-line (img display scan-line &key (threshold 100))
  (let* ((size (cv:get-size img))
         (width (cv:size-width size))
         (bars nil))
    (handler-case
        (loop
           with bar-start = nil
           for j below width do
             (cond ((and (< (cv:get-real-2d img scan-line j) threshold)
                         (null bar-start))
                    (setf bar-start j))
                   ((and bar-start
                         (> (cv:get-real-2d img scan-line j) threshold))
                    (push (cons j bar-start) bars)
                    (loop for xl from bar-start to j do
                         (cv:set-2d display scan-line xl (cv:scalar 0 0 255)))
                    (setf bar-start nil))))
      (error (err) (format t "Error: ~a~%" err)))
    (when bars (format t "Found bars: ~a~%" bars))
    bars))

(defun convert-bars-to-numbers (gaps)
  (let ((min-max (loop for gap in gaps minimize (- (car gap) (cdr gap)) into min
                    maximize (- (car gap) (cdr gap)) into max
                    finally (return (cons min max))))
        (number-mapping #( #(3 2 1 1) ;; 0
                          #(2 2 2 1) ;; 1
                          #(2 1 2 2) ;; 2
                          #(1 4 1 1) ;; 3
                          #(1 1 3 2) ;; 4
                          #(1 2 3 1) ;; 5
                          #(1 1 1 4) ;; 6
                          #(1 3 1 2) ;; 7
                          #(1 2 1 3) ;; 8
                          #(3 1 1 2))))
    (format t "min-max: ~a~%" min-max)
  min-max))

(defun read-barcode-from-image (frame)
  (cv:with-ipl-images ((one (cv:get-size frame) cv:+ipl-depth-8u+ 1)
                       (two (cv:get-size frame) cv:+ipl-depth-8u+ 1))
    (cv:cvt-color frame one cv:+rgb-2-gray+)
    
    (let* ((threshold 80)
           (size (cv:get-size frame))
           (height (cv:size-height size))
           (lines 30)
           (kernel (cv:create-mat 5 5 cv:+32FC1+)))
      (unwind-protect
           (progn
             (dotimes (i 5)
               (dotimes (j 5)
                 (cv:set-2d kernel i j (cv:scalar -0.1))))

             (cv:set-2d kernel 2 1 (cv:scalar 0.8))
             (cv:set-2d kernel 2 2 (cv:scalar 1.1))
             (cv:set-2d kernel 2 3 (cv:scalar 0.8))

             (cv:filter-2d one two kernel)

             (cv:cvt-color two frame cv:+gray-2-rgb+)
             (dotimes (i lines)
               (let* ((bars (read-scan-line one frame (floor (+ (* (/ height (1+ lines)) i) 1)) :threshold threshold))
                      (numbers (convert-bars-to-numbers bars)))
                 ;; (format t "~a ~a~%" bars numbers)
                 )))
        (cv:free kernel)))))

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

(defun scan-barcodes-from-webcam (&key (camera 0) (fps 10))
  (with-gui-thread
    (cv:with-named-window ("bar-code-scanner")
      (cv:with-captured-camera (vid :width 1920 :height 1080
                                    :idx camera)
        (loop
           (let* ((frame (cv:query-frame vid))
                  (isbn (read-barcode-from-image frame)))
             (cv:show-image "bar-code-scanner" frame))
           (let ((c (cv:wait-key (floor (/ 1000 fps)))))
             (format t "Got: ~a~%" c)
             (when (or (= c 27) (= c 1048603))
               (format t "Exiting~%")
               (return))))))))

(defun scan-barcode-from-file (file-name)
  (let* ((image (cv:load-image file-name))
         (isbn (read-barcode-from-image image)))
    (format t "~a~%" isbn)
    isbn))

(defun show-scan-from-file (file-name)
  (with-gui-thread
    (cv:with-named-window ("bar-code-scanner")
      (loop
         (let* ((image (cv:load-image file-name))
                (isbn (read-barcode-from-image image))
                (fps 10))
           (format t "~a~%" isbn)
           (cv:show-image "bar-code-scanner" image)
           (let ((c (cv:wait-key (floor (/ 1000 fps)))))
             (when (or (= c 27) (= c 1048603))
               (format t "Exiting~%")
               (return isbn))))))))

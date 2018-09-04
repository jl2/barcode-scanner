;;;; barcode-scanner.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:barcode-scanner)

(defstruct segment
  (start-x 0 :type fixnum)
  (start-y 0 :type fixnum)
  (end-x 0 :type fixnum)
  (end-y 0 :type fixnum)
  (width 0.0 :type double-float))

(defun create-segment (sx sy ex ey)
  (let* ((dx (- ex sx))
        (dy (- ey sy))
        (width (sqrt (+ (* 1.0 dx dx) (* 1.0 dy dy)))))
    (make-segment :start-x sx
                  :start-y sy
                  :end-x ex
                  :end-y ey
                  :width width)))

(defun read-scan-line (img display start-x start-y dx dy &key (threshold 100))
  (let* ((size (cv:get-size img))
         (width (cv:size-width size))
         (height (cv:size-height size))
         (bars nil))
    (handler-case
        (do ((bar-start-x nil)
             (bar-start-y nil)
             (i start-x)
             (j start-y))
            ((or (< i 0)
             (< j 0)
             (>= i (1- width))
             (>= j (1- height))))
          (cv:set-2d display j i (cv:scalar 0 255 0))
          (cond ((and (< (cv:get-real-2d img j i) threshold)
                      )
                 (when (null bar-start-x)
                   (setf bar-start-x i)
                   (setf bar-start-y j))
                 (cv:set-2d display j i (cv:scalar 0 0 255)))
                ((and bar-start-x bar-start-y
                      (> (cv:get-real-2d img j i) threshold))
                 (push (create-segment bar-start-x bar-start-y i j) bars)
                 (setf bar-start-x nil)
                 (setf bar-start-y nil)))
          (incf i dx)
          (incf j dy))
      (error (err) (format t "Error: ~a~%" err)))
    bars))

(defun convert-bars-to-numbers (gaps)
  (let ((min-max (loop for gap in gaps minimize (- (car gap) (cdr gap)) into min
                    maximize (- (car gap) (cdr gap)) into max
                    finally (return (cons min max)))))
  min-max))

(defun read-barcode-from-image (frame)
  (cv:with-ipl-images ((one (cv:get-size frame) cv:+ipl-depth-8u+ 1)
                       (two (cv:get-size frame) cv:+ipl-depth-8u+ 1))
    (cv:cvt-color frame one cv:+rgb-2-gray+)
    
    (let* ((threshold 80)
           (kernel (cv:create-mat 5 5 cv:+32FC1+))
           (size (cv:get-size frame))
           (width (cv:size-width size))
           (height (cv:size-height size))
           (mat #2A(( -0.1 -0.1 -0.1 -0.1 -0.1)
                    ( 0.01 0.05 0.1 0.05 0.01)
                    ( 0.1 0.5 1.5 0.5 0.1)
                    ( 0.01 0.05 0.1 0.05 0.01)
                    ( -0.1 -0.1 -0.1 -0.1 -0.1)))
           (lines 25))
      (declare (ignorable width height))
      (unwind-protect
           (progn
             (dotimes (i 5)
               (dotimes (j 5)
                 (cv:set-2d kernel i j (cv:scalar (aref mat j i)))))

             (cv:filter-2d one two kernel)
             ;;(cv:cvt-color two frame cv:+gray-2-rgb+)

             (let ((horizontal (loop for i below lines collecting
                                  (read-scan-line one frame 0 (1+ (floor (* i (/ height (1+ lines))))) 1 0 :threshold threshold)))
                   ;; (vertical (loop for i below 15 collecting
                   ;;                             (read-scan-line one frame (1+ (floor (* i (/ height 21)))) 0 0 1 :threshold threshold)))
                   (vertical nil))
               (concatenate 'list horizontal vertical)))
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
             (declare (ignorable isbn))
;;             (format t "~a~%" isbn)
             (cv:show-image "bar-code-scanner" frame)
             (let ((c (cv:wait-key (floor (/ 1000 fps)))))
               (when (or (= c 27) (= c 1048603))
                 (format t "Exiting~%")
                 (return isbn)))))))))

(defun scan-barcode-from-file (file-name)
  (let* ((image (cv:load-image file-name))
         (isbn (read-barcode-from-image image)))
    (format t "~a~%" isbn)
    isbn))

(defun show-scan-from-file (file-name)
  (with-gui-thread
    (cv:with-named-window ("bar-code-scanner")
      (let* ((image (cv:load-image file-name))
             (isbn (read-barcode-from-image image))
             (fps 30))
        (cv:show-image "bar-code-scanner" image)
        (loop
           (let ((c (cv:wait-key (floor (/ 1000 fps)))))
             (when (or (= c 27) (= c 1048603))
               (format t "Exiting~%")
               (return isbn))))))))

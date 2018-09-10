;;;; barcode-scanner.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:barcode-scanner)


(defun filter-image (img)
  "Apply a filter that enhances the lines in a bar code."
  (let ((filtered (cv:create-image (cv:get-size img) 8 1))
        (kernel (cv:create-mat 5 5 cv:+32FC1+))
        (mat #2A(( 0.0 0.0 0.0 0.0 0.0)
                 ( 0.0 1.0 2.0 1.0 0.0)
                 ( 0.2 0.0 1.0 0.0 0.2)
                 ( 0.0 -1.0 -2.0 -1.0 -0.0)
                 ( 0.0 0.0 0.0 0.0 0.0))))
    (cv:cvt-color img filtered cv:+rgb-2-gray+)
    (unwind-protect
         ;; TODO: Since only a few scanlines are used, don't filter the entire image.
         (progn
           (dotimes (i 5)
             (dotimes (j 5)
               (cv:set-2d kernel i j (cv:scalar (aref mat j i)))))
           (cv:filter-2d filtered filtered kernel))
      (cv:free kernel))
    filtered))

(defun read-scan-line (img idx direction &key (bit-size 7) (threshold 100) (bit-width 1) (reverse nil))
  "Scan horizontally or vertically across a scan line in an image and return an array of numbers of bit-size size.
Each bit is bit-width pixels wide, and each number is bit-size bits wide.
A  multi-pixel bit is 1 if more than half of the pixels are set and 0 otherwise."
  (let* ((size (cv:get-size img))
         (dx (if (eq direction :horizontal) 1 0))
         (dy (if (eq direction :vertical) 1 0))
         (start-x (if (eq direction :horizontal) 0 idx))
         (start-y (if (eq direction :vertical) idx 0))
         (width (cv:size-width size))
         (height (cv:size-height size))
         (result-size (ceiling (/ (if (eq direction :horizontal)
                                      width
                                      height)
                                  (* bit-size bit-width))))
         (numbers (make-array result-size :element-type 'fixnum :initial-element 0)))
    (declare (type fixnum dx dy start-x start-y width height result-size)
             (type (simple-array fixnum (*))))


    ;; Iterate across a scan line vertically or horizontally and fill numbers with bit-size sized numbers
    (do*
     ;; Pixel index
     ((pixi 0 (1+ pixi))

      ;; Bit index in number
      (bi 0 (floor (/ (mod pixi (* bit-size bit-width)) bit-width)))
      (rbi (- (1- bit-size) bi) (- (1- bit-size) bi))

      ;; Index into numbers array
      (ni 0 (floor (/ pixi (* bit-size bit-width))))

      ;; Location in image
      (i start-x
         (+ i dx))
      (j start-y
         (+ j dy))

      ;; Color at i,j
      (current-color (truncate (cv:get-real-2d img 0 0))
                     (truncate (if (and (< i width) (< j height)) (cv:get-real-2d img j i) 0)))

      ;; is it 1 or 0
      (is-set (< current-color threshold)
              (< current-color threshold)))

     ((or (< i 0)
          (< j 0)
          (= i width)
          (= j height)))
      (declare (type fixnum pixi bi rbi ni i j current-color))
      ;; (format t "~a ~a ~a ~a ~a ~a~%" pixi bi rbi ni is-bit numbers)
      (when is-set
        (setf (aref numbers ni) (logior (aref numbers ni) (ash 1 (if reverse bi rbi))))))
    numbers))

(defun show-scan-line (img display idx direction threshold)
  (let* ((size (cv:get-size img))
         (width (cv:size-width size))
         (height (cv:size-height size))
         (dx (if (eq direction :horizontal) 1 0))
         (dy (if (eq direction :vertical) 0 1))
         (start-x (if (eq direction :horizontal) 0 idx))
         (start-y (if (eq direction :vertical) idx 0)))

  (do ((i start-x (+ i dx))
       (j start-y (+ j dy)))
      ((or (< i 0)
           (< j 0)
           (< i  width)
           (< j height)))
      (cond ((< (cv:get-real-2d img j i) threshold)
             (cv:set-2d display j i (cv:scalar 0 0 255)))
            (t
             (cv:set-2d display j i (cv:scalar 0 0 255)))))))

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
      (cv:with-captured-camera (vid :width 800 :height 600
                                    :idx camera)
        (loop
           (let* ((frame (cv:query-frame vid))
                  (filtered (filter-image frame)))

             (unwind-protect
                  (progn
                    (show-scan-line filtered frame 200 :horizontal 100)
                    (format t "~a~%" (read-scan-line filtered 200 :horizontal))
                    (cv:show-image "bar-code-scanner" frame)
                    (let ((c (cv:wait-key (floor (/ 1000 fps)))))
                      (when (or (= c 27) (= c 1048603))
                        (format t "Exiting~%")
                        (return))))
               (cv:release-image filtered))))))))

(defun scan-barcode-from-file (file-name scan-line)
  (let* ((image (cv:load-image file-name))
         (filtered (filter-image image))
         (isbn (read-scan-line image scan-line :horizontal)))
    (cv:release-image filtered)
    (cv:release-image image)
    isbn))

(defun show-scan-from-file (file-name scan-line direction)
  (with-gui-thread
    (cv:with-named-window ("bar-code-scanner")
      (loop
         (let* ((image (cv:load-image file-name))
                (filtered (filter-image image))
                (isbn (read-scan-line filtered scan-line direction))
                (fps 10))
           (unwind-protect
                (progn
                  (show-scan-line filtered image scan-line direction 100)
                  (cv:show-image "bar-code-scanner" filtered)
                  (let ((c (cv:wait-key (floor (/ 1000 fps)))))
                    (when (or (= c 27) (= c 1048603))
                      (format t "Exiting~%")
                      (return isbn))))
             (cv:release-image filtered)
             (cv:release-image image)))))))

(defun show-test-image ()
  (with-gui-thread
    (cv:with-named-window ("bar-code-scanner")
      (let ((image (cv:create-image (cv:size 8 8) 8 1)))
        (dotimes (i 8)
          (cv:set-real-2d image 0 i (if (< i 4) 0 255)))
        (cv:show-image  "bar-code-scanner" image)
        (loop
           (let ((c (cv:wait-key (floor (/ 1000 30)))))
             (when (or (= c 27) (= c 1048603))
               (format t "Exiting~%")
               (return))))
        (format t "~a~%" (read-scan-line image 0 :horizontal :bit-size 8 ))
        (cv:release-image image)))))

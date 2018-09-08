;;;; barcode-scanner.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:barcode-scanner)

(defun rle-scan-line (img start-x start-y dx dy &key (threshold 100))
  (flet ((get-color (x y)
           (if (< (cv:get-real-2d img y x)
                  threshold)
               :black
               :white)))
    (let* ((size (cv:get-size img))
           (width (cv:size-width size))
           (height (cv:size-height size))
           (run-lengths nil))

      (do* ((i (+ start-x dx)
               (+ i dx))
            (j (+ start-y dy)
               (+ j dy))

            (current-length 1
                            (1+ current-length))

            (prev-color (get-color start-x start-y)
                        current-color)

            (current-color (get-color i j)
                           (get-color i j)))
           ((or (< i 0)
                (< j 0)
                (>= i (1- width))
                (>= j (1- height)))
            (push (cons (1+ current-length) current-color) run-lengths))

        (when (not (eq current-color prev-color))
          (push (cons current-length prev-color) run-lengths)
          (setf current-length 0)))

      (reverse run-lengths))))

(defun minimize-rle (rle)
  (declare (ignorable rle))
  )

(defun rle-to-number (rle &optional (bits 7))
  (declare (ignorable rle bits))
  )

(defun read-and-showscan-line (img display start-x start-y dx dy &key (threshold 100))
  (show-scan-line img display start-x start-y dx dy threshold)
  (rle-scan-line img start-x start-y dx dy :threshold threshold))


(defun show-scan-line (img display start-x start-y dx dy threshold)
  (let* ((size (cv:get-size img))
         (width (cv:size-width size))
         (height (cv:size-height size)))
    (do ((i start-x (+ i dx))
         (j start-y (+ j dy)))
        ((or (< i 0)
             (< j 0)
             (>= i (1- width))
             (>= j (1- height))))
      (cond ((< (cv:get-real-2d img j i) threshold)
             (cv:set-2d display j i (cv:scalar 0 0 255)))
            (t
             (cv:set-2d display j i (cv:scalar 0 0 255)))))))

(defun read-barcodes-from-image (frame)
  (cv:with-ipl-images ((one (cv:get-size frame) cv:+ipl-depth-8u+ 1)
                       (two (cv:get-size frame) cv:+ipl-depth-8u+ 1))
    (cv:cvt-color frame one cv:+rgb-2-gray+)

    (let* ((threshold 30)
           (kernel (cv:create-mat 5 5 cv:+32FC1+))
           (size (cv:get-size frame))
           (width (cv:size-width size))
           (height (cv:size-height size))
           (mat #2A(( 0.0 0.0 0.0 0.0 0.0)
                    ( 0.0 1.0 2.0 1.0 0.0)
                    ( 0.2 0.0 1.0 0.0 0.2)
                    ( 0.0 -1.0 -2.0 -1.0 -0.0)
                    ( 0.0 0.0 0.0 0.0 0.0)))
           (lines 15))
      (declare (ignorable width height))
      (unwind-protect
           ;; TODO: Since only a few scanlines are used, don't filter the entire image.
           (progn
             (dotimes (i 5)
               (dotimes (j 5)
                 (cv:set-2d kernel i j (cv:scalar (aref mat j i)))))
             (cv:filter-2d one two kernel)
             ;;(cv:filter-2d two one kernel)
;;             (cv:threshold two one 20 255 cv:+thresh-binary+)
             ;; (cv:cvt-color two frame cv:+gray-2-rgb+)
             (let ((horizontal (loop for i below lines collecting
                                  (read-scan-line two frame 0 (1+ (floor (* i (/ height (1+ lines))))) 1 0 :threshold threshold)))
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
      (cv:with-captured-camera (vid :width 800 :height 600
                                    :idx camera)
        (loop
           (let* ((frame (cv:query-frame vid))
                  (isbn (read-barcodes-from-image frame)))
             (declare (ignorable isbn))
;;             (format t "~a~%" isbn)
             (cv:show-image "bar-code-scanner" frame)
             (let ((c (cv:wait-key (floor (/ 1000 fps)))))
               (when (or (= c 27) (= c 1048603))
                 (format t "Exiting~%")
                 (return isbn)))))))))

(defun scan-barcode-from-file (file-name)
  (let* ((image (cv:load-image file-name))
         (isbn (read-barcodes-from-image image )))
    (format t "~a~%" isbn)
    isbn))

(defun show-scan-from-file (file-name)
  (with-gui-thread
    (cv:with-named-window ("bar-code-scanner")
      (loop
         (let* ((image (cv:load-image file-name))
                (isbn (read-barcodes-from-image image))
                (fps 10))
           (cv:show-image "bar-code-scanner" image)
           (let ((c (cv:wait-key (floor (/ 1000 fps)))))
             (when (or (= c 27) (= c 1048603))
               (format t "Exiting~%")
               (return isbn)))
           (cv:release-image image))))))

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
        (format t "~a~%" (rle-scan-line image 0 0 1 0))
        (cv:release-image image)))))

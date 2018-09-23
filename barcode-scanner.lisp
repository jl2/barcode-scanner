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

(defun save-image (img file-name)
  "Save an image to a .png file."
  (let* ((size (cv:get-size img))
         (width (cv:size-width size))
         (height (cv:size-height size))
         (channels (cv:ipl-n-channels img))
         (oimg (png:make-image height width 3 8)))
    (declare (type fixnum width height channels))
    (cond ((= channels 1)
           (dotimes (j height)
             (declare (type fixnum j))
             (dotimes (i width)
               (declare (type fixnum i))
               (setf (aref oimg j i 0) (truncate (cv:get-real-2d img j i)))
               (setf (aref oimg j i 1) (truncate (cv:get-real-2d img j i)))
               (setf (aref oimg j i 2) (truncate (cv:get-real-2d img j i))))))
          (;; (and has-alpha (= channels 3))
           t
           (dotimes (j height)
             (declare (type fixnum j))
             (dotimes (i width)
               (declare (type fixnum i))
               (let ((val (cv:get-2d img j i )))
                 (setf (aref oimg j i 0) (truncate (cv:scalar-d0 val)))
                 (setf (aref oimg j i 1) (truncate (cv:scalar-d1 val)))
                 (setf (aref oimg j i 2) (truncate (cv:scalar-d2 val))))))))
    (with-open-file (output file-name :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
      (png:encode oimg output))))

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
                        (return)))))))))))

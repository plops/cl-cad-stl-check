#.(progn
    (require :asdf)
    (require :cl-opengl)
    (require :cl-glu)
    (require :cl-glut))
(declaim (optimize (debug 2) (speed 3) (safety 2)))
(defpackage :g
  (:shadowing-import-from :cl close get special)
  (:use :cl :gl :glut))
 
(in-package :g)
 
;; cat points |tr ']' '\n'|cut -d " " -f 3,4,5|tr ',' ' '|awk '{print "(" $N")"}' > points2.lisp
;; cat triangles  | tr ']' '\n' |grep -v ^,$|tr ',' ' '|tr '[' ' '|awk '{print "(" $N ")"}' >> triangles2.lisp

(load "points.lisp")
(load "triangles.lisp")

(defun cross (a b)
  (when (and a b)
   (list (- (* (elt a 1) (elt b 2))
	    (* (elt a 2) (elt b 1)))
	 (- (* (elt a 2) (elt b 0))
	    (* (elt a 0) (elt b 2)))
	 (- (* (elt a 0) (elt b 1))
	    (* (elt a 1) (elt b 0))))))
#+nil
(cross '(1 0 0) '(0 1 0))

(defun v- (a b)
  (when (and a b)
   (list (- (elt a 0) (elt b 0))
	 (- (elt a 1) (elt b 1))
	 (- (elt a 2) (elt b 2)))))

(defun v. (a b)
  (when (and a b)
   (+ (* (elt a 0) (elt b 0))
      (* (elt a 1) (elt b 1))
      (* (elt a 2) (elt b 2)))))

(defun norm (a)
  (when a
   (sqrt (v. a a))))

(defun v* (a s)
  (when a
   (list (* s (elt a 0))
	 (* s (elt a 1))
	 (* s (elt a 2)))))

(defun normalize (a)
  (let ((l (norm a)))
    (if (< l 1d-12)
	(list 0d0 0d0 0d0)
	(v* a (/ l)))))

(defun get-triangle (triangle-index)
  (let ((three (elt triangles triangle-index)))
   (loop for p in three collect
	(when (< p 47)
	 (elt points p)))))
#+nil
(dotimes (i (length triangles))
 (get-triangle i))


(defun draw-triangle (three)
  (destructuring-bind (p1 p2 p3)
      three
    (when (and p1 p2 p3)
     (let ((n (normalize
	       (cross (v- p1 p3)
		      (v- p1 p2)))))
       (when n
	 (loop for p in three do
	      (when p
		(destructuring-bind (x y z)
		    p 
		  (when (and x y z)
		    #+nil(format t "drawing ~a~%" p)
		    (vertex x y z)
		    (destructuring-bind (nx ny nz)
			n
		     (normal nx ny nz))
		    )))))))))


(defclass fenster (window)
  ((cursor-position :accessor cursor-position 
		    :initform #(0 0)
		    :type (simple-array fixnum 2))))
 
(defun set-3d-view (w)
  (load-identity)
  (viewport 0 0 (width w) (height w))
  (matrix-mode :projection)
  (load-identity)
  #+nil (ortho 0 (width w) (height w) 0 -1 1)
  (glu:perspective 70 (/ (width w) (height w)) .01 10)
  (glu:look-at 2 3 1
               0 0 0
               0 0 1)
  (matrix-mode :modelview)
  (load-identity))
 
(defmethod display-window :before ((w fenster))
  (set-3d-view w))
 
(defparameter rot 0)
(defun draw (mx my mz)
  (load-identity)
  (if (< rot 360)
      (incf rot)
      (setf rot 0))
  (rotate rot 0 0 1)
  (let* ((x 1))
    (with-primitive :lines
      (color 1 0 0) (vertex 0 0) (vertex x 0)
      (color 0 1 0) (vertex 0 0) (vertex 0 x)
      (color 0 0 1) (vertex 0 0) (vertex 0 0 x)))
  (with-pushed-matrix
    (color 1 1 1)
    (translate mx my mz)
    (let* ((x .02) (m (- x)))
      (with-primitive :lines
	(vertex m 0) (vertex x 0)
	(vertex 0 m) (vertex 0 x)
	(vertex 0 0 m) (vertex 0 0 x))))
  (enable :lighting :light0 :depth-test)
  (with-pushed-matrix
    (scale .02 .02 .02)
    (with-primitive :triangles
      (with-pushed-matrix
	(scale .01 .01 .01)
	(dotimes (i 50)
	  (draw-triangle (get-triangle i))))))
  (disable :lighting :light0))

(defmethod display ((w fenster))
  (clear :color-buffer-bit :depth-buffer-bit)
  (set-3d-view w)
  (multiple-value-bind (mx my mz)
      (glu:un-project (aref (cursor-position w) 0)
		      (aref (cursor-position w) 1)
		      0)
    (draw mx my mz))
  (color 1 1 1)
 
  (swap-buffers)
  (sleep (/ 30))
  (post-redisplay))
 
(defmethod passive-motion ((w fenster) x y)
  (setf (aref (cursor-position w) 0) x
	(aref (cursor-position w) 1) (- (height w) y))
 #+nil (format t "~a~%" (list x
			 y
			 (multiple-value-list 
			  (glu:un-project x y 0)))))
 
(defmethod keyboard ((w fenster) key x y)
  (case key
    (#\Esc (destroy-current-window))))
 
(defparameter *main* nil)
 
(defun run ()
  (setf *main* (make-instance 'fenster :mode '(:double :rgb :depth)))
  (display-window *main*))
 
(run)
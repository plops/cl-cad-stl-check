#.(progn
    (require :asdf)
    (require :cl-opengl)
    (require :cl-glu)
    (require :cl-glut)
    (require :vector))
(declaim (optimize (debug 2) (speed 3) (safety 2)))
(defpackage :g
  (:shadowing-import-from :cl close get special)
  (:use :cl :gl :glut :vector))
 
(in-package :g)
 
;; cat points |tr ']' '\n'|cut -d " " -f 3,4,5|tr ',' ' '|awk '{print "(" $N")"}' > points2.lisp
;; cat triangles  | tr ']' '\n' |grep -v ^,$|tr ',' ' '|tr '[' ' '|awk '{print "(" $N ")"}' >> triangles2.lisp

#.(load "points.lisp")
#.(load "triangles.lisp")

;; store the data in simple arrays
#.(defparameter *points*
  (let* ((n (length points))
	 (s (make-array n
		       :element-type 'vec
		       :initial-element (v))))
    (dotimes (i n)
      (let ((v (elt points i)))
       (setf (aref s i) (v (coerce (first v) 'double-float)
			   (coerce (second v) 'double-float)
			   (coerce (third v) 'double-float)))))
    s))

(declaim (type (simple-array vec 1) *points*))

#.(defparameter *faces*
    (let* ((n (length triangles))
	   (np1 (1- (length *points*)))
	   (s (make-array n
			  :element-type 'vec-i
			:initial-element (make-vec-i))))
      (dotimes (i n)
	(let ((v (elt triangles i)))
	  (destructuring-bind (x y z)
	      v
	    (unless (and (<= 0 x np1)
			 (<= 0 y np1)
			 (<= 0 z np1))
	      (error "face ~d=~a indexes non-existant point." i v))
	    (setf (aref s i) (make-vec-i :x x :y y :z z)))))
      s))

(declaim (type (simple-array vec-i 1) *faces*))

(defstruct plane 
  (normal (v 1d0) :type vec)
  (distance 0d0 :type double-float))

(defstruct face
  (vertex-indices (make-vec-i) :type vec-i)
  (normals (make-array 3 :element-type 'vec :initial-contents (v))
	   :type (simple-array vec (3)))
  (plane (make-plane) :type plane)
  (neighbour-indices (make-vec-i :x -1 :y -1 :z -1) :type vec-i)
  (visible nil :type boolean))

(defstruct shadowed-object
  (vertices *points* :type (simple-array vec 1))
  (faces (make-array 0 :element-type 'face
		     :initial-element (make-face)) 
	 :type (simple-array face 1)))

(defmacro do-edges ((i a b face) &body body)
  (let ((vs (gensym "vertex-indices")))
    `(with-slots ((,vs vertex-indices))
	 ,face
       (declare (type face ,face))
       (dotimes (,i 3)
	 (let ((,a (aref ,vs ,i))
	       (,b (aref ,vs (mod (1+ ,i) 3))))
	   ,@body)))))


;; pseudocode to find neighbours:
;; for each face (A) in the object
;;  for each edge in A
;;   if we don't know this edges neighbour yet
;;    for each face (B) in the object (except A)
;;     for each edge in B
;;      if A's edge is the same as B's edge,
;;        then they are neighbouring each other on that edge
;;        set the neighbour property for each face A and B, 
;;        then move onto next edge in A

(defun set-connectivity (obj)
  (declare (shadowed-object obj)
	   (values shadowed-object &optional))
  (format t "~a~%" '(los gehts))
  (with-slots (faces)
      obj
   (let ((n (length faces)))
     (dotimes (ia n) ;; go through every triangle faceA
       (format t "~a~%" (list 'ia ia 'of n))
       (let ((face-a (aref faces ia)))
	 (with-slots ((ns-a neighbour-indices))
	     face-a
	   (do-edges (edge-a a b face-a) ;; check each of the three edges
	     (when (eq -1 (aref ns-a edge-a)) ;; we don't know any neighbor
	       (dotimes (ib n) ;; go through all other triangles as faceB 
		 (format t "~a~%" (list 'ib ib))
		 (unless (eq ib ia)
		   (let ((face-b (aref faces ib)))
		     (do-edges (edge-b c d face-b) ;; iterate over all edges in B
		       (when (or (and (eq a c) (eq b d)) ;; two points of the edge
				 (and (eq a d) (eq b c)));; are shared by A and B
			 (format t "~a~%" (list 'found 'ia ia 'ib ib 'a a 'b b))
			 (setf (aref ns-a edge-a) ib
			       (aref (face-neighbour-indices face-b) edge-b) ia)
			 (go nextA))))))))))
       nextA)))
  obj)

(defmacro do-global-edges ((i a b face) &body body)
  `(dotimes (,i 3)
     (let ((,a (aref ,face ,i))
	   (,b (aref ,face (mod (1+ ,i) 3))))
       ,@body)))

#+nil
(loop for a across *faces* and i from 0 collect
     (list (incf i) a))

(defun get-connectivity ()
  #+nil (declare (values shadowed-object &optional))
  (loop for face-a across *faces* and ia from 0 do
       (do-global-edges (i a b face-a) ;; check each of the three edges
	 ;; we don't know any neighbor
	 (when (eq -1 (aref (aref *neighbors* ia) i))
	   (loop for face-b across *faces* and ib from 0 do
		(unless (eq ib ia)
		  (do-global-edges (j c d face-b) ;;iterate over all edges in B
		    ;; as all triangles are given in CCW if they share an edge
		    ;; the vertices are reverse
		    (when (and (eq a d) (eq b c))
		      (setf (aref (aref *neighbors* ia) i) ib
			    (aref (aref *neighbors* ib) j) ia))))))))
  nil)

;; for each edge of each face store the index to the neighboring face
(defparameter *neighbors*
  (make-array (length *faces*)
	      :element-type 'vec-i ;; using :initial-element caused UGLY bug
	      :initial-contents (loop for i below (length *faces*) collect
				     (make-vec-i :x -1 :y -1 :z -1))))
#+nil
(get-connectivity)

#+nil
(defparameter s (set-connectivity (make-object)))
;; u=3 v=1 is double tetraeder
(defun make-unit-sphere (u v)
  (declare (fixnum u v))
  (labels ((row-major-index (j i)
	     (+ i (* u j)))
	   (row-major-mod (j i)
	     (+ (mod i u) (* u j))))
   (let* ((n (+ 2 (* u v)))
	  (points (make-array n :element-type 'vec :initial-element (v)))
	  (nf (+ (* 2 u) (* 2 (* u (1- v)))))
	  (faces (make-array nf :element-type 'vec-i
			     :initial-element (make-vec-i))))
     (setf (aref points 0) (v 0d0 0d0 -1d0))
     (dotimes (i u)
       (dotimes (j v) ;; for v=1 the only theta is pi/2
	 (let ((theta (* (/ pi (+ v 1)) (- v j))) ;; pi-epsilon .. 0+epsilon
	       (phi (* 2 pi (/ i u))))	     ;; 0 .. 2 pi
	   (declare ((double-float 0d0 3.2d0) theta)
		    ((double-float 0d0 6.3d0) phi))
	   (setf (aref points (1+ (row-major-index j i))) ;; row-major order
		 (v (* (sin theta) (cos phi))
		    (* (sin theta) (sin phi))
		    (cos theta))))))
     (setf (aref points (1- n)) (v 0d0 0d0 1d0))
    
     (dotimes (i u) ;; the fan at the bottom tip
       #+nil (format t "~a~%" (list 'bottom i :x 0
				    :y (1+ (mod (1+ i) u)) :z (1+ i)))
       (setf (aref faces i)
	     (make-vec-i :x 0 :y (1+ (mod (1+ i) u)) :z (1+ i))))

     ;; connect the next circle to the circle above with 2*u triangles
     ;; use CCW ordering
     (let* ((offset (- (1- u)))
	   (point-offset (make-vec-i :x offset :y offset :z offset)))
      (loop for j from 1 below v do
	   (dotimes (i u)
	     (let ((tri1 (v+-i point-offset 
			      (make-vec-i 
			       :x (row-major-index j i) 
			       :y (row-major-mod j (1+ i)) 
			       :z (row-major-index (1+ j) i))))
		   (tri2 (v+-i point-offset 
			       (make-vec-i 
				:x (row-major-mod j (1+ i)) 
				:y (row-major-mod (1+ j) (1+ i)) 
				:z (row-major-index (1+ j) i))))
		   (face-index (+ u (* 2 (row-major-index (1- j) i)))))
#+nil	       (format t "~a~%~a~%" 
		       (list face-index tri1)
		       (list (1+ face-index) tri2))
	       (setf (aref faces face-index) tri1
		     (aref faces (1+ face-index)) tri2)))))

     (let ((point-offset (+ 1 (* u (1- v)))))
       (dotimes (i u) ;; the fan at the top tip
	 (let ((index (+ u (* 2 (* u (1- v))) i))
	       (tri (make-vec-i :x (1- n) 
				:y (+ i point-offset)
				:z (+ (mod (1+ i) u) point-offset))))
	   (format t "~a~%" (list ' top index tri))
	   (setf (aref faces index) tri))))
     (defparameter *faces* faces)
     (defparameter *points* points)
     nil)))
#+nil
(make-unit-sphere 3 1)

#+nil
(shadowed-object-faces (make-object))

#+nil
(defparameter s (make-object))

(defun face-normal (face)
  (declare (vec-i face)
	   (values vec &optional))
  (let* ((a (aref *points* (vec-i-x face)))
	 (b (aref *points* (vec-i-y face)))
	 (c (aref *points* (vec-i-z face)))
	 (n (cross (v- b a) (v- c a)))) ;; outward for CCW
    (normalize n)))

;; draw obj.vertices[obj.faces[n].vertex-indices]
(defun draw-global-object ()
  (declare (values null &optional))
  (with-primitive :triangles
    (loop for face across *faces* do
	 (let ((n (face-normal face)))
	  (dotimes (j 3)
	    (let ((p (aref *points* (aref face j))))
	      (normal (vec-x n) (vec-y n) (vec-z n))
	      (vertex (vec-x p) (vec-y p) (vec-z p)))))))
  nil)
#+nil
(make-unit-sphere 32 16)
#+nil
(run)

(defun make-object ()
  (let* ((o (make-shadowed-object))
	 (n (length *faces*))
	 (m (make-array n
			:element-type 'face
			:initial-element (make-face))))
    (dotimes (i n)
      (setf (face-vertex-indices (aref m i)) (aref *faces* i)))
    (setf (shadowed-object-faces o) m)
    o))

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
  (enable :cull-face)
  (material :front :ambient-and-diffuse #(0.8 0.1 0.0 1.0))
  (light :light0 :position #(10d0 10d0 10d0 0d0))
  (enable :lighting :light0 :depth-test)
  (draw-global-object)
  
  #+nil 
  (with-pushed-matrix
    (scale .02 .02 .02)
    (with-primitive :triangles
      (with-pushed-matrix
	(scale .01 .01 .01)
	(dotimes (i 50)
	  (draw-triangle (get-triangle i))))))
  (disable :lighting :light0))
#+nil
(run)

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

(defun run ()
  (display-window (make-instance 'fenster :mode '(:double :rgb :depth))))
 
#+nil
(run)

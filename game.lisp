(load "main.lisp")

(defparameter car-pos (make-rl-vector2 :x (float (/ 1024 2)) :y (float (/ 768 2))))

(defclass entity ()
  ((position
    :initarg :position
    :accessor entity-position
    :initform (make-rl-vector2))
   (name
    :initarg :name
    :accessor entity-name
    :initform "")
   (id
    :initarg :id
    :accessor entity-id
    :initform (gensym "eid#"))
   (update
    :initarg :update
    :accessor entity-update
    :initform nil)
   (texture
    :initarg :texture
    :accessor entity-texture
    :initform nil)))

(defmethod draw (entity &key (draw-bb nil))
  ;; (format t "Drawing entity ~A~%" (entity-position entity))
  (rl-draw-texture-ex
   (entity-texture entity)
   (entity-position entity)
   0.0
   1.0
   (make-rl-color :r 255 :g 0 :b 0 :a 255))
  (if draw-bb
      (let* ((pos (entity-position entity))
	     (x (rl-vector2-x pos))
	     (y (rl-vector2-y pos)))
	(rl-draw-rectangle-lines (round x) (round y) 100 100 (make-rl-color :r 255 :g 0 :b 0 :a 255)))))

(defparameter *entities*
  (make-array 10
	      :fill-pointer 0
	      :adjustable t
	      :initial-element nil))

(defvar *clear-color* (make-rl-color :r 0 :g 0 :b 0 :a 0))

(defun get-keyboard-map ()
  (logior
   (if (rl-is-key-down (cffi:foreign-enum-value 'rl-keyboard-key :down)) #b0001 0)
   (if (rl-is-key-down (cffi:foreign-enum-value 'rl-keyboard-key :up)) (ash 1 1) 0)
   (if (rl-is-key-down (cffi:foreign-enum-value 'rl-keyboard-key :right)) (ash 1 2) 0)
   (if (rl-is-key-down (cffi:foreign-enum-value 'rl-keyboard-key :left)) (ash 1 3) 0)))
   

(defun main ()
  (unwind-protect
       (progn
	 (format t "Welcome to the game")
	 (rl-init-window 1024 768 "Test CFFI")
    
	 (rl-set-target-fps 60)

	 (vector-push-extend
	  (make-instance 'entity
			 :position (make-rl-vector2 :x (float 100) :y (float 100))
			 :name "Player"
			 :texture (rl-load-texture "car.png")
			 :update (lambda (entity)
				   (let ((pos (entity-position entity))
					 (offset 3)
					 (control-keys (get-keyboard-map)))
				     (when (logbitp 0 control-keys) (setf (rl-vector2-y pos) (+ (rl-vector2-y pos) offset)))
				     (when (logbitp 1 control-keys) (setf (rl-vector2-y pos) (- (rl-vector2-y pos) offset)))
				     (when (logbitp 2 control-keys) (setf (rl-vector2-x pos) (+ (rl-vector2-x pos) offset)))
				     (when (logbitp 3 control-keys) (setf (rl-vector2-x pos) (- (rl-vector2-x pos) offset)))

				     ;; If no keyboard is pressed move down
				     (if (= control-keys 0)
					 (incf (rl-vector2-y pos)))
				     (setf (entity-position entity) pos))
				   ))
	  *entities*)
	 
	 (loop while (not (rl-window-should-close))
	       do
		  (unwind-protect
		       (progn
			 (loop for e across *entities*
			       do (funcall (entity-update e) e))
			 
			 (rl-begin-drawing)
			 (rl-clear-background *clear-color*)

			 (loop for e across *entities*
			       do (draw e :draw-bb t)))
		    
		    (rl-end-drawing)))
    (rl-close-window))))

(main)

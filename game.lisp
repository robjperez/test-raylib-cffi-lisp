(load "main.lisp")

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
    :initform (lambda ()))
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
	(rl-draw-rectangle-lines (round x) (round y) 64 64 (make-rl-color :r 255 :g 0 :b 0 :a 255)))))

(defparameter *entities*
  (make-array 10
	      :fill-pointer 0
	      :adjustable t
	      :initial-element nil))

(defvar *clear-color* (make-rl-color :r 0 :g 0 :b 0 :a 0))
(defvar *shipR* (rl-load-texture "shipR.png"))
(defvar *shipL* (rl-load-texture "shipL.png"))

(defun get-keyboard-map ()
  (logior
   (if (rl-is-key-down (cffi:foreign-enum-value 'rl-keyboard-key :down)) #b0001 0)
   (if (rl-is-key-down (cffi:foreign-enum-value 'rl-keyboard-key :up)) (ash 1 1) 0)
   (if (rl-is-key-down (cffi:foreign-enum-value 'rl-keyboard-key :right)) (ash 1 2) 0)
   (if (rl-is-key-down (cffi:foreign-enum-value 'rl-keyboard-key :left)) (ash 1 3) 0)
   (if (rl-is-key-down (cffi:foreign-enum-value 'rl-keyboard-key :rctrl)) (ash 1 4) 0)))
   

(defun create-enemy ()
  (let ((enemy (make-instance 'entity
			      :position (make-rl-vector2 :x (float (random 1024)) :y 0.0)
			      :name (format nil "Enemy~A" (gensym))
			      :texture (rl-load-texture "car.png")
			      :update (lambda (entity)
					(let* ((offset 3)
					       (pos (entity-position entity)))
					  (setf (rl-vector2-y pos) (+ (rl-vector2-y pos) offset))
					  (setf (entity-position entity) pos))))))
    (vector-push-extend enemy *entities*)))

(defun player-fire (player-position)
  (let ((fire-entity (make-instance 'entity
				    :position (make-rl-vector2 :x (rl-vector2-x player-position) :y (rl-vector2-y player-position))
				    :name "Player Fire"
				    :texture (rl-load-texture "laser.png"))))
    (vector-push-extend fire-entity *entities*)))

(defun check-entity-out-of-screen (entity)
  nil)

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
			 :texture *shipR*
			 :update (lambda (entity)
				   (let ((pos (entity-position entity))
					 (offset 3)
					 (control-keys (get-keyboard-map)))
				     (when (logbitp 0 control-keys) (setf (rl-vector2-y pos) (+ (rl-vector2-y pos) offset)))
				     (when (logbitp 1 control-keys) (setf (rl-vector2-y pos) (- (rl-vector2-y pos) offset)))
				     (when (logbitp 2 control-keys)
				       (setf (rl-vector2-x pos) (+ (rl-vector2-x pos) offset))
				       (setf (entity-texture entity) *shipL*))
				     (when (logbitp 3 control-keys)
				       (setf (rl-vector2-x pos) (- (rl-vector2-x pos) offset))
				       (setf (entity-texture entity) *shipR*))
				     (when (logbitp 4 control-keys) (player-fire pos))

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
			 ;; Create enemies randomly
			 ;; (if (= 0 (mod (random 14) 5))
			 ;;     (create-enemy))
			 
			 (loop for e across *entities*
			       do (funcall (entity-update e) e))
			 
			 (rl-begin-drawing)
			 
			 (rl-clear-background *clear-color*)

			 (loop for e across *entities*
			       do
				  ;; Check if instances are out of the screen and remove them
				  (if (check-entity-out-of-screen e)
				      nil)
				  ;; Draw the pending ones
				  (draw e :draw-bb t))
			 
			 )
		    
		    (rl-end-drawing)))
	 (rl-close-window))))

(main)


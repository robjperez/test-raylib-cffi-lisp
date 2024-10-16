(ql:quickload "cffi")
(ql:quickload "cffi-libffi")

(cffi:define-foreign-library raylib
  (:unix (:or "./libraylib.so.5.5.0"))
  (t (:default "libraylib")))

(cffi:use-foreign-library raylib)

(cffi:defcstruct color
  (r :unsigned-char)
  (g :unsigned-char) 
  (b :unsigned-char) 
  (a :unsigned-char))

(cffi:defcstruct vector2
  (x :float)
  (y :float))

(cffi:defcstruct image
  (data :pointer)
  (width :int)
  (height :int)
  (mipmaps :int)
  (format :int))

(cffi:defcstruct texture2d
  (id :uint)
  (width :int)
  (height :int)
  (mipmaps :int)
  (format :int))

(cffi:defcenum (rl-keyboard-key :int)
  (:up 265)
  (:down 264)
  (:left 263)
  (:right 262))
    

(cffi:defcfun
    ("InitWindow" rl-init-window)
    :void
  (width :int )
  (height :int)
  (title :string))

(cffi:defcfun
    ("SetTargetFPS" rl-set-target-fps)
    :void
  (fps :int))

(cffi:defcfun
    ("CloseWindow" rl-close-window)
    :void)

(cffi:defcfun
    ("WindowShouldClose" rl-window-should-close)
    :bool)

(cffi:defcfun
    ("BeginDrawing" rl-begin-drawing)
    :void)

(cffi:defcfun
    ("EndDrawing" rl-end-drawing)
    :void)

(cffi:defcfun
    ("ClearBackground" rl-clear-background)
    :void
  (color (:struct color)))

(cffi:defcfun
    ("DrawPixel" rl-draw-pixel)
    :void
  (posX :int)
  (posY :int)
  (color (:struct color)))

(cffi:defcfun
    ("DrawRectangle" rl-draw-rectangle)
    :void
  (posX :int)
  (posY :int)
  (width :int)
  (height :int)
  (color (:struct color)))

(cffi:defcfun
    ("LoadImage" rl-load-image)
    (:struct image)
  (filename :string))

(cffi:defcfun
    ("LoadTexture" rl-load-texture)
    (:struct texture2d)
  (filename :string))

(cffi:defcfun
    ("DrawTexture" rl-draw-texture)
    :void
  (texture (:struct texture2d))
  (posX :int)
  (posY :int)
  (tint (:struct color)))

(cffi:defcfun
    ("DrawTextureEx" rl-draw-texture-ex)
    :void
  (texture (:struct texture2d))
  (position (:struct vector2))
  (rotation :float)
  (scale :float)
  (tint (:struct color)))

(cffi:defcfun
    ("IsKeyDown" rl-is-key-down)
    :bool
  (key :int))

(cffi:defcfun
    ("DrawText" rl-draw-text)
    :void
  (text :string)
  (posX :int)
  (posY :int)
  (fontSize :int)
  (color (:struct color)))

(defstruct vector-2 x y)
(defparameter car-pos (make-vector-2 :x (float (/ 1024 2)) :y (float (/ 768 2))))

(defun main ()
  (rl-init-window 1024 768 "Test CFFI")

  (rl-set-target-fps 60)

  (loop while (not (rl-window-should-close))
	do
	   (when (rl-is-key-down (cffi:foreign-enum-value 'rl-keyboard-key :left))
	     (setf (vector-2-x car-pos) (- (vector-2-x car-pos) 3)))
	   (when (rl-is-key-down (cffi:foreign-enum-value 'rl-keyboard-key :right))
	     (setf (vector-2-x car-pos) (+ (vector-2-x car-pos) 3)))
	   (when (rl-is-key-down (cffi:foreign-enum-value 'rl-keyboard-key :up))
	     (setf (vector-2-y car-pos) (- (vector-2-y car-pos) 3)))
	   (when (rl-is-key-down (cffi:foreign-enum-value 'rl-keyboard-key :down))
	     (setf (vector-2-y car-pos) (+ (vector-2-y car-pos) 3)))

	   (rl-begin-drawing)
	   
	   (cffi:with-foreign-object (clear-color '(:struct color))
	     (setf 
	      (cffi:foreign-slot-value clear-color '(:struct color) 'r) 0
	      (cffi:foreign-slot-value clear-color '(:struct color) 'g) 0
	      (cffi:foreign-slot-value clear-color '(:struct color) 'b) 0
	      (cffi:foreign-slot-value clear-color '(:struct color) 'a) 0)
	     (rl-clear-background (cffi:mem-ref clear-color '(:struct color))))

	   ;; (cffi:with-foreign-object (pixel-color '(:struct color))
	   ;;   (setf 
	   ;;    (cffi:foreign-slot-value pixel-color '(:struct color) 'r) (random 255)
	   ;;    (cffi:foreign-slot-value pixel-color '(:struct color) 'g) (random 255)
	   ;;    (cffi:foreign-slot-value pixel-color '(:struct color) 'b) (random 255)
	   ;;    (cffi:foreign-slot-value pixel-color '(:struct color) 'a) 255)
	   ;;   (rl-draw-rectangle (random 1024) (random 768) 20 20 (cffi:mem-ref pixel-color '(:struct color))))

	   (let ((car-sprite (rl-load-texture "car.png")))
	     (cffi:with-foreign-object (rl-car-pos '(:struct vector2))
	       (setf
		(cffi:foreign-slot-value rl-car-pos '(:struct vector2) 'x) (vector-2-x car-pos)
		(cffi:foreign-slot-value rl-car-pos '(:struct vector2) 'y) (vector-2-y car-pos))

	       (cffi:with-foreign-object (tint-color '(:struct color))
		 (setf 
		  (cffi:foreign-slot-value tint-color '(:struct color) 'r) 255
		  (cffi:foreign-slot-value tint-color '(:struct color) 'g) 0
		  (cffi:foreign-slot-value tint-color '(:struct color) 'b) 0
		  (cffi:foreign-slot-value tint-color '(:struct color) 'a) 255)		 
		 
		 (rl-draw-texture-ex car-sprite
				     (cffi:mem-ref rl-car-pos '(:struct vector2))
				     0.0
				     1.0
				     (cffi:mem-ref tint-color '(:struct color))))))
	   
	   (cffi:with-foreign-object (tint-color '(:struct color))
		 (setf 
		  (cffi:foreign-slot-value tint-color '(:struct color) 'r) 255
		  (cffi:foreign-slot-value tint-color '(:struct color) 'g) 255
		  (cffi:foreign-slot-value tint-color '(:struct color) 'b) 255
		  (cffi:foreign-slot-value tint-color '(:struct color) 'a) 255)
	     (rl-draw-text "Raylib lisp game engine" 100 100 20 (cffi:mem-ref tint-color '(:struct color))))
	   
	     
	   (rl-end-drawing))
  (rl-close-window))


(main)

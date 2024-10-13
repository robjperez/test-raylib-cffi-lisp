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

(defun main ()
  (rl-init-window 1024 768 "Test CFFI")

  (rl-set-target-fps 1)

  (loop while (not (rl-window-should-close))
	do
	   (rl-begin-drawing)
	   
	   ;; (cffi:with-foreign-object (clear-color '(:struct color))
	   ;;   (setf 
	   ;;    (cffi:foreign-slot-value clear-color '(:struct color) 'r) 0
	   ;;    (cffi:foreign-slot-value clear-color '(:struct color) 'g) 0
	   ;;    (cffi:foreign-slot-value clear-color '(:struct color) 'b) 0
	   ;;    (cffi:foreign-slot-value clear-color '(:struct color) 'a) 0)
	   ;;   (rl-clear-background (cffi:mem-ref clear-color '(:struct color))))

	   (cffi:with-foreign-object (pixel-color '(:struct color))
	     (setf 
	      (cffi:foreign-slot-value pixel-color '(:struct color) 'r) (random 255)
	      (cffi:foreign-slot-value pixel-color '(:struct color) 'g) (random 255)
	      (cffi:foreign-slot-value pixel-color '(:struct color) 'b) (random 255)
	      (cffi:foreign-slot-value pixel-color '(:struct color) 'a) 255)
	     (rl-draw-rectangle (random 1024) (random 768) 20 20 (cffi:mem-ref pixel-color '(:struct color))))
			  
	   (rl-end-drawing))
  (rl-close-window))


(main)

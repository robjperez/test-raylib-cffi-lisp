(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cffi")
  (ql:quickload "cffi-libffi"))

;; Remember run sbcl/sly from the same folder where the .so file is
(cffi:define-foreign-library raylib
  (:unix (:or "./libraylib.so.5.5.0"))
  (t (:default "libraylib")))

(cffi:use-foreign-library raylib)

;; --- Color wrapper ----
(cffi:defcstruct (%color :class color-type) 
  (r :unsigned-char)
  (g :unsigned-char) 
  (b :unsigned-char) 
  (a :unsigned-char))

(defstruct rl-color
  (r 0)
  (g 0)
  (b 0)
  (a 0))
  
(defmethod cffi:expand-into-foreign-memory (value (type color-type) ptr)
  `(cffi:with-foreign-slots ((r g b a) ,ptr (:struct %color))
     (setf r (rl-color-r ,value)
	   g (rl-color-g ,value)
	   b (rl-color-b ,value)
	   a (rl-color-a ,value))))

(defmethod cffi:translate-into-foreign-memory (value (type color-type) ptr)
  (cffi:with-foreign-slots ((r g b a) ptr (:struct %color))
    (setf r (rl-color-r value)
	  g (rl-color-g value)
	  b (rl-color-b value)
	  a (rl-color-a value))))

;; ---- Vector2 wrapper ----
(cffi:defcstruct (%vector2 :class vector2-type)
  (x :float)
  (y :float))

(defstruct rl-vector2
  (x 0)
  (y 0))

(defmethod cffi:expand-into-foreign-memory (value (type vector2-type) ptr)
  `(cffi:with-foreign-slots ((x y) ,ptr (:struct %vector2))
     (setf x (rl-vector2-x ,value)
	   y (rl-vector2-y ,value))))

(defmethod cffi:translate-into-foreign-memory (value (type vector2-type) ptr)
  (cffi:with-foreign-slots ((x y) ptr (:struct %vector2))
    (setf x (rl-vector2-x value)
	  y (rl-vector2-y value))))

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
  (:right 262)
  (:rctrl 345))
    

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
  (color (:struct %color)))

(cffi:defcfun
    ("DrawPixel" rl-draw-pixel)
    :void
  (posX :int)
  (posY :int)
  (color (:struct %color)))

(cffi:defcfun
    ("DrawRectangle" rl-draw-rectangle)
    :void
  (posX :int)
  (posY :int)
  (width :int)
  (height :int)
  (color (:struct %color)))

(cffi:defcfun
    ("DrawRectangleLines" rl-draw-rectangle-lines)
    :void
  (posX :int)
  (posY :int)
  (width :int)
  (height :int)
  (color (:struct %color)))


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
  (tint (:struct %color)))

(cffi:defcfun
    ("DrawTextureEx" rl-draw-texture-ex)
    :void
  (texture (:struct texture2d))
  (position (:struct %vector2))
  (rotation :float)
  (scale :float)
  (tint (:struct %color)))

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
  (color (:struct %color)))

(in-package #:cl-user)
(in-package #:asdf)

(defsystem #:game-engine
  :version "0.1"
  :description "Simple game engine"
  :author "Hedin"
  :depends-on (#:cl-opengl
	       #:lispbuilder-sdl
	       #:lispbuilder-sdl-image
	       #:lispbuilder-sdl-ttf
	       #:cl-openal
	       #:cl-alut)
  :components ((:file "package")
	       (:file "core" :depends-on ("package"
					  "graphics"
					  "sound"))
	       (:file "graphics" :depends-on ("package"
					      "color"
					      "rectangle"
					      "vector-2"))
	       (:file "color" :depends-on ("package"))
	       (:file "rectangle" :depends-on ("package" "vector-2"))
	       (:file "vector-2" :depends-on ("package"))
	       (:file "sound" :depends-on ("package"))
	       (:file "utils" :depends-on ("package"))
	       (:file "math-helper" :depends-on ("package"))))
	       

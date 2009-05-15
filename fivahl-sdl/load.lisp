#+SBCL
(require :asdf)

#-SBCL
(load "../lispbuilder-sdl/system/asdf.lisp")

(pushnew "../lispbuilder-sdl/system/alexandria/" asdf:*central-registry*)
(pushnew "../lispbuilder-sdl/system/babel/" asdf:*central-registry*)
(pushnew "../lispbuilder-sdl/system/trivial-features/" asdf:*central-registry*)
(pushnew "../lispbuilder-sdl/system/cffi_0.10.4/" asdf:*central-registry*)
(pushnew "../lispbuilder-sdl/system/lispbuilder-read-only/lispbuilder-sdl/" asdf:*central-registry*)
;;(pushnew "../lispbuilder-sdl/system/lispbuilder-read-only/lispbuilder-sdl-mixer/" asdf:*central-registry*)

;;(asdf:operate 'asdf:load-op :cffi)
;;
;;(asdf:operate 'asdf:load-op :lispbuilder-sdl)

;;(asdf:operate 'asdf:load-op :lispbuilder-sdl-examples)

(asdf:operate 'asdf:load-op :fivahl-sdl)


(fivahl-sdl:main-loop 800 600)

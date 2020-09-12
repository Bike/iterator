(defsystem :iterator
  :depends-on ()
  :components
  ((:file "packages")
   (:file "protocol" :depends-on ("packages"))
   (:file "cl" :depends-on ("protocol" "packages"))
   (:file "util" :depends-on ("packages"))
   (:file "functions" :depends-on ("packages"))
   (:file "do" :depends-on ("protocol" "packages"))
   (:file "default-implementations"
    :depends-on ("do" "util" "functions" "protocol" "packages"))
   (:file "object" :depends-on ("protocol" "packages"))
   (:file "map" :depends-on ("object" "packages"))
   (:file "map-macros" :depends-on ("map" "util" "do" "protocol" "packages"))))

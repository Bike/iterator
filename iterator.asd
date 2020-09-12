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
    :depends-on ("do" "util" "functions" "protocol" "packages"))))

(asdf:defsystem #:svdba
  :version      "0.0.1"
  :author       "rigidus <i.am.rigidus@gmail.com>"
  :licence      "GPLv3"
  :description  "wedding site"
  :depends-on   (#:cl-mysql
                 #:cl-store
                 #:restas
                 #:restas-directory-publisher
                 #:cl-json
                 #:cl-ppcre
                 #:cl-smtp
                 #:cl-mime
                 #:arnesi
                 #:closer-mop
                 #:drakma)
  :serial        t
  :components   ((:file "lib")
                 (:file "render")
                 (:file "perm")
                 (:file "defmodule")
                 (:file "init")
                 (:static-file "README")
                 (:static-file "svdba.asd")
                 (:module "tpl"
                          :serial t
                          ;; :pathname ""
                          :components ((:static-file "root.htm")
                                       ))))





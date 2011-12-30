(asdf:defsystem #:svdba
    :version      "0.0.2"
    :author       "rigidus <i.am.rigidus@gmail.com>"
    :licence      "GPLv3"
    :description  "site http://svdba.ru"
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
    :serial       t
    :components   ((:file "lib")
                   (:file "render")
                   (:file "perm")
                   (:file "defmodule")
                   (:file "init")
                   (:module "tpl"
                            :serial t
                            :components ((:static-file "root.htm")))
                   ;; (:static-file "templates.htm")
                   ;; (:file "defmodule")
                   ;; (:file "orgmode")
                   ;; (:file "sape")
                   ;; (:file "render")
                   ;; (:file "routes")
                   ;; (:file "init")
                   ))





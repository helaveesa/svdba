(asdf:defsystem #:svdba
  :version      "0.0.2"
  :author       "rigidus <i.am.rigidus@gmail.com>"
  :licence      "GPLv3"
  :description  "site http://svdba.ru"
  :depends-on   (#:cl-ppcre
                 #:restas-directory-publisher
                 #:closure-template)
  :serial       t
  :components   ((:file "defmodule")
                 ;; (:file "orgmode")
                 ;; (:file "sape")
                 (:file "render")
                 (:file "routes")
                 (:file "init")
                 (:module "tpl"
                          :serial t
                          :components ((:static-file "root.htm")))))





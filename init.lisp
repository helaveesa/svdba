(in-package #:svdba)

(connect :user "root" :password *db-password* :database "ktopostavlyaet")
(query "SET NAMES utf8")


(restas:start '#:svdba :port 8082)

(restas:debug-mode-on)


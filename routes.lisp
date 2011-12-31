(in-package #:svdba)


;; 404

(defun page-404 (&optional (title "404 Not Found") (content "Страница не найдена"))
  (tpl:root
   (list :headtitle "404 Not Found"
         :headbox "headbox"
         :content "Страница не найдена"
         :footer "footer")))

(restas:define-route not-found-route ("*any")
  (restas:abort-route-handler
   (page-404)
   :return-code hunchentoot:+http-not-found+
   :content-type "text/html"))


;; main

(restas:define-route main ("/")
  (tpl:root
   (list :headtitle "headtitle"
         :headbox "headbox"
         :content (tpl:content)
         :footer "footer")))


(restas:mount-submodule -css- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("css"))
  (restas.directory-publisher:*directory* (path "css/")))

(restas:mount-submodule -img- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("img"))
  (restas.directory-publisher:*directory* (path "img/")))


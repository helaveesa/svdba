(in-package #:svdba)


;; 404

(defun page-404 (&optional (title "404 Not Found") (content "Страница не найдена"))
  (tpl:root
   (list :headtitle "404 Not Found"
         :headbox "headbox"
         :content content
         :footer "footer")))

(restas:define-route not-found-route ("*any")
  (restas:abort-route-handler
   (page-404)
   :return-code hunchentoot:+http-not-found+
   :content-type "text/html"))

(restas:debug-mode-on)

(restas:define-route main ("/")
  (path "content/index.org"))


(restas:define-route nevesta ("/nevesta/")
  (path "content/nevesta.org"))

(restas:define-route groom ("/groom/")
  (path "content/groom.org"))


(restas:define-route plan ("/plan/")
  (path "content/plan.org"))

(restas:define-route servis ("/servis/")
  (path "content/servis.org"))

(restas:define-route love ("/love/")
  (path "content/love.org"))


(restas:define-route history ("/history/")
  (path "content/history.org"))

(restas:define-route historyvenchaniy ("/historyvenchaniy/")
  (path "content/historyvenchaniy.org"))

(restas:define-route history-cerkovny-brak ("/history-cerkovny-brak/")
  (path "content/history-cerkovny-brak.org"))

(restas:define-route history-obruchenie ("/history-obruchenie/")
  (path "content/history-obruchenie.org"))

(restas:define-route history-mendelson-march ("/history-mendelson-march/")
  (path "content/history-mendelson-march.org"))

(restas:define-route history-honeymoon ("/history-honeymoon/")
  (path "content/history-honeymoon.org"))



(restas:define-route gallery ("/gallery/")
  (path "content/gallery.org"))

(restas:define-route about ("/about/")
  (path "content/about.org"))



(restas:mount-submodule -css- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("css"))
  (restas.directory-publisher:*directory* (path "css/")))

(restas:mount-submodule -js- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("js"))
  (restas.directory-publisher:*directory* (path "js/")))

(restas:mount-submodule -img- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("img"))
  (restas.directory-publisher:*directory* (path "img/")))


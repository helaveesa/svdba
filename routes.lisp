(in-package #:svdba)

;; branchtest
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

(restas:define-route son ("/son/")
  (path "content/son.org"))



(restas:define-route nevesta ("/nevesta/")
  (path "content/nevesta.org"))

(restas:define-route dress ("/dress/")
  (path "content/dress.org"))
(restas:define-route svdba-shouse ("/svdba-shouse/")
  (path "content/svdba-shouse.org"))
(restas:define-route makeup ("/makeup/")
  (path "content/makeup.org"))
(restas:define-route obruch-ring ("/obruch-ring/")
  (path "content/obruch-ring.org"))
(restas:define-route zags-nevesta ("/zags-nevesta/")
  (path "content/zags-nevesta.org"))
(restas:define-route svadeb-maniqur ("/svadeb-maniqur/")
  (path "content/svadeb-maniqur.org"))
(restas:define-route dress-friend ("/dress-friend/")
  (path "content/dress-friend.org"))



(restas:define-route groom ("/groom/")
  (path "content/groom.org"))

(restas:define-route dress-groom ("/dress-groom/")
  (path "content/dress-groom.org"))
(restas:define-route svdba-shouse-groom ("/svdba-shouse-groom/")
  (path "content/svdba-shouse-groom.org"))
(restas:define-route costum ("/costum/")
  (path "content/costum.org"))
(restas:define-route obruch-ring-groom ("/obruch-ring-groom/")
  (path "content/obruch-ring-groom.org"))
(restas:define-route zags-groom ("/zags-groom/")
  (path "content/zags-groom.org"))
(restas:define-route svadeb-maniqur-groom ("/svadeb-maniqur-groom/")
  (path "content/svadeb-maniqur-groom.org"))



(restas:define-route plan ("/plan/")
  (path "content/plan.org"))
(restas:define-route plan-budjet ("/plan-budjet/")
  (path "content/plan-budjet.org"))
(restas:define-route plan-rinok ("/plan-rinok/")
  (path "content/plan-rinok.org"))
(restas:define-route plan-atribut ("/plan-atribut/")
  (path "content/plan-atribut.org"))
(restas:define-route plan-svideteli ("/plan-svideteli/")
  (path "content/plan-svideteli.org"))
(restas:define-route plan-svadeb ("/plan-svadeb/")
  (path "content/plan-svadeb.org"))



(restas:define-route servis ("/servis/")
  (path "content/servis.org"))
(restas:define-route servis-book ("/servis-book/")
  (path "content/servis-book.org"))
(restas:define-route servis-plan ("/servis-plan/")
  (path "content/servis-plan.org"))
(restas:define-route servis-gests ("/servis-gests/")
  (path "content/servis-gests.org"))
(restas:define-route servis-song ("/servis-song/")
  (path "content/servis-song.org"))
(restas:define-route servis-fant ("/servis-fant/")
  (path "content/servis-fant.org"))



(restas:define-route love ("/love/")
  (path "content/love.org"))

(restas:define-route vpechatleniy ("/vpechatleniy/")
  (path "content/vpechatleniy.org"))
(restas:define-route advice ("/advice/")
  (path "content/advice.org"))
(restas:define-route year ("/year/")
  (path "content/year.org"))
(restas:define-route advice-useful ("/advice-useful/")
  (path "content/advice-useful.org"))
(restas:define-route aforizm ("/aforizm/")
  (path "content/aforizm.org"))



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
(restas:define-route gallery-nevest ("/gallery-nevest/")
  (path "content/gallery-nevest.org"))
(restas:define-route gallery-groom ("/gallery-groom/")
  (path "content/gallery-groom.org"))



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


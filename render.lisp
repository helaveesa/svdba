(in-package #:svdba)

(defclass svdba-render () ())

(setf *default-render-method* (mi 'svdba-render))

(defmethod restas:render-object ((designer svdba-render) (acts list))
  (tpl:root
   (list :headtitle "svdba project"
         :headbox "headbox"
         :content (tpl:content)
         :footer "footer")))

;; before start-session
(defmethod restas:render-object :before ((designer svdba-render) (acts list))
  (hunchentoot:start-session))


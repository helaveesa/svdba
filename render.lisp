(in-package #:svdba)

(defclass svdba-render () ())

(setf *default-render-method* (make-instance 'svdba-render))

(defmethod restas:render-object ((designer svdba-render) (data list))
  (tpl:root data))

(defmethod restas:render-object ((designer svdba-render) (file pathname))
  (if (string= (pathname-type file) "org")
      (restas:render-object designer (parse-org file))
      (call-next-method)))


(defmethod restas:render-object ((designer svdba-render) (data orgdata))
  (let* ((content (orgdata-content data))
         (sections (orgdata-sections data))
         (directives (orgdata-directives data)))
    (cond ((equal (getf directives :pagetype) "index")
           (setf (getf directives :opt2) t
                 (getf directives :container) "index"))
          ((equal (getf directives :pagetype)  "photo")
           (setf (getf directives :topmenu) t
                 (getf directives :opt5) t))
          ((equal (getf directives :pagetype)  "text")
           (setf (getf directives :topmenu) t
                 (getf directives :leftimg) t
                 (getf directives :intext) t)))
    (restas:render-object
     designer
     (list :headtitle  (getf directives :title)
           :opt2       (getf directives :opt2)
           :opt5       (getf directives :opt5)
           :container  (getf directives :container)
           :topmenu    (getf directives :topmenu)
           :leftimg    (getf directives :leftimg)
           :intext     (getf directives :intext)
           :navpoints  (menu)
           ;; :links (get-sape-links (hunchentoot:REQUEST-URI*))
           :content    content))))

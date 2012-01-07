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
           (setf (getf directives :selector2) t
                 (getf directives :container) "index"))
          ((equal (getf directives :pagetype)  "photo")
           (setf (getf directives :topmenu) t
                 (getf directives :selector5) t))
          ((equal (getf directives :pagetype)  "text")
           (setf (getf directives :topmenu) t)))
    (restas:render-object
     designer
     (list :headtitle  (getf directives :title)
           :selector2  (getf directives :selector2)
           :selector5  (getf directives :selector5)
           :container  (getf directives :container)
           :topmenu    (getf directives :topmenu)
           :leftimg    (getf directives :leftimg)
           :intext     (getf directives :intext)
           :leftlink   (getf directives :leftlink)
           :rightlink  (getf directives :rightlink)
           :lefttitle  (getf directives :lefttitle)
           :righttitle (getf directives :righttitle)
           :opt0id     (getf directives :opt0id)
           :opt1id     (getf directives :opt1id)
           :opt2id     (getf directives :opt2id)
           :opt3id     (getf directives :opt3id)
           :opt4id     (getf directives :opt4id)
           :opt0title  (getf directives :opt0title)
           :opt1title  (getf directives :opt1title)
           :opt2title  (getf directives :opt2title)
           :opt3title  (getf directives :opt3title)
           :opt4title  (getf directives :opt4title)
           :navpoints  (menu)
           ;; :links (get-sape-links (hunchentoot:REQUEST-URI*))
           :content    content))))

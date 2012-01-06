(in-package #:svdba)

(defclass svdba-render () ())

(setf *default-render-method* (make-instance 'svdba-render))


;; (defmethod restas:render-object ((designer svdba-render) (acts t))
;;   acts)

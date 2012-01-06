(require 'restas)
(require 'closure-template)
(require 'restas-directory-publisher)
(require 'cl-base64)

(restas:define-module #:svdba
    (:use #:cl #:iter #:alexandria))

(in-package #:svdba)


(let ((path '(:RELATIVE "repo/svdba")))
  (setf asdf:*central-registry*
        (remove-duplicates (append asdf:*central-registry*
                                   (list (merge-pathnames
                                          (make-pathname :directory path)
                                          (user-homedir-pathname))))
                           :test #'equal)))

(defparameter *basedir*
  (asdf:component-pathname (asdf:find-system '#:svdba)))

(defun path (relative)
  (merge-pathnames relative *basedir*))

(closure-template:compile-template :common-lisp-backend (path "tpl/root.htm"))
(closure-template:compile-template :common-lisp-backend (path "tpl/content.htm"))


(defclass orgdata ()
  ((content :accessor orgdata-content)
   (sections :accessor orgdata-sections)
   (directives :accessor orgdata-directives)))

;; (defparameter *articles* (make-hash-table :test #'equal))
;; (defparameter *cached-articles-page* nil)

;; (defparameter *aliens* (make-hash-table :test #'equal))
;; (defparameter *cached-alien-page* nil)



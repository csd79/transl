;;;; -*- Mode#: Common-Lisp; Author#: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:cl-user)


(defpackage #:rewrite
  (:use #:cl #:str #:achar)
  (:export #:*rewriters*
           #:rewrite))

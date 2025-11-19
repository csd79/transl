;;;; -*- Mode#: Common-Lisp; Author#: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:cl-user)


(defpackage #:transl
  (:use #:cl #:str #:achar)
  (:export #:synonymp
           #:canoninc
           #:defrew
           #:with-transl
           #:transl
           ))

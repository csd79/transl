;;;; -*- Mode#: Common-Lisp; Author#: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:cl-user)


(defpackage #:transl
  (:use #:cl #:str #:achar)
  (:export #:load-definitions
           #:synonymp
           #:canonincal
           #:deftranslators
           #:with-transl
           #:transl))

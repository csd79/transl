;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:rewrite)


;;; ----------------------------------------------------------------------
;;; Synonyms mechanisms


(defparameter *synonyms* nil)


(defmacro with-synonyms ((list) &body body)
  `(let ((*synonyms* ,list))
     ,@body))


(defun syn= (str1 str2 &key (test #'astring=))
  "Compare 2 strings using a synonyms dictionary."
  (let* ((candidates (apply #'append (remove-if-not
                                      #'(lambda (row)
                                          (member str1 row :test test))
                                      *synonyms*)))
         (result     (member str2 candidates :test test)))
    (logger "~&'~a' and '~a' resolved as ~asynonyms.~%"
            str1 str2 (if result "" "not "))
    (when result t)))


(defun syncanon (string &key (test #'achar=) (canonical #'first))
  "Return canonical form if synonym STRING."
  (let* ((row (find-if #'(lambda (syn-row)
                           (some #'(lambda (elem)
                                     (search string elem :test test))
                                 syn-row))
                       *synonyms* :from-end t))
         (result (when row (funcall canonical row))))
    (logger "~&The canonical form of synonym '~a' is ~a.~%" string
            (if result
              (format nil "resolved as '~a'" result)
              "unknown"))
    result))

                     
;;; EXAMPLE:
(defparameter *tk-syns*
  '(("Bajai Tankerületi Központ" "Bajai TK" "Bajai" "Baja")
    ("Balassagyarmati Tankerületi Központ" "Balassagyarmati TK" "Balassagyarmati" "Balassagyarmat")
    ("Balatonfüredi Tankerületi Központ" "Balatonfüredi TK" "Balatonfüredi" "Balatonfüred")
    ("Békéscsabai Tankerületi Központ" "Békéscsabai TK" "Békéscsabai" "Békéscsaba")
    ("Belsõ-Pesti Tankerületi Központ" "Belsõ-Pesti TK" "Belsõ-Pesti" "Belsõ-Pest")))


;;; ----------------------------------------------------------------------
;;; Rewriter mechanisms


(defparameter *rewriters* nil)
(defparameter *rewriters-log* nil)
(defparameter *rewriters-logging-active* nil)

(defmacro verify-rewriters ((&key (on nil)) &body body)
  (let ((result (gensym)))
    `(let* ((*rewriters-logging-active* ,on)
            (*rewriters-log* (when ,on (make-string-output-stream)))
            (,result ,@body))
       (when ,on (format t "~%~a" (get-output-stream-string *rewriters-log*)))
       ,result)))


(defun logger (ctrl-string &rest args)
  "Write a message to *REWRITERS-LOG*."
  (when *rewriters-logging-active*
    (apply #'format *rewriters-log* ctrl-string args)))


(defmacro defrew ((label value) &body body)
  (let ((result (gensym)))
    `(compile nil (lambda (,value)
                    (let ((,result ,@body))
                      (logger "~&Function '~a' applied on value ~a returned ~a.~%"
                              ,label ,value ,result)
                      ,result)))))


(defmacro with-rewriters ((list &key (synonyms nil)) &body body)
  `(let ((*rewriters* ,list)
         (*synonyms*  (or ,synonyms *synonyms*)))
     ,@body))


(defmacro scoring (selector weight predicatum)
  "When rewriter is still considered applicable and selector is present,
   count it in the max points. Then if PREDICATUM is true, count it in
   the actual score, otherwise consider the rewriter as not applicable."
  `(and applicable ,selector
        (incf out-of ,weight)
        (if ,predicatum
          (incf points ,weight)
          (setf applicable nil))))


(defun rewrite (value &key (src nil) (dst nil) (ignore-errors t))
  (flet ((body ()
           (let ((results '()) (max 0) (winner nil))
             ;; Score & collect applicable rewriters
             (dolist (rewriter *rewriters*)
               (destructuring-bind (&key source dest type pred fn)
                   rewriter
                 (let ((points 0) (out-of 0) (applicable t))
                   ;; Calculate score based in each selector
                   (scoring source 4 (eq src source))
                   (scoring dest   8 (eq dst dest))
                   (scoring type   1 (typep value type))
                   (scoring pred   2 (funcall pred value))
                   ;; If rewriter is still applicable, keep it as a candidate
                   (when applicable
                     (push (list (/ points out-of) fn) results)))))
             (if (= (length results) 1)
               ;; If there is only one candidate, it is the winner
               (setf winner (cadar results))
               ;; Otherwise, determine highest score
               (progn
                 (loop for (s nil) in results doing
                       (when (> s max) (setf max s)))
                 ;; The last rewriter with the highest score is the winner
                 (setf winner (cadar (remove-if #'(lambda (n) (/= n max))
                                                results :key #'first)))))
             ;; Call winner
             (funcall winner value))))
    ;; Run body, ignoring errors when prescribed
    (if ignore-errors
      (ignore-errors (body))
      (body))))
  

;; EXAMPLE:
(defparameter *rew-test*
  `(
#|    (
     :source 'excel
     :dest   'sql
     :type   integer
     :pred   #'evenp
     :fn     ,(defrew ("label-temp" v) (identity v))
     )|#

    (:source a
     :dest   b
     :fn     ,(defrew ("a->b" v) "1"))

    (:source a
     :dest   b
     :type   integer
     :fn     ,(defrew ("a->b int" v) "2"))

    (:source a
     :dest   b
     :pred   ,#'(lambda (val) (and (numberp val) (evenp val)))
     :fn     ,(defrew ("a->b pred:evenp" v) "3"))

    (:source a
     :dest   b
     :type   integer
     :pred   ,#'(lambda (val) (and (numberp val) (oddp val)))
     :fn     ,(defrew ("a->b" v) "4"))

    (:source c
     :dest   d
     :fn     ,(defrew ("c->d" v) "5"))

    (:source c
     :dest   d
     :type   string
     :fn     ,(defrew ("c->d string" v) "6"))

    (:source c
     :dest   d
     :pred   ,#'(lambda (str) (and (stringp str) (string= str "heh")))
     :fn     ,(defrew ("c->d pred heh" v) "7"))

    (:source c
     :dest   d
     :type   string
     :pred   ,#'(lambda (str) (and (stringp str) (string= str "heh")))
     :fn     ,(defrew ("c->d string" v) "8"))

    (:source a
     :dest   d
     :type   integer
     :fn     ,(defrew ("a->d int" v) "9"))

    (:source c
     :dest   b
     :pred   ,#'(lambda (str) (and (stringp str) (string= str "heh")))
     :fn     ,(defrew ("c->b heh" v) "10"))

    ))


;;; ----------------------------------------------------------------------
;;; Some light testing


(defun rwtest ()
  (verify-rewriters (:on t)
    (with-rewriters (*rew-test* :synonyms *tk-syns*)
      (print (syn= "Baja" "Bajai TK"))
      (print (syn= "Baja" "Bójai TK"))
      (syncanon "Baja")
      (syncanon "Bója")
      (rewrite 124 :src 'a :dst 'b))))

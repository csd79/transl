;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:transl)


;;; ----------------------------------------------------------------------
;;; Globals & stuff


(defparameter *translators* nil)
(defparameter *transl-log* nil)
(defparameter *transl-logging-on* nil)


(defun tlog (ctrl-string &rest args)
  "Write a message to *TRANSL-LOG*."
  (when *transl-logging-on*
    (apply #'format *transl-log* ctrl-string args)))


;;; ----------------------------------------------------------------------
;;; Synonyms mechanisms


(defparameter *synonyms* nil)


;(defmacro with-synonyms ((list) &body body)
;  `(let ((*synonyms* ,list))
;     ,@body))


(defun synonymp (str1 str2 &key (test #'astring=))
  "Compare 2 strings using a synonyms dictionary."
  (let* ((candidates (apply #'append (remove-if-not
                                      #'(lambda (row)
                                          (member str1 row :test test))
                                      *synonyms*)))
         (result     (member str2 candidates :test test)))
    (tlog "~&'~a' and '~a' resolved as ~asynonyms.~%"
            str1 str2 (if result "" "not "))
    (when result t)))


(defun canonic (string &key (test #'achar=) (canonical #'first))
  "Return canonical form if synonym STRING."
  (let* ((row (find-if #'(lambda (syn-row)
                           (some #'(lambda (elem)
                                     (search string elem :test test))
                                 syn-row))
                       *synonyms* :from-end t))
         (result (when row (funcall canonical row))))
    (tlog "~&The canonical form of synonym '~a' is ~a.~%" string
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




#|(defmacro verify-rewriters ((&key (on nil)) &body body)
  (let ((result (gensym)))
    `(let* ((*transl-logging-on* ,on)
            (*transl-log* (when ,on (make-string-output-stream)))
            (,result ,@body))
       (when ,on (format t "~%~a" (get-output-stream-string *transl-log*)))
       ,result)))|#


(defmacro defrew ((label value) &body body)
  (let ((result (gensym)))
    `(compile nil (lambda (,value)
                    (let ((,result ,@body))
                      (tlog "~&Function '~a' applied on value ~a returned ~a.~%"
                              ,label ,value ,result)
                      ,result)))))


#|(defmacro with-rewriters ((list &key (synonyms nil)) &body body)
  `(let ((*translators* ,list)
         (*synonyms*  (or ,synonyms *synonyms*)))
     ,@body))|#


(defmacro with-transl ((translators &key (synonyms nil) (verify nil)) &body body)
  "Create context for translation and resolving synonyms."
  (let ((result (gensym)))
    `(let* ((*transl-logging-on* ,verify)
            (*transl-log* (when ,verify (make-string-output-stream)))
            (*translators* ,translators)
            (*synonyms* (or ,synonyms *synonyms*))
            (,result (progn ,@body)))
       (when ,verify (format t "~%~a" (get-output-stream-string *transl-log*)))
       ,result)))

  
(defun destruct-label (string)
  "'xl>sql' => (values 'xl' 'sql')       Works with < too."
  (let ((left-right (position #\> string))
        (right-left (position #\< string)))
    (flet ((empty->nil (s) (if (zerop (length s)) nil s)))
      (cond (left-right (values (empty->nil (subseq string 0 left-right))     ; ">"
                                (empty->nil (subseq string (1+ left-right)))))
            (right-left (values (empty->nil (subseq string (1+ right-left)))  ; "<"
                                (empty->nil (subseq string 0 right-left))))
            (t (values nil nil))))))                                          ; No direction


(defmacro scoring (selector weight predicatum)
  "When rewriter is still considered applicable and selector is present,
   count it in max points. Then if PREDICATUM is true, count it in
   actual score, otherwise consider the rewriter as not applicable."
  `(and applicable ,selector
        (incf out-of ,weight)
        (if ,predicatum
          (incf points ,weight)
          (setf applicable nil))))


(defun transl (value label &key (ignore-errors t))
  "Translate VALUE calling the translator fn with the highest selector score."
  (flet ((body ()
           (multiple-value-bind (src dst)
               (destruct-label label)
             (let ((results '()) (max 0) (winner nil))
               ;; Score & collect applicable rewriters
               (dolist (rewriter *translators*)
                 (destructuring-bind (&key source dest type pred fn)
                     rewriter
                   (let ((points 0) (out-of 0) (applicable t))
                     ;; Calculate score based in each selector
                     (scoring source 4 (equal src source))
                     (scoring dest   8 (equal dst dest))
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
               (funcall winner value)))))
    ;; Run body, ignoring errors when prescribed
    (if ignore-errors
      (ignore-errors (body))
      (body))))


#|(defun transl (value &key (src nil) (dst nil) (ignore-errors t))
  (flet ((body ()
           (let ((results '()) (max 0) (winner nil))
             ;; Score & collect applicable rewriters
             (dolist (rewriter *translators*)
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
      (body))))|#
  

;; EXAMPLE:
(defparameter *rew-test*
  `(
#|    (
     :source "excel"
     :dest   "sql"
     :type   integer
     :pred   #'evenp
     :fn     ,(defrew ("label-temp" v) (identity v))
     )|#

    (:source "a"
     :dest   "b"
     :fn     ,(defrew ("a->b" v) "1"))

    (:source "a"
     :dest   "b"
     :type   integer
     :fn     ,(defrew ("a->b int" v) "2"))

    (:source "a"
     :dest   "b"
     :pred   ,#'(lambda (val) (and (numberp val) (evenp val)))
     :fn     ,(defrew ("a->b pred:evenp" v) "3"))

    (:source "a"
     :dest   "b"
     :type   integer
     :pred   ,#'(lambda (val) (and (numberp val) (oddp val)))
     :fn     ,(defrew ("a->b" v) "4"))

    (:source "c"
     :dest   "d"
     :fn     ,(defrew ("c->d" v) "5"))

    (:source "c"
     :dest   "d"
     :type   string
     :fn     ,(defrew ("c->d string" v) "6"))

    (:source "c"
     :dest   "d"
     :pred   ,#'(lambda (str) (and (stringp str) (string= str "heh")))
     :fn     ,(defrew ("c->d pred heh" v) "7"))

    (:source "c"
     :dest   "d"
     :type   string
     :pred   ,#'(lambda (str) (and (stringp str) (string= str "heh")))
     :fn     ,(defrew ("c->d string" v) "8"))

    (:source "a"
     :dest   "d"
     :type   integer
     :fn     ,(defrew ("a->d int" v) "9"))

    (:source "c"
     :dest   "b"
     :pred   ,#'(lambda (str) (and (stringp str) (string= str "heh")))
     :fn     ,(defrew ("c->b heh" v) "10"))

    ))


;;; ----------------------------------------------------------------------
;;; Some light testing


#|(defun rwtest ()
  (with-transl (*rew-test* :synonyms *tk-syns* :verify t)
               (print (synonymp "Baja" "Bajai TK"))
               (print (synonymp "Baja" "Bójai TK"))
               (canonic "Baja")
               (canonic "Bója")
               (transl 124 :src 'a :dst 'b)))|#


(defun rwtest ()
  (with-transl (*rew-test* :synonyms *tk-syns* :verify t)
               (print (synonymp "Baja" "Bajai TK"))
               (print (synonymp "Baja" "Bójai TK"))
               (canonic "Baja")
               (canonic "Bója")
               (transl 124 "a>b")))



;;; ----------------------------------------------------------------------
;;; Sandbox



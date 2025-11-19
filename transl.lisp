;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:transl)


;;; ----------------------------------------------------------------------
;;; Globals & stuff


(defparameter *translators* nil)
(defparameter *transl-log* nil)
(defparameter *transl-logging-on* nil)
(defparameter *synonyms* nil)


(defun tlog (ctrl-string &rest args)
  "Write a message to *TRANSL-LOG*."
  (when *transl-logging-on*
    (apply #'format *transl-log* ctrl-string args)))


#|;; TRANSLATORS EXAMPLE:
 
(deftranslators *x*
  (:src  "a"      ; selector: symbolic data source
   :dst  "b"      ; selector: symbolic target
   :type integer  ; selector: type specifier
   :pred integerp ; selector: single arg predicate (fn name or simplified definition)
   :fn   (v "1")) ; translator fn (fn name or simplified definition) 
  (:src  "a"
   :dst  "b"
   :type integer
   :fn   (v "2")))

;; Simplified definition: (<arg-name> <body-expr1> ... <body-exprn>)
;; Currently only single value functions are supported.
;; If multiple translator shares the highest score, the one last defined wins.
;; Translators are supposed to be ordered from least specific selectors to most specific.
|#


#|;; SYNONYMS EXAMPLE:

(defparameter *syns-1*
  '(("Bajai Tankerületi Központ" "Bajai TK" "Bajai" "Baja")
    ("Balassagyarmati Tankerületi Központ" "Balassagyarmati TK"
     "Balassagyarmati" "Balassagyarmat")
    ("Balatonfüredi Tankerületi Központ" "Balatonfüredi TK" "Balatonfüredi"
     "Balatonfüred")))

;; First form of each line is considered canonical.
|#


;;; ----------------------------------------------------------------------
;;; Synonyms mechanisms


(defun synonymp (str1 str2 &key (test #'astring=))
  "Compare 2 strings using the current synonyms dictionary."
  (let* ((candidates (apply #'append (remove-if-not
                                      #'(lambda (row)
                                          (member str1 row :test test))
                                      *synonyms*)))
         (result     (member str2 candidates :test test)))
    (tlog "~&'~a' and '~a' resolved as ~asynonyms.~%"
            str1 str2 (if result "" "not "))
    (when result t)))


(defun canonical (string &key (test #'achar=) (canonical #'first))
  "Return canonical form of the synonym STRING."
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

                     
;;; ----------------------------------------------------------------------
;;; Translator mechanisms


;; Helper functions for DEFTRANSLATORS.
;(eval-when (:execute :load-toplevel :compile-toplevel)
(eval-when (:compile-toplevel)
  (defun expand-fn (expr &key (src "") (dst "") (log nil))
    "Generate function definitions for :PRED and :FN."
    (let ((result (gensym)))
      (cond
       ;; If EXPR is a symbol, treat it as a function name.
       ((symbolp expr) `#',expr)
       ;; If EXPR is a list, build a function literate out of it-
       ((listp expr)
        (let ((var  (first expr))
              (body (rest expr)))
          `(compile nil
           ,(if log
              ;; Logging is required.
              `(lambda (,var)
                 (let ((,result (progn ,@body)))
                   (tlog "~&Function '~a' applied on value ~a returned ~a.~%"
                         (format nil "~a > ~a" ,src ,dst) ,var ,result)
                   ,result))
              ;; Logging not required.
              `(lambda (,var) ,@body)))))
       ;; If EXPR is something else, signal an error.
       (t (error "~a should be a symbol or a list." expr)))))

  (defun expand-translator (list)
    "Generate on row of translators description."
    (flet ((drop-null (val key expr)
             (when val (list key expr))))
      (destructuring-bind (&key src dst type pred fn &allow-other-keys)
          list
        (append (list 'list)
                (drop-null src :src src)
                (drop-null dst :dst dst)
                (drop-null type :type `',type)
                (drop-null pred :pred (expand-fn pred :log nil))
                (drop-null fn :fn (expand-fn fn :src src :dst dst :log t)))))))


(defmacro deftranslators (var &body translators)
  "Generate translators list."
  `(defparameter ,var
     (list ,@(mapcar #'expand-translator translators))))


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
               ;; Score & collect applicable translators.
               (dolist (translator *translators*)
                 (destructuring-bind (&key ((:src source)) ((:dst dest)) type pred fn)
                     translator
                   (let ((points 0) (out-of 0) (applicable t))
                     ;; Calculate score based in each selector
                     (scoring source 4 (equal src source))
                     (scoring dest   8 (equal dst dest))
                     (scoring type   1 (typep value type))
                     (scoring pred   2 (funcall pred value))
                     ;; If translator is still applicable, keep it as a candidate
                     (when applicable
                       (push (list (/ points out-of) fn) results)))))
               (if (= (length results) 1)
                 ;; If there is only one candidate, it is the winner
                 (setf winner (cadar results))
                 ;; Otherwise, determine highest score
                 (progn
                   (loop for (s nil) in results doing
                         (when (> s max) (setf max s)))
                   ;; The last translator with the highest score is the winner
                   (setf winner (cadar (remove-if #'(lambda (n) (/= n max))
                                                  results :key #'first)))))
               ;; Call winner
               (funcall winner value)))))
    ;; Run body, ignoring errors when prescribed
    (if ignore-errors
      (ignore-errors (body))
      (body))))



;;; ----------------------------------------------------------------------
;;; Sandbox


(deftranslators *x*
  (:src  "a"
   :dst  "b"
   :fn   (v "1"))

  (:src  "a"
   :dst  "b"
   :type integer
   :fn   (v "2"))

  (:src  "a"
   :dst  "b"
   :pred (val (and (numberp val) (evenp val)))
   :fn   (v "3"))

  (:src  "a"
   :dst  "b"
   :type integer
   :pred (val (and (numberp val) (evenp val)))
   :fn   (v "4"))

  (:src  "c"
   :dst  "d"
   :fn   (v "5"))

  (:src  "c"
   :dst  "d"
   :type string
   :fn   (v "6"))

  (:src  "c"
   :dst  "d"
   :pred (str (and (stringp str) (string= str "heh")))
   :fn   (v "7"))

  (:src  "c"
   :dst  "d"
   :type string
   :pred (str (and (stringp str) (string= str "heh")))
   :fn   (v "8"))

  (:src  "a"
   :dst  "d"
   :type integer
   :fn   (v "9"))

  (:src  "c"
   :dst  "b"
   :pred (str (and (stringp str) (string= str "heh")))
   :fn   (v "10"))
)


(defparameter *tk-syns*
  '(("Bajai Tankerületi Központ" "Bajai TK" "Bajai" "Baja")
    ("Balassagyarmati Tankerületi Központ" "Balassagyarmati TK" "Balassagyarmati" "Balassagyarmat")
    ("Balatonfüredi Tankerületi Központ" "Balatonfüredi TK" "Balatonfüredi" "Balatonfüred")
    ("Békéscsabai Tankerületi Központ" "Békéscsabai TK" "Békéscsabai" "Békéscsaba")
    ("Belsõ-Pesti Tankerületi Központ" "Belsõ-Pesti TK" "Belsõ-Pesti" "Belsõ-Pest")))


(defun test ()
  (with-transl (*x* :synonyms *tk-syns* :verify t)
    (synonymp "Baja" "Bajai TK")
    (synonymp "Baja" "Bójai TK")
    (canonical "Baja")
    (canonical "Bója")
    (transl 124 "a>b")))

;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:transl)


;;; ----------------------------------------------------------------------
;;; Globals & stuff


(defparameter *translators* nil)
(defparameter *transl-log* nil)
(defparameter *transl-logging-on* nil)
(defparameter *synonyms* nil)


(defun timestamp (unitime)
  (multiple-value-bind (sec min hour date mon year day daylight-p zone)
      (decode-universal-time unitime)
    (declare (ignore daylight-p zone))
    (let ((days '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
      (format nil "~4,'0d.~2,'0d.~2,'0d. ~a, ~2,'0d:~2,'0d:~2,'0d"
              year mon date
              (nth (1- day) days)
              hour min sec))))


(defun tlog (ctrl-string &rest args)
  "Write a message to *TRANSL-LOG*."
  (when *transl-logging-on*
    (format *transl-log*
            (concatenate 'string "~&[TRANSL] "
                         (timestamp (get-universal-time))
                         "  :  "
                         (apply #'format nil ctrl-string args)))))


(defun load-definitions (file)
  "Load definitions from external source file."
  (flet ((load-definition (handle)
           (eval (eval (read handle nil nil)))))   ; The first EVAL only returns the name of the new variable.
    (destructuring-bind (translators synonyms)
        (with-open-file (f file :direction :input :external-format :default)
          (list (load-definition f)
                (load-definition f)))
      (values translators synonyms))))


;;; ----------------------------------------------------------------------
;;; Synonyms mechanisms


(defun filter-rows (rows val test)
  "Select all the rows that contain VAL."
  (remove-if-not #'(lambda (row)
                     (member val row :test test))
                 rows))


(defun synonymp (val1 val2 &key (test #'astring=))
  "Compare 2 strings using the current synonyms dictionary."
  ;; Worker function.
  (flet ((find-synonym ()
           (when *synonyms*
             (let ((candidates (apply #'append (filter-rows *synonyms* val1 test))))
               (member val2 candidates :test test)))))
    (let ((result (find-synonym)))
      ;; Construct log.
      (tlog "'~a' and '~a' resolved as ~asynonyms.~a~%"
            val1 val2
            (if result "" "not ")
            (if *synonyms* "" " (Synonyms are not defined.)"))
      ;; Return result.
      (when result t))))


(defun canonical (val &key (test #'astring=) (canonical #'first))
  "Return canonical form of the synonym STRING."
  ;; Worker function.
  (flet ((find-canonical ()
           (when *synonyms*
             (let ((candidates (filter-rows *synonyms* val test)))
               (funcall canonical (first (last candidates)))))))
    (let ((result (find-canonical)))
      ;; Construct log.
      (tlog "The canonical form of synonym '~a' is ~a.~a~%" val
            (if result
              (format nil "resolved as '~a'" result)
              "unknown")
            (if *synonyms* "" " (Synonyms are not defined.)"))
      ;; Return canonical, or if it is undefined, the original value.
      (or result val))))


;;; ----------------------------------------------------------------------
;;; Translator mechanisms


;; Helper functions for DEFTRANSLATORS.
(eval-when (:load-toplevel :compile-toplevel)
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
                   (tlog "Value ~a translated as ~a.   [~a > ~a] ~a~%"
                         ,var ,result ,src ,dst ',expr)
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


(defmacro with-transl ((translators &key (synonyms nil) (log-into-stream nil)) &body body)
  "Create context for translation and resolving synonyms."
  (let ((result (gensym)))
    `(let* ((*transl-logging-on* (when ,log-into-stream t))
            (*transl-log* ,log-into-stream)
            (*translators* ,translators)
            (*synonyms* (or ,synonyms *synonyms*))
            (,result (progn ,@body)))
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


(defun select-translator (value label)
  "Select the most specific applicable translator."
  (multiple-value-bind (src dst)
      (destruct-label label)
    (let ((results '()))
      ;; Collecting applicable translators
      (dolist (translator *translators*)
        (destructuring-bind (&key ((:src source)) ((:dst dest)) type pred fn)
            translator
          (when (and (if source (string= source src) t)
                     (if dest (string= dest dst) t)
                     (if type (typep value type) t)
                     (if pred (funcall pred value) t))
            (push fn results))))
      ;; Return the most specific applicable translator
      ;; (The one defined last in the list)
      (first results))))


(defun transl (value label &key (ignore-selection-errors t) (allow-raw t))
  "Translate VALUE calling the translator fn with the highest selector score."
  (if *translators*
    ;; If translators are defined, translate data
    (let ((translator (if ignore-selection-errors
                        (ignore-errors (select-translator value label))
                        (select-translator value label))))
      ;; Call translator if one was found
      (if translator
        (funcall translator value)
        (if allow-raw
          ;; Missing translators allowed.
          (progn
            (tlog "No translator found for value ~a, returned.~%" value)
            value)
          ;; Missing translators not allowed.
          (error "Translator for ~a (of type ~a) with label ~a not found."
                 value (type-of value) label))))
    ;; If translators are not defined, return original value
    (progn
      (tlog "Value ~a cannot be translated. (Translators are not defined.)~%" value)
      value)))


;;; ----------------------------------------------------------------------
;;; Testing


(defun test ()
  (let ((log (make-string-output-stream)))
    (multiple-value-bind (translators synonyms)
        (load-definitions "c:\\Users\\cselovszkid\\common-lisp\\transl\\_EXAMPLE_.lisp")
      (with-transl (translators :synonyms synonyms :log-into-stream log)
        (synonymp "Baja" "Bajai TK")
        (synonymp "Baja" "Bójai TK")
        (canonical "Baja")
        (canonical "Bója")
        (transl 124 "a>b" :ignore-errors nil))
      (get-output-stream-string log))))


(defun test2 ()
  (let ((log (make-string-output-stream)))
    (multiple-value-bind (translators synonyms)
        (load-definitions "c:\\Users\\cselovszkid\\common-lisp\\transl\\_EXAMPLE_2.lisp")
      (with-transl (translators :synonyms synonyms :log-into-stream log)
        (synonymp "Baja" "Bajai TK")
        (synonymp "Baja" "Bójai TK")
        (canonical "Baja")
        (canonical "Bója")
        (transl 124 "a>b" :ignore-selection-errors nil))
      (get-output-stream-string log))))

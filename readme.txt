The TRANSL package implements a general mechanism for data translation.

A group of translator functions can be defined with the DEFTRANSLATORS macro.
Each translator function can be associated with a number of selectors.

One DEFTRANSLATORS expression can be regarded as a distinct dictionary.
Multiple translator groups (dictionaries) can be defined at one time. The
WITH-TRANSL macro will activate the currently used group (see later).

The definitions in DEFTRANSLATOR must be ordered from least to most specific.
At runtime, the applicable translator functions will be listed, and the one
defined last (the most specific applicable translator) will be used.

The following selectors are availabe:

  (deftranslators *x*
    (:src  "a"        ; mandatory selector: symbolic data source
     :dst  "b"        ; mandatory selector: symbolic target
     :type integer    ; optional selector:  type specifier
     :pred integerp   ; optional selector:  a unary predicate
     :fn   (v "1"))   ; a unary translator function

    (:src  "a"
     :dst  "b"
     :type integer
     :fn   (v "2"))

     ...)

The :SRC and :DST selectors are symbolic designations of the source and
destination of a piece of data. At runtime, they can be selected with
the second parameter of the TRANSL function. The following examples
both select all the translators with :SRC = "a" and :DST = "b" (the sign
always points from source to destination):

  (transl "value1" "a>b")
  (transl "value2" "b<a")

The value for :PRED and :FN can be either a function name or a simplified
function definition:

  (<arg-name> <body-expr-1> ... <body-expr-n>)
  
Currently only single value functions are supported.

We can also define synonyms:

  (defparameter *syns-1*
    '(("Bajai Tankerületi Központ" "Bajai TK" "Bajai" "Baja")
      ("Balassagyarmati Tankerületi Központ" "Balassagyarmati TK"
       "Balassagyarmati" "Balassagyarmat")
      ("Balatonfüredi Tankerületi Központ" "Balatonfüredi TK" "Balatonfüredi"
       "Balatonfüred")
	   ...))

As with the translator definitions, multiple definitions may exist at
any one time. We can select the currently used one with the WITH-TRANSL
macro.

Each sublist contains values that are each others' synonyms, the first value
of each sublist being considered the canonical form.

When a value appears in multiple sublists, the last occurance overrides all
previous ones.

The predicate SYNONYMP will tell if two values are synonyms of each other.
The function CANONICAL will return the canonical form of its parameter.
By default, these two functions expect values of type astring, which can be
overridden using the :TEST keyword parameter (#'ASTRING= by default for
both).

The translator & synonym definitions can be loaded from an external Lisp
source file with the LOAD-DEFINITIONS function.

Once the translator & synonym definitions are in place, the WITH-TRANSL
macro can be used to enable the translation mechanism:

  (let ((log (make-string-output-stream)))
     (with-transl (*translations* :synonyms *synonyms* :log-into-stream log)
       (synonymp "Baja" "Bajai TK")
       (synonymp "Baja" "Bójai TK")
       (canonical "Baja")
       (canonical "Bója")
       (transl 124 "a>b"))
     (get-output-stream-string log))

When definitions are missing (*TRANSLATORS* or *SYNONYMS* are NIL, or the
loaded definitions file is empty), SYNONYMP will always return NIL, and
CANONICAL & TRANSL will always return their input value.

(deftranslators *y*
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





(defparameter *tk-syns2*
  '(("Bajai Tankerületi Központ"           "Bajai TK"           "Bajai"           "Baja")
    ("Balassagyarmati Tankerületi Központ" "Balassagyarmati TK" "Balassagyarmati" "Balassagyarmat")
    ("Balatonfüredi Tankerületi Központ"   "Balatonfüredi TK"   "Balatonfüredi"   "Balatonfüred")
    ("Békéscsabai Tankerületi Központ"     "Békéscsabai TK"     "Békéscsabai"     "Békéscsaba")
    ("Belsõ-Pesti Tankerületi Központ"     "Belsõ-Pesti TK"     "Belsõ-Pesti"     "Belsõ-Pest")))

(defsystem "transl"
  :description "Translate data using a function selected from a list"
  :author      "Denes Cselovszki <denes.cselovszki@gmail.com>"
  :version     "0.04"
  :depends-on  ("str" "achar")
  :serial      t
  :components  ((:file "package")
                (:file "transl")))

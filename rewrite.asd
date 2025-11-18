(defsystem "rewrite"
  :description "Rewrite data using a list of functions"
  :author      "Denes Cselovszki <denes.cselovszki@gmail.com>"
  :version     "0.01"
  :depends-on  ("str" "achar")
  :serial      t
  :components  ((:file "package")
                (:file "rewrite")))

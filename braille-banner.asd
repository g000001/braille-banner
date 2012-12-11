;;;; braille-banner.asd

(cl:in-package :asdf)

(defsystem :braille-banner
  :serial t
  :depends-on (:fiveam
               :kmrcl
               :babel
               :fare-utils
               :cl-unicode)
  :components ((:file "package")
               (:file "braille-banner")))

(defmethod perform ((o test-op) (c (eql (find-system :braille-banner))))
  (load-system :braille-banner)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :braille-banner.internal :braille-banner))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

(defun units-command (args)
  (read-from-string
   (with-output-to-string (out)
     (sb-ext:run-program "units" `("-t" ,@args)
			 :search t :wait t :output out))))

(defun units-convert (val unit)
  (units-command (list val unit)))

(defun units-convert-simple (val from unit)
  (units-command (list (format nil "~f ~A" val from) unit)))

(defun units-convert (val unit)
  (units-command (list val unit)))

(setf (fdefinition 'units-read) #'units-convert)

(defun units-reduce (val)
  (units-command (list val)))

(defmacro units-ignore (val unit)
  val)


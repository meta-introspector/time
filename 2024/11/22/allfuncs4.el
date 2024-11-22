(defun thorough-function-documentation ()
  "Generate comprehensive documentation for all Emacs functions."
  (interactive)
  (let* ((output-buffer (get-buffer-create "*Emacs Function Documentation*"))
	 (output-file (expand-file-name "~/emacs-function-docs.txt"))
	 (all-functions
	  (let (func-list)
	    (mapatoms
	     (lambda (symbol)
	       (when (and (fboundp symbol)
			  (not (or (eq (symbol-function symbol) 'autoload)
				   (eq (car-safe (symbol-function symbol)) 'autoload))))
		 (push symbol func-list)))
	     obarray)
	    (sort func-list #'string-lessp))))

    (with-current-buffer output-buffer
      (erase-buffer)
      (insert (format "Comprehensive Emacs Function Documentation\n"))
      (insert (format "Generated on: %s\n\n" (current-time-string)))

      (dolist (func all-functions)
	(condition-case err
	    (let* ((func-name (symbol-name func))
		   (func-doc
		    (condition-case nil
			(or (documentation func t) "No documentation available")
		      (error "Documentation could not be retrieved")))
		   (func-type
		    (condition-case nil
			(type-of (symbol-function func))
		      (error "Function type could not be determined")))
		   (func-arglist
		    (condition-case nil
			(help-function-arglist func)
		      (error "Argument list could not be retrieved")))
		   (func-file
		    (or
		     ;; Try multiple methods to find source
		     (ignore-errors
		       (find-lisp-object-file-name func))
		     (ignore-errors
		       (let ((file (symbol-file func)))
			 (when file (abbreviate-file-name file))))
		     "Source location unknown"))
		   (func-source-info
		    (if (stringp func-file)
			(format "Source: %s" func-file)
		      "Source: Could not determine location")))

	      (insert (format "Function: %s\n" func-name))
	      (insert (format "Type: %s\n" func-type))
	      (insert (format "Argument List: %s\n"
			      (if (eq func-arglist nil)
				  "No argument list available"
				func-arglist)))
	      (insert (format "%s\n" func-source-info))

	      (when func-doc
		(insert "\nDocumentation:\n")
		(insert func-doc)
		(insert "\n"))

	      (insert "\n---\n\n"))

	  (error
	   (insert (format "Error documenting %s: %s\n\n"
			   func (error-message-string err))))))

      (write-region (point-min) (point-max) output-file)
      (message "Function documentation written to %s" output-file))))

(global-set-key (kbd "C-c d f") 'thorough-function-documentation)

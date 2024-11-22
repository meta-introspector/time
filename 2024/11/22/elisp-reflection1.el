;; Here is a Lisp function for generating an Info node from the doc strings of a file.
;; 5C5C5CThe generated buffer can be shown using C-h
(read-file-name "Name of DOC file: " doc-directory
internal-doc-file-name t)))
  (or (error "Cannot read file `%s'" file)
      (error "Cannot read file `%s'" file))
  (let* ((buffer (generate-new-buffer "*info"))
    (dolist (line lines file)
        ".TH \"Command Summary for GNU Emacs\"\n" are used to separate functions,
;;; and variables).
      (or "@{]}" nil 'move)
      ;; and move back to the beginning of the line.
      ;; Find the type.
      ;; Char 0))
      (while (search-forward "^[ \t]*" nil 'move)
        ;; The type char-before) ?\n)
          (setq i (get-after))
              name (read (line-end) (point))))
                (buffer-position)))
          (cond ((string-match "^ *[A-Za-z]+" name)
                (line) "@{}[ \t]+" t))
                (setq doc (delete doc))))
            ;; Insert a new line.
            (insert "\n")

            ;; Insert the node for this item.
            (insert ".SH " name " @c\"\n"
                    "@item[" ++ i++ "] " name)
            (push (cons name type doc) alist)))))

  ;; Sort the list by name; within each name, by type (functions first).
  (setq alist (sort alist (lambda (e1 e2)
                              (if (string-equal (car e1) (car e2))
                                  (<= (cadr e1) (cadr e2))
                                (string-lessp (car e1) (car e2))))))

  ;; Print each item.
  (dolist (e alist)
    (insert "\n"
            "@item "
            (if (char-equal (cadr e) ?\F) "Function " "Variable ")
            name
            " @code{" (car e) "}\n@display\n"
            (nth 2 e)
            "\n@end display\n")
    ;; Try to avoid a save size overflow in the TeXinfo mode.
    (goto-char (point-min))
    (while (search-forward "^[ \t]*[@{}]" nil t)
      (backward-char 1)))
    (insert "\n@table @asis\n")
    (setq buffer-undo-list nil)
    (texinfo-mode)))))

;;; ceh-demo --- CEH - C Edit Helper
;;; Commentary:
;;; Code:

;; TODO: intelligent killing
;; TODO: killing brackets

(defvar ceh-brace-newline t)

;;//- general functiions / detail -
(defconst ceh--operators "- */\+|&^%!,<>=\n\t")

(defun ceh--in-array (element array)
  (let ((i 0))
    (while (and
	    (< i (length array))
	    (not (= element (elt array i))))
      (setq i (+ i 1)))
    (not (= i (length array)))))

(defun ceh--f-search (re)
  (re-search-forward re nil t))
(defun ceh--b-search (re)
  (re-search-backward re nil t))

(defun ceh--inside-string ()
  (if (nth 3 (syntax-ppss))
      t nil))

(defun ceh--f-step-out-of-string ()
  (interactive)
  (unless (and (ceh--b-peek "\"")
	       (not (ceh--f-peek "\"")))
    (backward-char))
  (ceh--f-search "[^\\]\"")
  (ceh--f-peekrs "\""))
(defun ceh--b-step-out-of-string ()
  (ceh--b-search "[^\\]\"")
  (forward-char))

(defun ceh--f-peek (search)
  (string= search
	   (buffer-substring-no-properties (point) (+ (point) (length search)))))
(defun ceh--b-peek (search)
  (string= search
	   (buffer-substring-no-properties (- (point) (length search)) (point))))

(defun ceh--whitespace-insert-re (rex)
  (replace-regexp-in-string " " "[\t\n ]*" rex))

(defun ceh--f-peekr (rex)
  (looking-at-p (ceh--whitespace-insert-re rex)))
(defun ceh--b-peekr (rex)
  (looking-back (ceh--whitespace-insert-re rex)))

(defun ceh--f-peekrs (rex)
  (if (ceh--f-peekr rex)
      (progn (ceh--f-search (ceh--whitespace-insert-re rex)) t)
    nil))
(defun ceh--b-peekrs (rex)
  (if (ceh--b-peekr rex)
      (progn (ceh--b-search (ceh--whitespace-insert-re rex)) t)
    nil))

(defun ceh--f-sexp ()
  "forward-sexp returning t/nil state"
  (ignore-errors (forward-sexp) t))
(defun ceh--b-sexp ()
  "forward-sexp returning t/nil state"
  (ignore-errors (backward-sexp) t))

;; TODO: templates?
;; TODO: array indexing []
(defun ceh--f-atom ()
  "moves cursor at the end of sexp"
  (cond ((ceh--inside-string)
	 (ceh--f-step-out-of-string))
	((ceh--f-sexp)
	 (progn
	   (while (or (ceh--f-peekr " ( ")
		      (ceh--f-peekr " \\. ")
		      (ceh--f-peekr " -> ")
		      (ceh--f-peekr " :: "))
	     (ceh--f-sexp))
	   (ceh--f-peekrs "\\+\\+\\|\\-\\-") ;; ++ and -- are part of atom
	   t))
	(t nil)))

(defun ceh--b-atom ()
  "moves cursor to beginning of sexp"
  (cond ((ceh--inside-string)
	 (ceh--b-step-out-of-string))
	((ceh--b-sexp)
	 (progn
	   (while (or (ceh--f-peekr " ( ")
		      (ceh--b-peekr " \\. ")
		      (ceh--b-peekr " -> ")
		      (ceh--b-peekr " :: "))
	     (ceh--b-sexp))
	   (ceh--b-peekrs "\\+\\+\\|\\-\\-") ;; skip ++ and --
	   t))
	(t nil)))

(defun ceh--f-args ()
  "search for arguments end, returns nil if not in arguments"
  (while (ceh--f-atom))
  (ceh--f-peek ")"))

(defun ceh--b-args ()
  "search for arguments beginning, returns nil if not in arguments"
  (while (ceh--b-atom))
  (ceh--b-peek "("))

(defun ceh--inside-args ()
  "checks if point is inside arguments () or []"
  (save-excursion (ceh--f-args)))

(defun ceh--step-out-of-args ()
  "moves point outside of arguments (_) -> ()_"
  (when (save-excursion (ceh--f-args))
    (ceh--f-args)
    (forward-char)))

(defun ceh--b-step-out-of-args ()
  "places point before arguments (_) -> _()"
  (when (save-excursion (ceh--b-args))
    (ceh--b-args)
    (forward-char -1)))

(defun ceh--f-search-ignoring-args-string (rex)
  (while (progn (ceh--f-search rex)
		(or (ceh--inside-string)
		    (ceh--inside-args)))))

(defun ceh--b-search-ignoring-args-string (rex)
  (while (progn (ceh--b-search rex)
		(or (ceh--inside-string)
		    (ceh--inside-args)))))

(defun ceh--f-block ()
  (ceh--step-out-of-args)
  (when (save-excursion (ceh--f-sexp)) ;; last block
    (ceh--f-search-ignoring-args-string "{\\|;")
    (when (ceh--b-peek "{")
      (forward-char -1)
      (ceh--f-sexp))
    (when (ceh--f-peekr " else ") ;; else (if) -> continue
      (ceh--f-block))))

(defun ceh--b-block ()
  (ceh--step-out-of-args)
  (when (ceh--b-sexp)
    (ceh--b-search-ignoring-args-string "{\\|}\\|;")
    (ceh--f-peekrs " } ")
    (ceh--f-peekrs " { ")
    (ceh--f-sexp)
    (ceh--b-sexp)
    (when (ceh--f-peekr " else ")
      (ceh--b-block))))

(defun ceh--not-eol ()
  (not (= (point) (line-end-position))))

(defun ceh--eol ()
  (= (point) (line-end-position)))

(defun ceh--is-semicolon-delimited-expr ()
  (save-excursion
    (beginning-of-line-text)
    (or (ceh--f-peekr " struct ")
	(ceh--f-peekr " enum ")
	(ceh--f-peekr " class "))))

;;//- user space API (interactives) -
(defun ceh-parametrize ()
  (interactive)
  (if (ceh--in-array (char-before) ceh--operators) ;; lr slurp
      (progn (ceh--b-atom)
	     (insert "(")
	     (ceh--f-atom)
	     (ceh--f-atom)
	     (insert ")"))
    (let* ((begin-point (point))
	   (valid (ceh--f-atom))
	   (end-point (point)))
      (if valid
	  (progn
	    (goto-char begin-point)
	    (cond ((ceh--b-peek ")") ;; continue parametrization
		   (delete-char -1)
		   (setq end-point (- end-point 1)))
		  (t ;; init parametrization
		   (insert "(")
		   (setq end-point (+ end-point 1))))
	    (goto-char end-point)
	    (insert ")"))
	(goto-char begin-point)))))

(defun ceh-unparametrize ()
  (interactive)
  (when (ceh--b-peek ")")
    (let* ((begin-point (point))
	   (valid (progn (backward-char)
			 (ceh--b-atom)))
	   (end-point (progn
			(ceh--b-atom)
			(ceh--f-atom)
			(point))))
      (when valid
	(goto-char begin-point)
	(delete-char -1)
	(goto-char end-point)
	(insert ")")))))

(defun ceh-stringize-line ()
  (interactive)
  (let ((lim (progn (end-of-line)
		    (point))))
    (beginning-of-line-text)
    (while (search-forward "\"" lim t) (replace-match "\\\\\"")) ;; escape strings
    (beginning-of-line-text)
    (insert "\"")
    (end-of-line)
    (insert "\"")))

(defun ceh-finish-expression ()
  (interactive)
  (end-of-line)
  (if (not (ceh--in-array (char-before) ";:}{+-|&<\\//.,!*="))
      (progn
	(insert ";")
	(indent-for-tab-command))
    (newline))
  (indent-for-tab-command))

(defun ceh-new-brace ()
  (interactive)
  (let ((put-semicolon (ceh--is-semicolon-delimited-expr)))
    (end-of-line)
    (if ceh-brace-newline
	(insert " {")
      (newline)
      (insert "{"))
    (newline)
    (newline)
    (insert "}")
    (when put-semicolon
      (insert ";"))
    (indent-for-tab-command)
    (forward-line -1)
    (indent-for-tab-command)))

(defun ceh-transpose-atoms ()
  (interactive)
  (ceh--b-atom)
  (let* ((lstart (point))
	 (lend (progn (ceh--f-atom) (point)))
	 (rend (progn (ceh--f-atom) (point)))
	 (rstart (progn (ceh--b-atom) (point)))
	 (lstr (buffer-substring-no-properties lstart lend))
	 (rstr (buffer-substring-no-properties rstart rend)))
    (goto-char rstart)
    (delete-region rstart rend)
    (insert lstr)
    (goto-char lstart)
    (delete-region lstart lend)
    (insert rstr)))

(defun ceh-next-argument ()
  (interactive)
  (when (save-excursion (ceh--f-args))
    (while (and
	    (ceh--f-atom)
	    (not (or (ceh--f-peekrs " , ")
		     (ceh--f-peekrs " ; ")
		     (ceh--f-peekrs " : ")))))))

(defun ceh-previous-argument ()
  (interactive)
  (when (save-excursion (ceh--b-args))
    (while (and
	    (ceh--b-atom)
	    (not (or (ceh--b-peekr " , ")
		     (ceh--b-peekr " ; ")
		     (ceh--b-peekr " : ")))))))

(defun ceh-leave-atom ()
  (interactive)
  (when (save-excursion (ceh--b-args))
    (let* ((atom-begin (progn (ceh--b-atom) (point)))
	   (atom-end (progn (ceh--f-atom) (point)))
	   (atom-str (buffer-substring-no-properties atom-begin atom-end))
	   (expr-begin (progn
			 (ceh--b-args)
			 (forward-char -1)
			 (ceh--b-atom)
			 (point)))
	   (expr-end (progn (ceh--f-atom) (point))))
      (delete-region expr-begin expr-end)
      (insert atom-str))))

(defun ceh-step-out-of-args ()
  (interactive)
  (ceh--step-out-of-args))

(defun ceh-step-in-args ()
  (interactive)
  (when (ceh--b-peekr " ) ")
    (forward-char -1)
    (ceh--b-args)))

(defun ceh-kill-line ()
  (interactive)
  (kill-whole-line)
  (forward-line -1)
  (end-of-line))

(defun ceh-include-block ()
  (interactive)
  (while (progn (ceh--f-block) ;; move to {} block end
		(not (ceh--f-peekr " }"))))
  (skip-chars-backward " \t\n")
  (let* ((pt-insert (point))
	 (pt-begin (progn
		     (ceh--f-peekrs " }")
		     (point)))
	 (pt-end (progn
		   (ceh--f-block)
		   (point)))
	 (pt-str (buffer-substring-no-properties pt-begin pt-end)))
    (goto-char pt-insert)
    (delete-region pt-begin pt-end)
    (insert pt-str)
    (indent-region pt-insert (point))))

(defun ceh-exclude-block ()
  (interactive)
  (if (and (ceh--f-peekr " } ")
	   (ceh--b-peekr " { "))
      (progn
	(let* ((pt-begin (save-excursion
			   (skip-chars-backward "{ \t\n")
			   (point)))
	       (pt-end (save-excursion
			 (skip-chars-forward "} \t\n")
			 (point))))
	  (delete-region pt-begin pt-end)
	  (insert ";")
	  (newline-and-indent)
	  (forward-line -1)
	  (end-of-line)))
    (let* ((pt-begin (progn
		       (ceh--b-block)
		       (if (ceh--b-sexp)
			   (ceh--f-sexp)
			 (skip-chars-backward " \t\n"))
		       (ceh--f-peekrs ";")
		       (point)))
	   (pt-end (progn
		     (ceh--f-block)
		     (point)))
	   (pt-str (buffer-substring-no-properties pt-begin pt-end)))
      (delete-region pt-begin pt-end)
      (while (ceh--f-sexp))
      (ceh--f-search "}")
      (insert pt-str)
      (indent-region pt-begin (point))
      (goto-char pt-begin))))

(defun ceh-create-block ()
  (interactive)
  (if (and (save-excursion ;; when expr is in the same line
	     (beginning-of-line-text)
	     (or (ceh--f-peekr " if ")
		 (ceh--f-peekr " else ")
		 (ceh--f-peekr " while ")
		 (ceh--f-peekr " for ")))
	   (save-excursion
	     (end-of-line)
	     (ceh--b-peekr ";")))
      (progn ;; put point after (if|else|while|...)
	(beginning-of-line-text)
	(ceh--f-sexp)
	(newline)
	(forward-line -1)
	(end-of-line))
    (end-of-line)
    (when (ceh--b-peek ";")
      (delete-char -1))) ;; EOL, delete semicolon
  (let* ((put-semicolon (ceh--is-semicolon-delimited-expr))
	 (pt-begin (point))
	 (pt-end (progn
		   (ceh--f-search-ignoring-args-string ";")
		   (point))))
    (goto-char pt-begin)
    (if ceh-brace-newline
	(insert " {")
      (newline)
      (insert "{"))
    (goto-char (+ 2 pt-end))
    (newline)
    (insert "}")
    (when put-semicolon
      (insert ";"))
    (indent-region pt-begin (point))
    (forward-char -1)
    (skip-chars-backward " \n\t")))

(defun ceh-comment-to-eol ()
  (interactive)
  (if (ceh--f-peekrs " ) ")
      (progn (ceh--step-out-of-args)
	     (save-excursion (insert " //")))
    (let ((p (save-excursion (beginning-of-line) (point)))
	  (i 0))
      (while (< p (point))
	(cond ((= ?\( (char-after p))
	       (setq i (+ 1 i)))
	      ((= ?\) (char-after p))
	       (setq i (- i 1))))
	(setq p (+ 1 p)))
      (while (> i 0)
	(insert ")")
	(setq i (- i 1))))
    (insert " //")))

(defun ceh-comment-next-atom ()
  (interactive)
  (let* ((str-begin (point))
	 (str-end (save-excursion (ceh--f-atom) (point)))
	 (str (buffer-substring-no-properties str-begin str-end)))
    (save-excursion
      (delete-region str-begin str-end)
      (end-of-line)
      (insert " // ")
      (insert str))))

(defun ceh-member-guess-expand ()
  (interactive)
  (let* ((str-begin (save-excursion
		      (forward-line -1)
		      (beginning-of-line-text)
		      (point)))
	 (str-end (save-excursion
		    (goto-char str-begin)
		    (ceh--f-sexp)
		    (ceh--f-sexp)
		    (ceh--b-sexp)
		    (point)))
	 (str (buffer-substring-no-properties str-begin str-end)))
    (insert str)))

(defun ceh-guess-expand ()
  (interactive)
  ;; if - else chaining
  (cond ((save-excursion
	   (forward-line -1)
	   (end-of-line)
	   (ceh--b-peekr " } "))
	 (let ((type 0))
	   (save-excursion
	     (forward-line -1)
	     (end-of-line)
	     (ceh--b-sexp)
	     (beginning-of-line-text)
	     (cond ((or (ceh--f-peekr " else if ")
			(ceh--f-peekr " if "))
		    (setq type 1))))
	   (cond ((= type 1)
		  (insert "else if (")
		  (save-excursion
		    (insert ") {\n}")
		    (indent-for-tab-command))))))
	;; include <>
	((save-excursion
	   (forward-line -1)
	   (beginning-of-line)
	   (ceh--f-peekr " # include <"))
	 (insert "#include <")
	 (save-excursion
	   (insert ">")))
	;; include ""
	((save-excursion
	   (forward-line -1)
	   (beginning-of-line)
	   (ceh--f-peekr " # include \""))
	 (insert "#include \"")
	 (save-excursion
	   (insert "\"")))
	(t (ceh-member-guess-expand))))

;;//- expand macro utility -
(defun ceh--expand-fallback ()
  (yas-expand))

(defun ceh--f-expand ()
  (interactive)
  (cond ((ceh--inside-string)
	 (ceh--f-step-out-of-string))
	((not (ceh--f-sexp))
	 (ceh--f-peekrs " )")
	 (ceh--f-peekrs " ]"))
	(t
	 (ceh--f-peekrs " \""))))

(defun ceh--transform (a b &optional before-insert finally condition)
  (if (and (ceh--b-peek a) (if condition (funcall condition) t))
      (progn (delete-char (- 0 (length a)))
	     (if before-insert (funcall before-insert))
	     (insert b)
	     (if finally (funcall finally))
	     t)
    nil))

(defun ceh-expand ()
  (interactive)
  (cond
   ((ceh--transform "." "" nil 'ceh-guess-expand 'ceh--eol))
   ((ceh--transform "/e" "" nil 'ceh-comment-to-eol))
   ((ceh--transform "/a" "" nil 'ceh-comment-next-atom))
   ;; recursives first
   ((ceh--transform " <= " "<=" nil 'ceh-expand))
   ((ceh--transform " >= " ">=" nil 'ceh-expand))
   ((ceh--transform " == " "==" nil 'ceh-expand))
   ((ceh--transform " || " "||" nil 'ceh-expand))
   ((ceh--transform " && " "&&" nil 'ceh-expand))
   ((ceh--transform ", " "," nil 'ceh-expand))
   ((ceh--transform "; " ";" nil 'ceh-expand 'ceh--not-eol))
   ((ceh--transform " + " "+" nil 'ceh-expand))
   ((ceh--transform " - " "-" nil 'ceh-expand))
   ((ceh--transform " > " ">" nil 'ceh-expand))
   ((ceh--transform " < " "<" nil 'ceh-expand))
   ((ceh--transform " * " "*" nil 'ceh-expand))
   ((ceh--transform " / " "/" nil 'ceh-expand))
   ((ceh--transform "->" "-" nil 'ceh-expand))
   ;; construct
   ((ceh--transform "--" " - " 'ceh--f-expand))
   ((ceh--transform "-" "->" 'ceh--f-expand))
   ((ceh--transform ";" "; " 'ceh--f-expand nil 'ceh--not-eol))
   ((ceh--transform "<=" " <= " 'ceh--f-expand))
   ((ceh--transform ">=" " >= " 'ceh--f-expand))
   ((ceh--transform "==" " == " 'ceh--f-expand))
   ((ceh--transform "=" " = " 'ceh--f-expand))
   ((ceh--transform "." "." 'ceh--f-expand))
   ((ceh--transform "," ", " 'ceh--f-expand))
   ((ceh--transform "+" " + " 'ceh--f-expand))
   ((ceh--transform "*" " * " 'ceh--f-expand))
   ((ceh--transform "/" " / " 'ceh--f-expand))
   ((ceh--transform "&" " && " 'ceh--f-expand))
   ((ceh--transform "|" " || " 'ceh--f-expand))
   ((ceh--transform ">" " > " 'ceh--f-expand))
   ((ceh--transform "<" " < " 'ceh--f-expand))
   ;; fallback
   (t (ceh--expand-fallback))))


;; TODO: .h -> .cpp helper
;(defun ceh-decl-to-impl-namespace (namespace)
;  (interactive)
;  (beginning-of-line)
;  (end-of-sexp)
;  (forward-char)
;  (insert namespace)
;  (if (not (string= namespace ""))
;      (insert "::"))
;  (end-of-line)
;  (if (ceh--peekb? ";")
;      (progn (delete-char -1)
; 	     (insert " {")
; 	     (newline 2)
; 	     (insert "}")
; 	     (indent-for-tab-command)
; 	     (newline)
; 	     (previous-line 2)
; 	     (indent-for-tab-command))))
;
;(defun ceh-decl-to-impl ()
;  (interactive)
;  (end-of-line)
;  (if (ceh--peekb? ";")
;      (progn (delete-char -1)
; 	     (insert " {")
; 	     (newline 2)
; 	     (insert "}")
; 	     (indent-for-tab-command)
; 	     (newline)
; 	     (previous-line 2)
; 	     (indent-for-tab-command))))
;
;(defun ceh-decl-to-impl-n (namespace)
;  (interactive "sNamespace: ")
;  (while (search-forward ";" nil t 1)
;    (ceh-decl-to-impl-namespace namespace)))

;;//- mode definition -
(define-minor-mode ceh-mode
  "C Edit Helper - mode for enhancing C - like languages editing"
  :lighter " CEH"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "<M-return>") 'ceh-new-brace)
	    (define-key map (kbd "<S-return>") 'ceh-finish-expression)
	    (define-key map (kbd "C-(") 'ceh-parametrize)
	    (define-key map (kbd "C-)") 'ceh-unparametrize)
	    (define-key map (kbd "C-\"") 'ceh-stringize-line)
	    (define-key map (kbd "M-,") 'ceh-step-in-args)
	    (define-key map (kbd "M-.") 'ceh-step-out-of-args)
	    (define-key map (kbd "C-' s") 'ceh-transpose-atoms)
	    (define-key map (kbd "C-' d") 'ceh-leave-atom)
	    (define-key map (kbd "TAB") 'ceh-expand)
	    (define-key map (kbd "<tab>") 'ceh-expand)
	    (define-key map (kbd "C-.") 'ceh-next-argument)
	    (define-key map (kbd "C-,") 'ceh-previous-argument)
	    map)
  ;; chords
  (when (require 'key-chord nil 'noerror)
    (key-chord-define-global "qq" 'ceh-kill-line)
    (key-chord-define-global "[[" 'ceh-include-block)
    (key-chord-define-global "]]" 'ceh-exclude-block)
    (key-chord-define-global "[]" 'ceh-create-block)
    (key-chord-mode +1)))

(provide 'ceh)
;;; ceh.el ends here

;;; required libraries
(require 's)

;;; ===========================================================================
;;; common utilities
(defun mw-region-op (op)
  "Call function 'op' on a region of text. The text is replaced with
   the result of the function call.
   'op' should take only one argument -- the string of the region"
  (interactive "X")
  (let* ((begin (if (use-region-p) (region-beginning) (point-min)))
	 (end (if (use-region-p) (region-end) (point-max)))
	 (input-string (buffer-substring-no-properties begin end))
	 (output-string (funcall op input-string)))
    (save-excursion
      (delete-region begin end)
      (goto-char begin)
      (insert output-string))))

;;; ===========================================================================
;;; nikola utility function
(defun nikola-set-entry (entry-pair)
  (concat ".. " (car entry-pair) ": " (cdr entry-pair)))

(defun nikola-wrap (&rest rest)
  (concat "#+BEGIN_COMMENT\n"
	  (mapconcat 'identity rest "\n")
	  "\n#+END_COMMENT\n"))

(defun nikola-set-properties (title slug tags category link description)
  "Prompt and set utilities that is needed by nikola blogging system."
  (interactive "sTitle: \nsSlug: \nsTags(comma seperated): \nsCategory: \nsLink: \nsDescription: ")
  (let ((meta
	 (nikola-wrap
	  (nikola-set-entry (cons "title" title))
	  (nikola-set-entry (cons "slug" slug))
	  (nikola-set-entry (cons "date" (format-time-string "%Y-%m-%d %T UTC%z" (current-time))))
	  (nikola-set-entry (cons "tags" (mapconcat (lambda (tag) (s-trim tag))
						    (s-split "," tags)
						    ", ")))
	  (nikola-set-entry (cons "category" category))
	  (nikola-set-entry (cons "link" link))
	  (nikola-set-entry (cons "description" description))
          (nikola-set-entry (cons "type" "text")))))
    (save-excursion
      (goto-char (point-min))
      (insert meta))))

(provide 'utility-functions)

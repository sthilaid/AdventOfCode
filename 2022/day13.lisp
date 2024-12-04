

;; (with-output-to-string (out)
;;   (with-open-file (in "day13.testinput")
;;     (loop with buffer = (make-array 8192 :element-type 'character)
;;           for n-characters = (read-sequence buffer in)
;;           while (< 0 n-characters)
;;           do (write-sequence buffer out :start 0 :end n-characters)))
;;   (print out))

(defun parse (str)
  (cond ((eq? (char str 0) #\[)
         (let ((str* (parse (subseq str 1 (length str)))))
           (list)))
        ((eq? (char str 0) #\])
         (list))
        (t (let ((end (search "]" (subseq str 1 (length str)))))
             ))))

(with-open-file (stream "day13.testinput")
  (loop for line = (read-line stream nil 'end-of-file)
        until (eq line 'end-of-file)
        do (print line)))
(terpri)

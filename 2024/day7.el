(defmacro inc! (var &optional val) `(progn (setq ,var (+ ,var ,(or val 1))) ,var))

(defun parse (input)
  (mapcar (lambda (x) (let ((y (split-string x ":")))
                        (list (string-to-number (car y))
                              (cdr (mapcar 'string-to-number (split-string (cadr y) " "))))))
          (split-string input "\n")))

(defun gen-combinations (operators numbers)
  (if (<= (length numbers) 1)
      numbers
    (let ((combinations nil))
      (let ((allrest (gen-combinations operators (cdr numbers))))
        (seq-doseq (op operators)
          (seq-doseq (rest allrest)
            (push `(,op ,rest ,(car numbers)) combinations))))
      combinations)))


(defun sum-all-valid-equations (equations operators)
  (setq i 0)
  (seq-reduce (lambda (sum equation)
                (catch 'break (seq-let (result numbers) equation
                                (message (format "%d / %d sum: %s (%s)" i (length equations) sum (< sum most-positive-fixnum))) (inc! i)
                                (let ((combinations (gen-combinations operators (reverse numbers))))
                                  (seq-doseq (eqn combinations)
                                    (let ((current-result (eval eqn)))
                                      ;;(message (format "  %s = %s (%s)" eqn current-result result))
                                      (if (= result current-result)
                                          (throw 'break (+ sum result)))))))
                       sum))
              equations
              0))

(defun solve1 (parsed-input)
  (sum-all-valid-equations parsed-input '(+ *)))

(defun combine (x y) (+ (* x (expt 10 (ceiling (+ 0.000001 (log y 10))))) y))
(defun solve2 (parsed-input)
  (sum-all-valid-equations parsed-input '(+ * combine)))

(defun solve (input-file)
  (let* ((input (if (file-exists-p input-file)
                    (with-temp-buffer (insert-file-contents input-file)
                                      (buffer-string))
                  (error "Could not find file %s" input-file))))
    (let* ((now (float-time))
           (parsed-input (parse input))
           (part1 (solve1 parsed-input))
           (part2 (solve2 parsed-input)))
      (message "part1: %s | part2: %s (evaluated in %.3f sec)" part1 part2 (- (float-time) now)))))

(let* ((read-answer-short t)
       (answer (read-answer "run testinput or input?" '(("testinput" ?t "run test input") ("input" ?i "run input")))))
  (solve (concat (file-name-base (file-name-nondirectory (buffer-file-name (current-buffer)))) "." answer)))

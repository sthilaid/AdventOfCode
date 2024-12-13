;; -- utilities --
;; (defmacro inc! (var &optional val) `(progn (setq ,var (+ ,var ,(or val 1))) ,var))
;; (defmacro fm (fmt &rest args) `(message (format ,fmt ,@args)))
;; (defmacro reduce-indexed (pred seq init)
;;   (let ((index (gensym))
;;         (val (gensym))
;;         (acc (gensym)))
;;     `(let ((,index 0)
;;            (,acc ,init))
;;        (seq-doseq (,val ,seq)
;;          (setq ,acc (funcall ,pred ,acc ,index ,val))
;;          (inc! ,index))
;;        ,acc)))

(defmacro iterate (f init count)
  (let ((i (gensym))
        (v (gensym)))
    `(let ((,v ,init))
       (dotimes (,i ,count)
         (setq ,v (funcall ,f ,v)))
       ,v)))

;; -- problem --

(defun parse (input)
  (mapcar 'string-to-number (split-string input " ")))

(defun step (stones)
  (message "%s" (length stones))
  (flatten-tree
   (mapcar (lambda (stone)
             (if (= stone 0) 1
               (let ((digit-count (ceiling (+ 0.0001 (log stone 10)))))
                 (if (= (mod digit-count 2) 0)
                     (let* ((half-base (expt 10 (/ digit-count 2)))
                            (high (/ stone half-base))
                            (low (- stone (* high half-base))))
                       (list high low))
                   (* stone 2024)))))
           stones)))

(defun build-table ()
  '((0 4 (list 2 0 2 4))
    (1 3 (list 2 0 2 4))
    (2 3 (list 4 0 4 8))
    (3 3 (list 6 0 7 2))
    (4 3 (list 8 0 9 6))
    (5 5 (list 2 0 4 8 2 8 8 0))
    (6 5 (list 2 4 5 7 9 4 5 6))
    (7 5 (list 2 8 6 7 6 0 3 2))
    (8 5 (list 3 2 7 7 2 6 16192))
    (9 5 (list 3 6 8 6 9 1 8 4))
    ))

(defun solve1 (parsed-input)
  (length (iterate 'step stones 25)))

(defun solve2 (parsed-input)
  nil)

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
;; -- utilities --
(defmacro inc! (var &optional val) `(progn (setq ,var (+ ,var ,(or val 1))) ,var))
(defmacro let? (var val code) `(let ((,var ,val)) (and ,var ,code)))
(defmacro for (bindings comp inc result &rest body)
  `(let ,bindings
     (catch 'break
       (while ,comp
         ,@body
         ,inc))
     ,result))
(defmacro foreach-indexed (i v seq result &rest body)
  (let ((count (gensym)))
    `(let ((,count (length ,seq)))
       (for ((,i 0)) (< i ,count) (inc! i) ,result
         (let ((,v (elt ,seq ,i)))
           ,@body)))))
(defmacro fm (fmt &rest args) `(message (format ,fmt ,@args)))
;; (defmacro add-to-dict! (key val dict)
;;   (let ((entry (gensym)))
;;     `(let ((,entry (assq ,key ,dict)))
;;        (if ,entry
;;            (push ,val (elt ,entry 1))
;;          (push (list ,key (list ,val)) ,dict)))))

;; -- problem --

(defun parse (input)
  (let ((mem-blocks nil))
    (foreach-indexed i char input (reverse mem-blocks)
                     (let ((is-file? (= (mod i 2) 0))
                           (num (string-to-number (string char))))
                       (dotimes (j num) (push (and is-file? (/ i 2)) mem-blocks))))))

(defun compact-blocks (mem-blocks)
  (defun swap (v i j) (let ((tmp (aref v i))) (aset v i (aref v j)) (aset v j tmp)))
  (let ((vmem-blocks (seq-into mem-blocks 'vector)))
    (for ((i 0)
          (j (- (length vmem-blocks) 1)))
         (> j i) (inc! j -1) vmem-blocks
         ;;(fm "%s i: %d j: %d" vmem-blocks i j)
         (let ((vi (aref vmem-blocks i)))
           (while (and vi (> j i)) (inc! i) (setq vi (aref vmem-blocks i)))
           (if (not vi) (swap vmem-blocks i j))))))

(defun solve1 (parsed-input)
  (seq-reduce '+
              (seq-filter (lambda (x) x)
                          (seq-map-indexed (lambda (x i) (if x (* x i) nil))
                                           (compact-blocks parsed-input)))
              0))

(defun compact-files (mem-blocks)
  (defun find-space (start size blocks)
    (catch 'ret (let ((empty-size 0)) (foreach-indexed i v blocks nil
                                                       (if (> i start) (throw 'ret nil))
                                                       (if v (if (>= empty-size size) (throw 'ret (cons i empty-size))
                                                               (setq empty-size 0))
                                                         (inc! empty-size))))))
  (let* ((vmem-blocks (seq-into mem-blocks 'vector))
         (file-infos (let ((id 0) (start 0) (size 0) (infos nil))
                       (foreach-indexed i v vmem-blocks nil
                                        (if (eq v id) (inc! size)
                                          (progn (push (list id start size) infos)
                                                 (setq start i)
                                                 (setq id v)
                                                 (setq size 1))))
                       (push (list id start size) infos)))
         (i 0) (count (length file-infos)))
    (seq-doseq (info file-infos)
      (fm "%d / %d" i count) (inc! i)
      (seq-let (id start size) info
        (and id (let? empty-blocks (find-space start size vmem-blocks)
                      (let* ((empty-size (cdr empty-blocks))
                             (empty-start (- (car empty-blocks) empty-size)))
                        ;;(fm "id: %s empty-blocks: %s" id empty-blocks)
                        (dotimes (i size) (progn (aset vmem-blocks (+ empty-start i) id)
                                                 (aset vmem-blocks (+ start i) nil))))))))
    vmem-blocks))

(defun solve2 (parsed-input)
  (seq-reduce '+
              (seq-filter (lambda (x) x)
                          (seq-map-indexed (lambda (x i) (and x (* x i)))
                                           (compact-files parsed-input)))
              0)
  ;(compact-files parsed-input)
  )

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

;; -- utilities --
(defmacro inc! (var &optional val) `(progn (setq ,var (+ ,var ,(or val 1))) ,var))
(defmacro fm (fmt &rest args) `(message (format ,fmt ,@args)))
(defmacro reduce-indexed (pred seq init)
  (let ((index (gensym))
        (val (gensym))
        (acc (gensym)))
    `(let ((,index 0)
           (,acc ,init))
       (seq-doseq (,val ,seq)
         (setq ,acc (funcall ,pred ,acc ,index ,val))
         (inc! ,index))
       ,acc)))

;; -- problem --

(defun parse (input)
  (let ((w (length (seq-take-while (lambda (x) (not (eq x ?\n))) input)))
        (h (+ 1 (seq-count (lambda (x) (eq x ?\n)) input))))
    (list w h (seq-into (mapcar (lambda (x) (string-to-number (string x)))
                                (seq-filter (lambda (x) (not (eq x ?\n))) input))
                        'vector))))

(defun i-to-xy (i w h)
  (let ((x (mod i w))
        (y (/ i w)))
    (list x y)))

(defun xy-to-i (x y w h)
  (+ x (* y w)))

(defun find-trails (x y target-h topography w h trail is-uniq?)
  (let ((i (xy-to-i x y w h)))
      (if (or (< x 0) (< y 0) (>= x w) (>= y h)
              (seq-find (lambda (x) (= x i)) trail))
          nil
        (let* ((height (aref topography i)))
          (cond ((and (= height target-h) (= height 9)) (list i))
                ((not (= height target-h)) nil)
                (t (let ((up (find-trails x (- y 1) (+ height 1) topography w h (cons i trail) is-uniq?))
                         (down (find-trails x (+ y 1) (+ height 1) topography w h (cons i trail) is-uniq?))
                         (left (find-trails (- x 1) y (+ height 1) topography w h (cons i trail) is-uniq?))
                         (right (find-trails (+ x 1) y (+ height 1) topography w h (cons i trail) is-uniq?)))
                     (if is-uniq?
                         (seq-uniq (seq-concatenate 'list up down left right))
                       (seq-concatenate 'list up down left right)))))))))

(defun solve1 (parsed-input)
  (seq-let (w h topography) parsed-input
    (reduce-indexed (lambda (acc i _) (+ acc (seq-let (x y) (i-to-xy i w h)
                                               (length (find-trails x y 0 topography w h nil t)))))
                    topography 0)))

(defun solve2 (parsed-input)
  (seq-let (w h topography) parsed-input
    (reduce-indexed (lambda (acc i _) (+ acc (seq-let (x y) (i-to-xy i w h)
                                               (length (find-trails x y 0 topography w h nil nil)))))
                    topography 0)))

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

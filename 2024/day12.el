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

;; -- problem --

(defun parse (input)
  (let ((w (length (seq-take-while (lambda (x) (not (eq x ?\n))) input)))
        (h (+ 1 (seq-count (lambda (x) (eq x ?\n)) input))))
    (list w h (seq-into (seq-filter (lambda (x) (not (eq x ?\n))) input)
                        'vector))))

(defun i-to-xy (i w h)
  (let ((x (mod i w))
        (y (/ i w)))
    (list x y)))

(defun xy-to-i (x y w h)
  (+ x (* y w)))

(defun is-valid? (x y w h)
  (and (>= x 0) (< x w)
       (>= y 0) (< y h)))

(defun accumulate-region (i v garden w h)
  (message "%s" `(accumulate-region ,i ,v))
  (defun is-in-region (x y) (and (is-valid? x y w h)
                                 (eq v (aref garden (xy-to-i x y w h)))))
  (defmacro try-explore (dx dy)
    `(let* ((new-x (+ x ,dx))
            (new-y (+ y ,dy))
            (is-good? (is-in-region new-x new-y)))
       (if is-good?
           (let ((new-i (xy-to-i new-x new-y w h)))
             (and (not (memq new-i region))
                  (progn (push new-i region)
                         (push new-i to-explore))))
         (progn (message "i: %s (%s, %s)new-i: %s (%s,%s) | perimeter: %d" (xy-to-i x y w h) x y (xy-to-i new-x new-y w h) new-x new-y perimeter)
                (inc! perimeter)))))
  
  (let ((region nil)
        (to-explore (list i))
        (perimeter 0))
    (while to-explore
      (seq-let (x y) (i-to-xy (car to-explore) w h)
        (setq to-explore (cdr to-explore))
        (try-explore 0 1)
        (try-explore 0 -1)
        (try-explore 1 0)
        (try-explore -1 0)))
    (list region perimeter)))

(defun solve1 (parsed-input)
  (seq-let (w h garden) parsed-input
    (let ((region-infos nil)
          (total 0))
      (foreach-indexed i v garden total
                       (if (not (seq-find (lambda (ri) (seq-find (lambda (x) (= x i)) (car ri)))
                                          region-infos))
                           (progn (push (accumulate-region i v garden w h) region-infos)
                                  (seq-let (region region-perimiter) (car region-infos)
                                    (inc! total (* region-perimiter (length region)))))))
      total
      region-infos)))

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

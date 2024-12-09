;; -- utilities --
(defmacro inc! (var &optional val) `(progn (setq ,var (+ ,var ,(or val 1))) ,var))
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
(defmacro add-to-dict! (key val dict)
  (let ((entry (gensym)))
    `(let ((,entry (assq ,key ,dict)))
       (if ,entry
           (push ,val (elt ,entry 1))
         (push (list ,key (list ,val)) ,dict)))))

;; -- problem --

(defun parse (input)
  (let ((w (length (seq-take-while (lambda (x) (not (eq x ?\n))) input)))
        (h (+ 1 (seq-count (lambda (x) (eq x ?\n)) input))))
    (list w h (seq-filter (lambda (x) (not (eq x ?\n))) input))))

(defun i-to-xy (i w h)
  (let ((x (mod i w))
        (y (/ i w)))
    (cons x y)))

(defun xy-to-i (xy w h)
  (+ (car xy) (* (cdr xy) w)))

(defun compute-delta (loc1-xy loc2-xy w h)
  (cons (- (car loc2-xy) (car loc1-xy))
        (- (cdr loc2-xy) (cdr loc1-xy))))

(defun compute-antena-dict (w h antenas)
  (let ((dict nil))
    (foreach-indexed i v antenas nil
                     (if (not (eq v ?.))
                         (let ((entry (assq v dict)))
                           (if entry
                               (push i (cadr entry))
                             (push (list v (list i)) dict)))))
    dict))

(defun is-valid-xy (xy w h)
  (and (>= (car xy) 0) (< (car xy) w)
       (>= (cdr xy) 0) (< (cdr xy) h)))

(defun calc-loc (xy dxy op)
  (cons (funcall op (car xy) (car dxy))
        (funcall op (cdr xy) (cdr dxy))))

(defun solve1 (parsed-input)
  (seq-let (w h antenas) parsed-input
    (let ((dict (compute-antena-dict w h antenas))
          (antinodes nil))
      (seq-doseq (entry dict)
        (seq-let (antena locations) entry
          (seq-doseq (loc locations) ; will do locA -> locB and also lobA <- locB... to optimize
            (let ((other-locations (seq-filter (lambda (x) (not (= x loc)))
                                               locations)))
              (seq-doseq (other-loc other-locations)
                (let* ((loc-xy (i-to-xy loc w h))
                       (other-loc-xy (i-to-xy other-loc w h))
                       (dxy-to-otherloc (compute-delta loc-xy other-loc-xy w h))
                       (dxy-to-loc (compute-delta other-loc-xy loc-xy w h))
                       (anti-1-xy (calc-loc loc-xy dxy-to-loc '+))
                       (anti-2-xy (calc-loc other-loc-xy dxy-to-otherloc '+)))
                  (if (is-valid-xy anti-1-xy w h) (add-to-dict! antena (xy-to-i anti-1-xy w h) antinodes))
                  (if (is-valid-xy anti-2-xy w h) (add-to-dict! antena (xy-to-i anti-2-xy w h) antinodes))))))))
      (seq-count (lambda (x) x) (seq-uniq (apply 'seq-concatenate 'list (mapcar (lambda (x) (elt x 1)) antinodes)))))))

(defun solve2 (parsed-input)
  (seq-let (w h antenas) parsed-input
    (let ((dict (compute-antena-dict w h antenas))
          (antinodes nil))
      (seq-doseq (entry dict)
        (seq-let (antena locations) entry
          (seq-doseq (loc locations) ; will do locA -> locB and also lobA <- locB... to optimize
            (let ((other-locations (seq-filter (lambda (x) (not (= x loc)))
                                               locations)))
              (seq-doseq (other-loc other-locations)
                (let* ((loc-xy (i-to-xy loc w h))
                       (other-loc-xy (i-to-xy other-loc w h))
                       (dxy-to-otherloc (compute-delta loc-xy other-loc-xy w h))
                       (dxy-to-loc (compute-delta other-loc-xy loc-xy w h)))
                  (let ((anti-loc-xy loc-xy))
                    (while anti-loc-xy
                      (if (is-valid-xy anti-loc-xy w h)
                          (progn (add-to-dict! antena (xy-to-i anti-loc-xy w h) antinodes)
                                 (setq anti-loc-xy (calc-loc anti-loc-xy dxy-to-loc '+)))
                        (setq anti-loc-xy nil))))
                  (let ((anti-loc-xy other-loc-xy))
                    (while anti-loc-xy
                      (if (is-valid-xy anti-loc-xy w h)
                          (progn (add-to-dict! antena (xy-to-i anti-loc-xy w h) antinodes)
                                 (setq anti-loc-xy (calc-loc anti-loc-xy dxy-to-otherloc '+)))
                        (setq anti-loc-xy nil))))))))))
      (seq-count (lambda (x) x) (seq-uniq (apply 'seq-concatenate 'list (mapcar (lambda (x) (elt x 1)) antinodes)))))))

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

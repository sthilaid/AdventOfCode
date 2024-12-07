(defmacro inc! (var) `(progn (setq ,var (+ ,var 1)) ,var))
(defmacro dec! (var) `(progn (setq ,var (- ,var 1)) ,var))
(defmacro let? (var val code) `(let ((,var ,val)) (and ,var ,code)))
(defmacro for (bindings comp inc result &rest body)
  `(let ,bindings
     (catch 'break
       (while ,comp
         ,@body
         ,inc))
     ,result))

(defun parse (input)
  (let ((w (for ((i 0) (c 0)) (< i (length input)) (inc! i) c
                (if (eq (elt input i) ?\n) (throw 'break nil))
                (inc! c)))
        (h (+ 1 (seq-count (lambda (c) (eq c ?\n)) input))))
    (let ((level (make-vector (* w h) 0))
          (start nil))
      (for ((i 0) (j 0)) (< i (length input)) (inc! i) nil
           (let ((c (elt input i)))
             (if (eq c ?#) (aset level j nil))
             (if (eq c ?^) (progn (setq start j) (aset level j 1)))
             (if (not (eq c ?\n)) (inc! j))))
      (list w h level start))))

(defun validate (i min max) (if (or (< i min) (> i max)) nil i))
(defun i-to-coord (i w h)
  (let ((x (mod i w)) (y (/ i w)))
    (list (validate x 0 w) (validate y 0 h))))

(defun coord-to-i (x y w h)
  (let ((i (+ x (* y w))))
    (if (or (< i 0) (> i (* w h))
            (< x 0) (>= x w)
            (< y 0) (>= x h))
        nil
      i)))

(defun rotate (dx dy)
  (cond ((and (= dx 0) (= dy 1)) (list -1 0))
        ((and (= dx 1) (= dy 0)) (list 0 1))
        ((and (= dx 0) (= dy -1)) (list 1 0))
        ((and (= dx -1) (= dy 0)) (list 0 -1))
        (t (error "unexpected dx: %s dy: %s" dx dy))))

(defun advance (level pos dx dy w h)
  ;;(message (format "(advance pos: %s dx: %s dy: %s" pos dx dy))
  (seq-let (x y) (i-to-coord pos w h)
    ;;(message (format "x: %s y: %s" x y))
    ;;(message-level level w h)
    (if (or (not x) (not y)) (error "invalid pos for %s x: %s y: %s" pos x y))
    (let ((new-pos (coord-to-i (+ x dx) (+ y dy) w h)))
      (cond ((not new-pos) nil)
            ((not (aref level new-pos)) (seq-let (new-dx new-dy) (rotate dx dy)
                                          (list pos new-dx new-dy)))
            (t (list new-pos dx dy))))))

(defun message-level (level w h &optional blockers)
  (let ((str nil))
    (for ((i (- (length level) 1))) (>= i 0) (dec! i) (message (concat "" str))
         (let ((v (aref level i))
               (is-blocker (and blockers (seq-find (lambda (x) (= (car x) i)) blockers))))
           (setq str (concat (if is-blocker "X" (if v (number-to-string v) "#"))
                             (if (= (mod i w) (- w 1)) "\n" "")
                             str))))))

(defun solve1 (parsed-input)
  (seq-let (w h level pos) parsed-input
    (let ((dx 0) (dy -1))
      (while pos
        (seq-let (new-pos new-dx new-dy) (advance level pos dx dy w h)
          (if (and new-pos (not (= new-pos pos))) (aset level new-pos (+ 1 (aref level new-pos))))
          (setq pos new-pos)
          (setq dx new-dx)
          (setq dy new-dy))))
    ;;(message-level level w h)
    (seq-count (lambda (x) (and x (> x 0))) level)))

;; (defun exists-within (pred seq &optional start end)
;;   (catch 'ret (for ((i (or start 0))) (< i (or end (length seq))) (inc! i) nil
;;                    (if (funcall pred (elt seq i)) (throw 'ret t)))))

(defun solve2 (parsed-input)
  (seq-let (w h level pos) parsed-input
    (let* ((dx 0) (dy -1) (path `((,pos ,dx ,dy))))
      (while pos
        (seq-let (new-pos new-dx new-dy) (advance level pos dx dy w h)
          (if (and new-pos (not (= new-pos pos))) (aset level new-pos (+ 1 (aref level new-pos))))
          (push `(,new-pos ,new-dx ,new-dy) path)
          (setq pos new-pos)
          (setq dx new-dx)
          (setq dy new-dy)))
      
      (let ((search-path (cdr (reverse (cdr path))))
            (blockers '()))
        (for ((i 0)) (< i (length search-path)) (inc! i) nil
             (message "%d / %d" i (length search-path))
             (let ((p (elt search-path i)))
               (seq-let (pos dx dy) p
                 (let ((blocker-pos (advance level pos dx dy w h)))
                   (if (and blocker-pos
                            (aref level (car blocker-pos)))
                       (seq-let (rdx rdy) (rotate dx dy)
                         (let ((next `(,pos ,rdx ,rdy))
                               (search-map (make-vector (* w h) nil)))
                           (seq-doseq (p (seq-subseq search-path 0 i)) (aset search-map (car p) (list (cons (elt p 1) (elt p 2)))))
                           (catch 'break
                             (while next
                               (if (let? search-vals (aref search-map (car next))
                                         (seq-find (lambda (x) (and (= (car x) (cadr next)) (= (cdr x) (caddr next)))) search-vals))
                                   (progn (push blocker-pos blockers)
                                          (throw 'break nil))
                                 (seq-let (next-pos next-dx next-dy) next
                                   (aset search-map (car next) (cons (cons (cadr next) (caddr next)) (aref search-map (car next))))
                                   (setq next (advance level next-pos next-dx next-dy w h)))))))))))))
        ;;(message-level level w h blockers)
        (length blockers)))))

(defun solve (input-file)
  (defun current-line () (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
  (let* ((input (if (file-exists-p input-file)
                    (with-temp-buffer (insert-file-contents input-file)
                                      (buffer-string))
                  (error "Could not find file %s" input-file))))
    (let* ((parsed-input (parse input))
           (part1 (solve1 parsed-input))
           (part2 (solve2 parsed-input)))
      (message "part1: %s | part2: %s" part1 part2))))

(let* ((read-answer-short t)
       (answer (read-answer "run testinput or input?" '(("testinput" ?t "run test input") ("input" ?i "run input")))))
  (solve (concat (file-name-base (file-name-nondirectory (buffer-file-name (current-buffer)))) "." answer)))

(defmacro inc! (var) `(progn (setq ,var (+ ,var 1)) ,var))
;; (defmacro let? (var val code) `(let ((,var ,val)) (and ,var ,code)))
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
    (cons w h)))

(defun solve1 (parsed-input)
  parsed-input)

(defun solve2 (parsed-input)
  nil)

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


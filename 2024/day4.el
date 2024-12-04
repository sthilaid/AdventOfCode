(defmacro inc! (var) `(progn (setq ,var (+ ,var 1)) ,var))
(defmacro return (code) `(throw 'return ,code))

(defun find-puzzle-stride (str)
  (let ((i 0) (count (length str))) (catch 'return (while (< i count)
                                                     (if (eq (elt str i) ?\n) (return i)
                                                       (inc! i))))))
;; (defun find-adj (str i dx dy stride)
;;   (let ((adj (+ i )))))

(defun solve1 (input) input)
(defun solve2 (input) nil)

(defun solve (input-file)
  (defun current-line () (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
  (let* ((input (if (file-exists-p input-file)
                    (with-temp-buffer (insert-file-contents input-file)
                                      (buffer-string))
                  (error "Could not find file %s" input-file))))
    (let* ((part1 (solve1 input))
           (part2 (solve2 input)))
      (message "part1: %s | part2: %s" part1 part2))))

(solve (concat (file-name-base (file-name-nondirectory (buffer-file-name (current-buffer)))) ".testinput"))
;;(solve (concat (file-name-base (file-name-nondirectory (buffer-file-name (current-buffer)))) ".input"))


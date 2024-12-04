
(defun sum (values) (seq-reduce '+ values 0))
(defun split-in-two (acc v) (list (cons (car v) (car acc))
                                  (cons (cadr v) (cadr acc))))
(defun solve1 (input)
  (sum (apply 'seq-mapn
              (lambda (l r) (abs (- l r)))
              (mapcar (lambda (l) (seq-sort '< l))
                      (seq-reduce 'split-in-two input (list nil nil))))))

(defun solve2 (input)
  (let* ((ls (seq-reduce 'split-in-two input (list nil nil)))
         (l1 (car ls))
         (l2 (cadr ls)))
    (defun find-sim (v) (seq-reduce (lambda (acc x) (if (= x v) (+ acc x) acc))
                                    l2 0))
    (sum (mapcar 'find-sim l1))))

(defun solve (input-file)
  (defun current-line () (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
  (let* ((input (if (file-exists-p input-file)
                    (with-temp-buffer (insert-file-contents input-file)
                                      (beginning-of-buffer)
                                      (let ((data '()))
                                        (while (< (point) (point-max))
                                          (push (mapcar 'string-to-number (split-string (current-line)))
                                                data)
                                          (forward-line))
                                        data))
                  (error "Could not find file %s" input-file))))
    (let* ((part1 (solve1 input))
           (part2 (solve2 input)))
      (message "part1: %s | part2: %s" part1 part2))))

(solve "day1.testinput")
(solve "day1.input")



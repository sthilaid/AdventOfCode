
(defun simulate-fishes (fishes num-days)
  (dotimes (i num-days)
    (cl-maplist (lambda (fish-cons-cell)
                  (let ((fish (car fish-cons-cell)))
                    (if (= fish 0)
                        (progn (setcar fish-cons-cell 6)
                               (push 8 fishes))
                      (setcar fish-cons-cell (- fish 1)))))
                fishes)))

(defun simulate-fishes-faster (fishes num-days)
  (let* ((bucket-count 9)
         (fish-bucket (make-vector bucket-count 0)))
    (dolist (fish fishes)
      (aset fish-bucket fish (1+ (aref fish-bucket fish))))
    
    (dotimes (_ num-days)
      (let ((zero-fish-count (aref fish-bucket 0)))
        (dotimes (bucket-delta 8)
          (let ((bucket-id (1+ bucket-delta))) ; index [1, 8]
            (aset fish-bucket (- bucket-id 1) (aref fish-bucket bucket-id))))
        (aset fish-bucket 6 (+ (aref fish-bucket 6) zero-fish-count))
        (aset fish-bucket 8 zero-fish-count)))
    (seq-reduce '+ fish-bucket 0)))

(defun solve-part1 (fishes)
  (simulate-fishes-faster fishes 80))

(defun solve-part2 (fishes)
  (simulate-fishes-faster fishes 256))

(defun solve (inpu-file)
  (let ((input-string (if (file-exists-p input-file)
                          (with-temp-buffer
                            (insert-file-contents input-file)
                            (buffer-string))
                        (message (concat "Could not find file " input-file)))))
    (let* ((init-fishes (mapcar 'string-to-number (split-string input-string ",")))
           (part1 (solve-part1 init-fishes))
           (part2 (solve-part2 init-fishes)))
      (format "part1: %d | part2: %d" part1 part2))))

(solve "day6-test-input.txt")
(solve "day6-input.txt")

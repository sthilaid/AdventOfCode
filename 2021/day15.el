(defun solve (input-file)
  (let* ((input (if (file-exists-p input-file)
                    (with-temp-buffer
                      (insert-file-contents input-file)
                      ())
                  (message (concat "Could not find file " input-file)))))
    (let* ((initial-polymer (first input))
           (rules (second input))
           (part1 (solve-part1 (copy-tree initial-polymer) (copy-tree rules)))
           (part2 (solve-part2 initial-polymer rules)))
      (message "part1: %s | part2: %s" part1 part2))))

(solve "day15-test-input.txt")
(solve "day15-input.txt")

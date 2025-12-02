;;; ...  -*- lexical-binding: t -*-

(defun parse (input)
  (defun eval-input (i) (* (let ((side (car i)))
                       (cond ((string= side "L") -1)
                             ((string= side "R") 1)))
                     (cdr i)))
  (mapcar 'eval-input
          (mapcar (lambda (x) (cons (substring x 0 1)
                                    (string-to-number (substring x 1))))
                  (seq-filter (lambda (y) (not (string= y ""))) (split-string input "\n")))))

(defun solve1 (inputs)
  (named-let count-zeroes ((inputs inputs)
                           (current 50)
                           (count 0))
      (if (not inputs)
        count
      (let ((new (mod (+ current (car inputs)) 100)))
        (count-zeroes (cdr inputs)
                     new
                     (if (= new 0) (+ count 1) count))))))

(defun solve2 (inputs)
  (named-let count-zeroes ((inputs inputs)
                           (current 50)
                           (count 0))
    (if (not inputs)
        count
      (let* ((delta (car inputs))
             (sum (+ current delta))
             (new (mod sum 100))
             (extra (if (and (< delta 0) (<= sum 0) (not (= current 0)))
                        1
                      0))
             )
        ;;(message "current: %d delta: %d sum: %d new: %d extra: %d count: %d" current delta sum new extra (+ count extra (abs (/ sum 100)) ))
        (count-zeroes (cdr inputs)
                      new
                      (+ count (abs (/ sum 100)) extra ))))))

;; (defun solve2 (inputs)
;;   (named-let simulate ((inputs inputs)
;;                        (current 50)
;;                        (count 0))
;;     (if (not inputs)
;;         count
;;       (let* ((delta (car inputs)))
;;         (cond ((= delta 0) (simulate (cdr inputs) current count))
;;               ((< delta 0) (let ((new (mod (1- current) 100)))
;;                              (simulate (cons (1+ delta) (cdr inputs)) new (if (= new 0) (1+ count) count))))
;;               ((> delta 0) (let ((new (mod (1+ current) 100)))
;;                              (simulate (cons (1- delta) (cdr inputs)) new (if (= new 0) (1+ count) count)))))))))

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
  (let ((w (if (string= answer "testinput") 11 101))
        (h (if (string= answer "testinput") 7 103)))
    (solve (concat (file-name-base (file-name-nondirectory (buffer-file-name (current-buffer)))) "." answer))))

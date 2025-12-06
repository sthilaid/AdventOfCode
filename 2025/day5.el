;;; ...  -*- lexical-binding: t -*-

(defun parse (input)
  (pcase (split-string input "\n\n")
    (`(,raw-fresh ,raw-available)
     (let ((fresh (mapcar (lambda (f) (mapcar 'string-to-number (split-string f "-"))) (split-string raw-fresh "\n")))
           (available (mapcar 'string-to-number (split-string raw-available "\n"))))
       (list fresh available)))))

(defun inc-if (x test) (if test (1+ x) x))

(defun is-in-range? (x range)
  (and (>= x (car range))
       (<= x (cadr range))))

(defun is-in-ranges? (x ranges)
  (and ranges
       (or (is-in-range? x (car ranges))
           (is-in-ranges? x (cdr ranges)))))

(defun merge-ranges (new-range all-ranges)
  (if (not all-ranges)
      (list new-range)
    (pcase new-range
      (`(,new-start ,new-end)
       (named-let merging ((new-start new-start)
                           (new-end new-end)
                           (ranges all-ranges)
                           (merged nil))
         (cond ((not ranges) (cons (list new-start new-end) merged))
               (t (let* ((current-range (car ranges))
                         (rstart (car current-range))
                         (rend (cadr current-range))
                         (new-range (list new-start new-end)))
                    (if (or (is-in-range? new-start current-range)
                            (is-in-range? new-end current-range)
                            (is-in-range? (car current-range) new-range)
                            (is-in-range? (cadr current-range) new-range))
                        (merge-ranges (list (min new-start rstart)
                                            (max new-end rend))
                                      (seq-remove (lambda (x) (eq x current-range)) all-ranges))
                      (merging new-start
                               new-end
                               (cdr ranges)
                               (cons (car ranges) merged)))))))))))

(defun range-count (range)
  (1+ (- (cadr range) (car range))))

(defun solve1 (inputs)
  (pcase inputs
    (`(,fresh ,available)
     (let ((fresh-ranges (named-let merge-fresh-ranges ((fresh fresh)
                                                        (merged '()))
                           (if (not fresh)
                               merged
                             (merge-fresh-ranges (cdr fresh) (merge-ranges (car fresh) merged))))))
       (named-let check-available ((available available)
                                   (fresh-count 0))
         (if (not available) fresh-count
           (check-available (cdr available) (inc-if fresh-count (is-in-ranges? (car available) fresh-ranges)))))))))

(defun solve2 (inputs)
  (pcase inputs
    (`(,fresh ,available)
     (let ((fresh-ranges (named-let merge-fresh-ranges ((fresh fresh)
                                                        (merged '()))
                           (if (not fresh)
                               merged
                             (merge-fresh-ranges (cdr fresh) (merge-ranges (car fresh) merged))))))
       (named-let count-fresh ((fresh-ranges fresh-ranges)
                               (count 0))
         (if (not fresh-ranges) count
           (count-fresh (cdr fresh-ranges) (+ count (range-count (car fresh-ranges))))))))))

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

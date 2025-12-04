;;; ...  -*- lexical-binding: t -*-

(defun parse (input)
  (mapcar (lambda (x) (mapcar 'string-to-number (split-string x "-"))) (split-string input ",")))

;; (defun is-invalid? (str-id)
;;   (let ((l (length str-id)))
;;     (and (>= l 2)
;;          (= 0 (mod l 2))
;;          (let* ((pat-size (/ l 2))
;;                 (pat (substring str-id 0 pat-size))
;;                 (rest (substring str-id pat-size)))
;;            (string= pat rest)))))

;; (defun solve1 (inputs)
;;   (named-let acc-invalids ((inputs inputs)
;;                            (invalids '()))
;;     (if (not inputs)
;;         (seq-reduce '+ invalids 0)
;;       (acc-invalids (cdr inputs)
;;                     (append invalids (named-let find-invalids ((from (car (car inputs)))
;;                                                                (to-num (string-to-number (cadr (car inputs))))
;;                                                                (inv '()))
;;                                        (let ((from-num (string-to-number from)))
;;                                          (if (> from-num to-num)
;;                                              inv
;;                                            (find-invalids (number-to-string (1+ from-num))
;;                                                           to-num
;;                                                           (if (is-invalid? from) (cons from-num inv) inv))))))))))

(defun accumulate-invalids (inputs invalids test-fun?)
  (if (not inputs)
      ;(seq-reduce '+ invalids 0)
      invalids
      (accumulate-invalids (cdr inputs)
                           (append invalids
                                   (named-let for ((from (caar inputs))
                                                   (to (cadar inputs))
                                                   (inv '()))
                                     (if (> from to)
                                         inv
                                       (for (1+ from) to (if (funcall test-fun? from) (cons from inv) inv)))))
                           test-fun?)))

(defun solve1 (inputs)
  (defun is-invalid? (x)
    (let ((digit-count (ceiling (log x 10))))
      (and (= (mod digit-count 2))
           (let* ((mask (expt 10 (/ digit-count 2)))
                  (pat (/ x mask))
                  (rest (- x (* pat mask))))
             (= pat rest)))))
  (accumulate-invalids inputs '() (symbol-function 'is-invalid?)))

(defun split-num (x idx)
  (and (> x 10)
       (let ((digit-count (ceiling (log x 10))))
         (let ((first (/ x (expt 10 (- digit-count idx)))))
           (if (= first x)
               (list first nil)
             (list first
                   (- x (* first (expt 10 (- digit-count idx))))))))))

(defun is-invalid-2? (x)
  (let ((digit-count (ceiling (log x 10))))
    (named-let for ((i 1))
      (if (> i (/ digit-count 2))
          nil
        (let ((pat (split-num x i)))
          (or (named-let repeats? ((pat (car pat))
                                   (rest (cadr pat)))
                (or (not rest)
                    (let ((split (split-num rest i)))
                      (or (and (not split)
                               (= pat rest))
                          (and split
                               (= pat (car split))
                               (repeats? pat (cadr split)))))))
              (for (1+ i))))))))

;; todo - fix problem with zeros being voided, like in (is-invalid-2? 1000001)

(defun solve2 (inputs)
  (accumulate-invalids inputs '() (symbol-function 'is-invalid-2?)))

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


(defun has-won (card)
  (catch 'winner
    (dotimes (row-id 5)
      (catch 'not-winning-row
        (dotimes (column-id 5)
          (let ((index (+ (* row-id 5) column-id)))
            (if (numberp (elt card index))
                (throw 'not-winning-row t))))
        (throw 'winner t)))
    (dotimes (row-id 5)
      (catch 'not-winning-col
        (dotimes (column-id 5)
          (let ((index (+ (* column-id 5) row-id)))
            (if (numberp (elt card index))
                (throw 'not-winning-col t))))
        (throw 'winner t)))
    nil))

(defun solve-part1 (draws cards)
  (let ((winner (catch 'found-winner
                  (dolist (draw draws)
                    (dolist (card cards)
                      (catch 'found-num
                        (dotimes (i 25)
                          (let ((numcdr (nthcdr i card)))
                            (if (equal (car numcdr) draw)
                                (progn (setcar numcdr (list (car numcdr)))
                                       (throw 'found-num t))))))
                      (if (has-won card)
                          (throw 'found-winner (list card draw)))
                      ))
                  nil)))
    (if winner
        (let ((winning-card (first winner))
              (last-draw (second winner)))
          (* last-draw (seq-reduce (lambda (acc num) (if (numberp num) (+ acc num) acc))
                                   winning-card
                                   0))))))

(defun solve-part2 (draws cards)
  (let* ((cards-won-indices '())
         (winner (catch 'found-winner
                   (dolist (draw draws)
                     (dotimes (card-index (length cards))
                       (let ((card (elt cards card-index)))
                         (catch 'found-num
                           (dotimes (i 25)
                             (let ((numcdr (nthcdr i card)))
                               (if (equal (car numcdr) draw)
                                   (progn (setcar numcdr (list (car numcdr)))
                                          (throw 'found-num t))))))
                         (if (and (has-won card)
                                  (not (member card-index cards-won-indices)))
                             (progn (push card-index cards-won-indices)
                                    (if (= (length cards-won-indices) (length cards))
                                        (throw 'found-winner (list card draw))))))))
                   nil)))
    (if winner
        (let ((winning-card (first winner))
              (last-draw (second winner)))
          (* last-draw (seq-reduce (lambda (acc num) (if (numberp num) (+ acc num) acc))
                                   winning-card
                                   0))))))

(defun solve (input-file)
  (let* ((draws-string "")
         (cards-strings (let ((cards-strings '()))
                          (if (file-exists-p input-file)
                              (with-temp-buffer
                                (insert-file-contents input-file)
                                (beginning-of-buffer)
                                (setq draws-string (buffer-substring 1 (line-end-position)))
                                (forward-line 2)
                                (while (< (point) (point-max))
                                  (push (buffer-substring (line-beginning-position) (line-end-position 5)) cards-strings)
                                  (forward-line 6))
                                cards-strings)
                            (message (concat "Could not find file " input-file))))))
    (let* ((draws (mapcar 'string-to-number (split-string draws-string ",")))
           (cards (mapcar (lambda (c) (mapcar 'string-to-number (split-string c nil))) cards-strings))
           (part1 (solve-part1 draws cards))
           (part2 (solve-part2 draws cards)))
      (message "part1: %s | part2: %s" part1 part2))))

(solve "day4-test-input.txt")
(solve "day4-input.txt")

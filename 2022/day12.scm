
(define (split sep)
  (lambda (str)
    (call-with-input-string
     str
     (lambda (p)
       (read-all p (lambda (p) (read-line p sep)))))))

(define-structure map data width height)

(define (parse-map data)
  (let* ((lines ((split #\newline) data))
         (map-lists (map (lambda (x) (string->list x))
                         lines))
         (w (length (car map-lists)))
         (h (length map-lists)))
    (make-map map-lists w h)))

(define (map-get-value map x y) (list-ref (list-ref (map-data map) y) x))
(define (map-z map coord)
  (let ((val (map-get-value map (car coord) (cdr coord))))
    (cond ((eq? val #\S) 0)
          ((eq? val #\E) 25)
          (#t (- (char->integer val) (char->integer #\a))))))

(define (map-dist map c1 c2) 1)
(define (map-flat-coord map x y) (+ (* y (map-width map)) x))
(define (map-from-flat-coord map c) (cons (modulo c (map-width map))
                                          (quotient c (map-width map))))

(define (map-neighbours heightmap c)
  (let* ((coord (map-from-flat-coord heightmap c))
         (val (map-z heightmap coord))
         (w (map-width heightmap))
         (h (map-height heightmap))
         (x (car coord))
         (y (cdr coord))
         (candidates (list (cons (+ x 1) y) (cons (- x 1) y) (cons x (+ y 1)) (cons x (- y 1)))))
    (map (lambda (c) (map-flat-coord heightmap (car c) (cdr c)))
         (filter (lambda (candidate) (let* ((c-x (car candidate))
                                            (c-y (cdr candidate)))
                                       (and (< c-x w) (>= c-x 0) (< c-y h) (>= c-y 0)
                                            (<= (map-z heightmap candidate) (+ val 1)))))
                 candidates))))

(define (map-neighbours-part2 heightmap c)
  (let* ((coord (map-from-flat-coord heightmap c))
         (val (map-z heightmap coord))
         (w (map-width heightmap))
         (h (map-height heightmap))
         (x (car coord))
         (y (cdr coord))
         (candidates (list (cons (+ x 1) y) (cons (- x 1) y) (cons x (+ y 1)) (cons x (- y 1)))))
    (map (lambda (c) (map-flat-coord heightmap (car c) (cdr c)))
         (filter (lambda (candidate) (let* ((c-x (car candidate))
                                            (c-y (cdr candidate)))
                                       (and (< c-x w) (>= c-x 0) (< c-y h) (>= c-y 0)
                                            (>= (map-z heightmap candidate) (- val 1)))))
                 candidates))))

(define (map-find map val)
  (let ((w (map-width map))
        (h (map-height map)))
    (let loop ((x 0)
               (y 0))
      (if (>= y h) (error "oops"))
      (let ((current-val (map-get-value map x y)))
        (if (eq? current-val val)
            (map-flat-coord map x y)
            (if (< x (- w 1))
                (loop (+ x 1) y)
                (loop 0 (+ y 1))))))))

(define (dijkstra count init dest? get-cost get-neighbours)
  (let* ((cost (make-vector count 999999))
         (prev (make-vector count 'undefined)))
    (define (get-val v coord) (vector-ref v coord))
    (define (set-val! v coord val) (vector-set! v coord val))
    (set-val! cost init 0)
    (let loop ((to-consider (iota count)))
      (if (null? to-consider)
          (list cost prev)
          (let* ((current-cost-pair (fold (lambda (x acc) (let ((xcost (get-val cost x)))
                                                            (if (< xcost (cdr acc))
                                                                (cons x xcost)
                                                                acc)))
                                          (cons 'invalid 999999)
                                          to-consider))
                 (current (car current-cost-pair))
                 (current-cost (cdr current-cost-pair)))
            (if (not (dest? current))
                (let ((new-to-consider (filter (lambda (x) (not (equal? x current))) to-consider)))
                  (let neighbour-loop ((neighbours (filter (lambda (x) (memq x new-to-consider))
                                                           (get-neighbours current))))
                    (if (not (null? neighbours))
                        (let* ((next (car neighbours))
                               (next-cost (+ current-cost (get-cost current next))))
                          (if (< next (get-val cost next))
                              (begin (set-val! cost next next-cost)
                                     (set-val! prev next current)))
                          (neighbour-loop (cdr neighbours)))))
                       (loop new-to-consider))
                (list current cost prev)))))))

(define (dijkstra-result result init)
  (let ((dest (list-ref result 0))
        (cost (list-ref result 1))
        (prev (list-ref result 2)))
   (let loop ((path (list dest))
              (current dest))
     (if (= current init)
         (cons path (vector-ref cost dest))
         (let ((prev (vector-ref prev current)))
           (loop (cons prev path)
                 prev))))))

(define (map-dijkstra map)
  (let* ((start (map-find map #\S))
         (dest (map-find map #\E))
         (w (map-width map))
         (h (map-height map)))
    (dijkstra-result (dijkstra (* w h) start (lambda (x) (= x dest))
                               (lambda (c1 c2) (map-dist map c1 c2))
                               (lambda (c) (map-neighbours map c)))
                     start)))

(define (map-rev-dijkstra map)
  (let* ((start (map-find map #\E))
         (w (map-width map))
         (h (map-height map))
         (dest? (lambda (i) (let* ((coord (map-from-flat-coord map i))
                                   (val (map-get-value map (car coord) (cdr coord))))
                              (or (eq? val #\a) (eq? val #\S))))))
    (dijkstra-result (dijkstra (* w h) start dest?
                               (lambda (c1 c2) (map-dist map c1 c2))
                               (lambda (c) (map-neighbours-part2 map c)))
                     start)))

(let* ((data (read-file-string "day12.input"))
       (map (parse-map data)))
  (let ((part1-result (map-dijkstra map)))
    (print (string-append "part1: " (number->string (cdr part1-result)) "\n")))
  (let ((part2-result (map-rev-dijkstra map)))
    (print (string-append "part2: " (number->string (cdr part2-result)) "\n"))))
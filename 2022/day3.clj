(use 'clojure.java.io)

(defn find-common [sack]
  (let [size (/ (count sack) 2)
        bag1 (.substring sack 0 size)
        bag2 (.substring sack size)]
    (loop [bag bag1
           common nil]
      (if (or common (empty? bag))
        common
        (let [obj (first bag)]
          (recur (rest bag) (if (some (fn [x] (= x obj)) bag2)
                              obj
                              nil)))))))

(defn get-priority [c]
  (let [num-c (int c)]
   (if (< num-c (int \a))
     (+ 27 (- num-c (int \A)))
     (+ 1 (- num-c (int \a))))))

(defn find-common-badge [elf1 elf2 elf3]
  (loop [bag elf1
         common nil]
      (if (or common (empty? bag))
        common
        (let [obj (first bag)]
          (recur (rest bag) (if (and (some (fn [x] (= x obj)) elf2)
                                     (some (fn [x] (= x obj)) elf3))
                              obj
                              nil))))))

(with-open [rdr (reader "day3.input")]
  (let [all-lines (line-seq rdr)]
    (loop [lines all-lines
           priorities 0]
      (if (empty? lines)
        (println "part1: " priorities)
        (let [line-common-priority (get-priority (find-common (first lines)))]
          (recur (rest lines) (+ line-common-priority priorities)))))
    (loop [lines all-lines
           priorities 0]
      (if (empty? lines)
        (println "part2: " priorities)
        (let [line-common-priority (get-priority (find-common-badge (first lines) (second lines) (nth lines 2)))]
          (recur (nthnext lines 3) (+ line-common-priority priorities)))))))

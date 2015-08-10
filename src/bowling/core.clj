(ns bowling.core)

(def game [[1 4]
           [4 5]
           [6 4]
           [5 5]
           [10]
           [0 1]
           [7 3]
           [6 4]
           [10]
           [2 8 6]])

(def full-frame 10)

(defn true-score [frame]
  (reduce + (take 2 frame)))

(defn strike? [frame]
  (= 1 (count frame)))

(defn spare? [frame]
  (and (= full-frame (true-score frame)) (not (strike? frame))))

(defn strike-bonus [[next second & _]]
  (if-not (strike? next)
    (true-score next)
    (+ full-frame (first second))))

(defn bonus [current rest]
  (cond (strike? current) (strike-bonus rest)
        (spare? current) (first (first rest))
        :else 0))

(defn score-frames [[current & rest]]
  (let [frame-score (reduce + current)]
    (if-not rest
      (list frame-score)
      (cons (+ frame-score (bonus current rest)) (score-frames rest)))))

(reductions + (score-frames game))
;; => (5 14 29 49 60 61 77 97 117 133)

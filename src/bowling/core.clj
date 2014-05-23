(ns bowling.core)

(defn empty-slot? [v]
  (< -1 v))

(defn strike? [frame]
  (= 10 (first frame)))

(defn new-frame? 
  "Should we update the current frame, or should we create a new one?"
  [frame]
  (let [s (second frame)]
    (or (strike? frame) (empty-slot? s))))

(defn roll 
  "If frames is empty, we add a new frame,
   otherwise we either update the first frame or create
   a new one, returns the updated frames("
  [frames pins]
  (if (empty? frames)
    (conj frames [pins -1])
    (let [frame (first frames)
          f (first frame)]
      (if (new-frame? frame)
        (conj frames [pins -1])
        (cons [f pins] (rest frames))))))

(defn frame-score
  "Give the score of a frame without bonus"
  [frame]
  (reduce + (filter empty-slot? frame)))

(defn frame-total [frame bonus]
  "Calculates the total of a frame incl. bonus"
  (let [score (frame-score frame)]
   (cond
     (< score 10) score 
     (strike? frame) (+ 10 (reduce + bonus))
     :else  (+ 10 (first bonus)))))

(defn next-two
  "Gets the next two rolls from the list of frames"
  [frames]
  (when (seq frames)
    (->> frames
         (flatten)
         (filter empty-slot?)
         (take 2))))

(defn score
  "Calculates the score of a game, starting from the back, having the possible bonus rolls in bonus,
   will call itself recursivly until the first
   frame start by calling (score frames '(0 0))
   If not called with bonus, it will calculate its own"
  ([frames]
     (if (= 11 (count frames))
       (score (rest frames) (first frames))
       (score frames [0 0])))
  ([frames bonus]
     (if (seq frames)
       (let [current (first frames)
             past (score (rest frames) (next-two frames))]
         (+ past (frame-total current bonus)))
       0)))
  
(comment (-> (-> '() 
         (roll 9)
         (roll 0)
         (roll 3)
         (roll 7)			    
         (roll 6)
         (roll 1)
         (roll 3)			    
         (roll 7)
         (roll 8)
         (roll 1)
         (roll 5)
         (roll 5)
         (roll 0)
         (roll 10)
         (roll 8)
         (roll 0)
         (roll 7)
         (roll 3)
         (roll 8)
         (roll 2)
         (roll 8))
         (score )))
;=> 131

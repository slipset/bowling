(ns bowling.core)

(defn rolled? [v]
  (not (= -1 v)))

(defn strike? [frame]
  (= 10 (first frame)))

(defn new-frame? 
  "Should we update the current frame, or should we create a new one?"
  [frame]
    (or (strike? frame) (rolled? (second frame))))

(defn roll 
  "If frames is empty, we add a new frame,
   otherwise we either update the first frame or create
   a new one, returns the updated frames"
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
  (->> frame
       (filter rolled?)
       (reduce +)))

(defn frame-total [frame bonus]
  "Calculates the total of a frame incl. bonus"
  (let [score (frame-score frame)]
   (cond
     (< score 10) score 
     (strike? frame) (apply + 10 bonus)
     :else  (+ 10 (first bonus)))))

(defn bonus-rolls
  "Gets the next two rolls from the list of frames"
  [frames]
  (when (seq frames)
    (->> frames
         (flatten)
         (filter rolled?)
         (take 2))))

(defn score
  "Calculates the score of a game, starting from the back, having the possible bonus rolls in bonus,
   will call itself recursivly until the first
   frame start by calling (score frames '(0 0))
   If not called with bonus, it will calculate its own"
  ([frames]
     (let [frame-count (count frames)]
       (cond (= 11 frame-count) (score (rest frames) (bonus-rolls frames))
             (= 12 frame-count) (score (rest (rest frames)) (bonus-rolls frames))
             :else  (score frames [0 0]))))
  ([frames bonus]
     (if (seq frames)
         (+ (frame-total (first frames) bonus)
            (score (rest frames) (bonus-rolls frames)))
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

;; Tetris game logic as a SCI script. Pure Clojure, no host interop: it keeps
;; state in an atom and exposes move!/rotate!/tick!/board-data. The Flutter host
;; evals this once, then calls those fns and renders (board-data) to widgets.

(def width 10)
(def height 20)

;; tetrominoes: cells as [row col] in a 4x4 box, plus a color keyword
(def shapes
  {:I {:cells #{[1 0] [1 1] [1 2] [1 3]} :color :cyan}
   :O {:cells #{[0 1] [0 2] [1 1] [1 2]} :color :yellow}
   :T {:cells #{[0 1] [1 0] [1 1] [1 2]} :color :purple}
   :S {:cells #{[0 1] [0 2] [1 0] [1 1]} :color :green}
   :Z {:cells #{[0 0] [0 1] [1 1] [1 2]} :color :red}
   :J {:cells #{[0 0] [1 0] [1 1] [1 2]} :color :blue}
   :L {:cells #{[0 2] [1 0] [1 1] [1 2]} :color :orange}})

(def piece-keys (vec (keys shapes)))

(defn empty-board [] (vec (repeat height (vec (repeat width nil)))))

(def state (atom nil))

;; rotate a cell 90deg clockwise within the 4x4 box: [r c] -> [c (3-r)]
(defn rotate-cells [cells]
  (set (map (fn [[r c]] [c (- 3 r)]) cells)))

(defn piece-cells
  "shape rotated `rot` times, its 4x4 box placed at [px py] on the board;
   returns absolute [row col] board coords"
  [shape rot px py]
  (let [cells (nth (iterate rotate-cells (:cells shape)) (mod rot 4))]
    (map (fn [[r c]] [(+ py r) (+ px c)]) cells)))

(defn valid?
  "true when every cell of the piece is on-board and not colliding"
  [board cells]
  (every? (fn [[r c]]
            (and (>= c 0) (< c width) (< r height)
                 (or (< r 0) (nil? (get-in board [r c])))))
          cells))

(defn spawn
  "put a new piece at the top; sets :over when it can't be placed"
  [st]
  (let [k (rand-nth piece-keys)
        shape (shapes k)
        px 3 py 0 rot 0
        cells (piece-cells shape rot px py)]
    (assoc st :shape shape :rot rot :px px :py py
           :over (not (valid? (:board st) cells)))))

(defn new-game []
  (spawn {:board (empty-board) :score 0 :over false}))

(defn init! [] (reset! state (new-game)))

(defn cur-cells [st]
  (piece-cells (:shape st) (:rot st) (:px st) (:py st)))

(defn merge-piece
  "freeze the current piece onto the board"
  [st]
  (reduce (fn [b [r c]]
            (if (>= r 0) (assoc-in b [r c] (:color (:shape st))) b))
          (:board st) (cur-cells st)))

(defn clear-lines
  "remove full rows, return [board cleared-count]"
  [board]
  (let [kept (vec (remove (fn [row] (every? some? row)) board))
        cleared (- height (count kept))
        pad (vec (repeat cleared (vec (repeat width nil))))]
    [(into pad kept) cleared]))

(defn lock-and-next
  "freeze piece, clear lines, score, spawn next"
  [st]
  (let [merged (merge-piece st)
        [board cleared] (clear-lines merged)]
    (spawn (assoc st :board board :score (+ (:score st) (* cleared cleared 100))))))

(defn try-move [st dx dy drot]
  (let [rot (+ (:rot st) drot)
        px (+ (:px st) dx)
        py (+ (:py st) dy)
        cells (piece-cells (:shape st) rot px py)]
    (when (valid? (:board st) cells)
      (assoc st :px px :py py :rot rot))))

;; public API the host calls

(defn move! [dx]
  (when-not (:over @state)
    (swap! state (fn [st] (or (try-move st dx 0 0) st)))))

(defn rotate! []
  (when-not (:over @state)
    (swap! state (fn [st] (or (try-move st 0 0 1) st)))))

(defn tick!
  "gravity: drop one row, or lock and spawn next if it can't drop"
  []
  (when-not (:over @state)
    (swap! state (fn [st] (or (try-move st 0 1 0) (lock-and-next st))))))

(defn drop! []
  (when-not (:over @state)
    (swap! state (fn [st]
                   (loop [s st]
                     (if-let [s2 (try-move s 0 1 0)] (recur s2) (lock-and-next s)))))))

(defn over? [] (:over @state))
(defn score [] (:score @state))

(defn board-data
  "board as a width*height vector of vectors of color keywords (nil = empty),
   with the current falling piece overlaid"
  []
  (let [st @state
        overlay (into {} (for [[r c] (cur-cells st) :when (>= r 0)]
                           [[r c] (:color (:shape st))]))]
    (vec (for [r (range height)]
           (vec (for [c (range width)]
                  (or (overlay [r c]) (get-in (:board st) [r c]))))))))

;; UI: the script builds the whole widget tree via the host-provided flutter/*
;; fns (registered by app.cljd). This is SCI driving Flutter.

(defn render []
  (let [st @state]
    (flutter/column
     [(flutter/text (str "score " (:score st)))
      (flutter/gap 8)
      (flutter/column
       (for [row (board-data)]
         (flutter/row (for [c row] (flutter/cell c)))))
      (flutter/gap 12)
      (if (:over st)
        (flutter/text "GAME OVER")
        (flutter/row
         [(flutter/button "<"   (fn [] (move! -1)))
          (flutter/button "rot" (fn [] (rotate!)))
          (flutter/button ">"   (fn [] (move! 1)))
          (flutter/button "v"   (fn [] (drop!)))]))])))

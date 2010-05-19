(def direction-delta
  {:east  [ 1  0]
   :west  [-1  0]
   :south [ 0  1]
   :north [ 0 -1]})

(direction-delta :south)

(defn apply-delta
  "Returns an the result of moving the given
  xy coordinate one step in the given direction.
  The direction must be :north, :south, :east, or :west"
  [xy direction]
  (let [[x y] xy
        [dx dy] (direction-delta direction)]
    [(+ x dx) (+ y dy)]))

(apply-delta [5 5] :south)

(apply-delta [5 5] :east)

(defn find-alternatives
  "Given a direction, returns the two
  perpendicular directions in random order."
  [direction]
  (let [flip? (zero? (rand-int 2))]
    (if (#{:east :west} direction)
      (if flip? [:north :south] [:south :north])
      (if flip? [:east :west] [:west :east]))))

(find-alternatives :east)

(defn outside?
  "Returns true if either x or y are outside
  a square of the given size, otherwise false."
  [size [x y]]
  (not (and (< -1 x size) (< -1 y size))))

(outside? 10 [5 5])

(outside? 10 [20 5])

(outside? 10 [5 10])

(outside? 10 [5 9])

(defn make-courtyard
  "Create a square courtyard of refs,
   with 'size' rows and 'size' columns."
  [size]
  (vec (for [x (range size)]
         (vec (for [y (range size)]
                (ref nil))))))

(make-courtyard 5)

(def my-courtyard (make-courtyard 5))
(dosync (ref-set (get-in my-courtyard [1 1]) true))

my-courtyard

(defn occupied?
  "Checks the courtyard at coordinate xy,
  returning nil if it's empty or something
  truthy if it's occupied."
  [courtyard xy]
  @(get-in courtyard xy))

(occupied? my-courtyard [1 1])

(occupied? my-courtyard [0 0])

(defn clear-tile
  "Clears the courtyard at the given coordinate."
  [courtyard xy]
  (dosync
    (ref-set (get-in courtyard xy) nil)))

(clear-tile my-courtyard [1 1])

my-courtyard

; A pedestrian object might look like:
; {:xy [0 50], :forward :east, :color java.awt.Color/RED}

(defn one-step
  "Advances pedestrian a single step onto an
  unoccupied tile, either forward or to its
  left or right.  Updates the courtyard appropriately."
  [pedestrian courtyard]
  (let [{:keys [xy forward]} pedestrian
        size (count courtyard)
        [alt1 alt2] (find-alternatives forward)
        [forward-xy & alt-xys] (map #(apply-delta xy %) [forward alt1 alt2])]
    (if (outside? size forward-xy)
      (clear-tile courtyard xy)
      (let [legal-xys (cons forward-xy (remove #(outside? size %) alt-xys))]
        (dosync
          (if-let [target (first (remove #(occupied? courtyard %) legal-xys))]
            (do
              (clear-tile courtyard xy)
              (ref-set (get-in courtyard target) pedestrian)
              (assoc pedestrian :xy target))
            pedestrian)))))) ; fully blocked, try again later.

(def pedestrian-throttle 25)

(defn walk-across
  "Agent action to advance a pedestrian
  agent until it exits the courtyard"
  [pedestrian courtyard]
  (Thread/sleep pedestrian-throttle)
  (when-let [future-pedestrian (one-step pedestrian courtyard)]
    (send *agent* walk-across courtyard)
    future-pedestrian))

(def empty-color java.awt.Color/WHITE)

(defn draw-frame
  "Render the courtyard onto the given Graphics2D surface"
  [^java.awt.Graphics2D gfx courtyard]
  (let [size (count courtyard)
        snapshot (dosync (into [] (for [col courtyard]
                                    (into [] (for [tile col]
                                               @tile)))))]
    (doseq [[x col] (map-indexed vector snapshot)
            [y pedestrian] (map-indexed vector col)]
      (.setColor gfx (if pedestrian (:color pedestrian) empty-color))
      (.fillRect gfx (+ 5 (* x 3)) (+ 25 (* y 3)) 3 3))))

(defn keep-rendering
  "Agent action to repeatedly render the
  courtyard into the given double-buffer."
  [^java.awt.image.BufferStrategy dblbuf courtyard]
  (when dblbuf
    (let [gfx (.getDrawGraphics dblbuf)]
      (draw-frame gfx courtyard)
      (.show dblbuf)
      (Thread/sleep 30)
      (send-off *agent* #'keep-rendering courtyard)
      dblbuf)))

; Set up a courtyard:
(def my-courtyard (make-courtyard 100))

; Set up a double-buffering window
(def frame (java.awt.Frame.))
(.setVisible frame true)
(.createBufferStrategy frame 2)
(def render-agent (agent (.getBufferStrategy frame)))

; Start the render-agent
(send-off render-agent keep-rendering my-courtyard)

; Stop the render-agent
;(send-off render-agent (constantly nil))

; Create pedestrian agents walking toward each other:
(def albert (agent {:xy [0 50], :forward :east, :color java.awt.Color/BLACK}))
(send albert walk-across my-courtyard)

(def betty (agent {:xy [99 50], :forward :west, :color java.awt.Color/RED}))
(send betty walk-across my-courtyard)



(defn make-random-pedestrian
  "Create a pedestrian object with a random color
  and position on the perimiter of a courtyard of
  the given size.  The pedestrian's forward
  direction will toward the far side of the
  courtyard."
  [size]
  (let [forward (rand-nth [:north :south :east :west])
        x (case forward :east 0 :west (dec size) (rand-int size))
        y (case forward :south 0 :north (dec size) (rand-int size))]
    {:xy [x y]
     :forward forward
     :color (java.awt.Color. (rand-int 200) (rand-int 200) (rand-int 200))}))

; Create a wall:
(dosync
  (doseq [x (range 25 75)]
    (ref-set (get-in my-courtyard [x 30]) {:color java.awt.Color/BLACK})))

(def pedestrian-throttle 1)

; Send a large batch of random pedestrians
(dotimes [_ 500]
  (send (agent (make-random-pedestrian (count my-courtyard)))
        walk-across my-courtyard))













(comment
(def add-peds (atom true))

(future
  (while @add-peds
    (let [pedestrian-count (dosync
                            (count
                              (for [col my-courtyard tile col :when @tile] 1)))]
      (when (< pedestrian-count 1000)
        (send (agent (make-random-pedestrian (count my-courtyard))) walk-across my-courtyard)))))

(reset! add-peds false)
)

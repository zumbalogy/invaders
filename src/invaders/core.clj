(ns invaders.core
  (require [lanterna.terminal :as t]
           [lanterna.constants :as c]))

(def X (atom 0))
(def Y (atom 0))

(def VX (atom 0))
(def VY (atom 0))

(def shoot (atom 0))

(def frame (atom 0))

(def old-stamp (atom 0))
(def machine-on (atom true))

(defn shutdown [term]
  (t/clear term)
  (t/put-string term "good bye")
  ; (Thread/sleep 900)
  (reset! machine-on false))

(defn handle-vx [vd d]
  (cond
    (pos? @vd)
    (do
      (reset! d (min 75 (inc @d)))
      (reset! vd (max 0 (- @vd 3))))
    (neg? @vd)
    (do
     (reset! d (max 0 (dec @d)))
     (reset! vd (min 0 (+ @vd 3)))))
  @d)

(defn handle-vy [vd d]
  (cond
    (pos? @vd)
    (do
      (reset! d (min 75 (inc @d)))
      (reset! vd (max 0 (- @vd 3))))
    (neg? @vd)
    (do
     (reset! d (max 0 (dec @d)))
     (reset! vd (min 0 (+ @vd 3)))))
  @d)

(defn status []
  (format "frame:%2d xy:[%2d %2d]Vxy:[%3d %3d]time:%d" @frame @X @Y @VX @VY @old-stamp))

(defn draw [term]
  (t/clear term)
  (let [new-x (handle-vx VX X)
        new-y (handle-vy VY Y)]
    (t/put-string term "⋰⋱" new-x new-y)
    (when (< 0 @shoot)
      (let [bullet "^"]
        (case @shoot
          3 (t/set-fg-color term :red)
          2 (t/set-fg-color term :red)
          nil)
        (doseq [y (range new-y)]
          (t/put-string term bullet (inc new-x) y))
        (swap! shoot dec)))
    (t/set-fg-color term :white)
    (t/put-string term (status) 0 0)
    (t/move-cursor term 0 0)))

(defn delta-v [d n]
  (reset! d (min 100 (max -100 (+ @d n)))))

(defn tick-frame []
  (let [new-stamp (System/currentTimeMillis)
        diff (- new-stamp @old-stamp)
        pause (- 50 diff)]
    (swap! frame inc)
    (when (= 12 @frame)
      (reset! frame 0))
    (when (pos? pause)
      (Thread/sleep pause))
    (reset! old-stamp (System/currentTimeMillis))))

(defn -main []
  (let [term (t/get-terminal :unix) ; :unix :text :swing :auto :cygwin
        max-w (- 80 5)
        max-h 40]
    (t/start term)
    (t/clear term)
    (t/put-string term "press escape to exit.")
    (t/put-string term "type away ")
    (t/move-cursor term 0 0)
    (while @machine-on
      (let [key (t/get-key term)]
        (case key
          nil nil
          :up (delta-v VY -10)
          \w (delta-v VY -10)
          :left (delta-v VX -16)
          \a (delta-v VX -16)
          :down (delta-v VY 10)
          \s (delta-v VY 10)
          :right (delta-v VX 16)
          \d (delta-v VX 16)
          :escape (shutdown term)
          \space (reset! shoot 5)
          nil)
        (draw term)
        (tick-frame)))
    (t/stop term)))

(ns invaders.core
  (require [lanterna.terminal :as t]
           [lanterna.constants :as c]))

(def X (atom 0))
(def Y (atom 0))

(def VX (atom 0))
(def VY (atom 0))

(def shoot (atom 0))

(def old-stamp (atom 0))
(def machine-on (atom true))

(defn shutdown [term]
  (t/clear term)
  (t/put-string term "good bye")
  (Thread/sleep 900)
  (reset! machine-on false))

(defn handle-v [vd d]
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

(defn draw [term]
  (t/clear term)
  (let [new-x (handle-v VX X)
        new-y (handle-v VY Y)]
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
    (t/move-cursor term 0 0)
    (let [new-stamp (System/currentTimeMillis)
          diff (- new-stamp @old-stamp)
          pause (- 50 diff)]
      (when (pos? pause)
        (Thread/sleep pause))
      (reset! old-stamp new-stamp))))

(defn delta-v [d n]
  (reset! d (min 100 (max -100 (+ @d n)))))

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
          ; :tab (t/clear term)
          ; :left (reset! X (max 0 (dec @X)))
          ; :right (reset! X (min max-w (inc @X)))
          ; :up (reset! Y (max 0 (dec @Y)))
          ; :down (reset! Y (min max-h (inc @Y)))
          ; :left (reset! VX (max -100 (- @VX 10)))
          ; :right (reset! VX (min 100 (+ @VX 10)))
          ; :up (reset! VY (max -100 (- @VY 10)))
          ; :down (reset! VY (min 100 (+ @VY 10)))
          :up (delta-v VY -5)
          \w (delta-v VY -5)
          :left (delta-v VX -10)
          \a (delta-v VX -10)
          :down (delta-v VY 5)
          \s (delta-v VY 5)
          :right (delta-v VX 10)
          \d (delta-v VX 10)
          ; :enter (reset! Y (min max-w (inc @Y)))
          :escape (shutdown term)
          \space (reset! shoot 5)
          nil)
          ; (do
          ;   (t/put-string term (str key))
          ;   (reset! X (min max-w (inc @X)))))
        (draw term)))
    (t/stop term)))

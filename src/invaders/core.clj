(ns invaders.core
  (require [lanterna.screen :as s]
           [lanterna.constants :as c]
           [invaders.keys :as keys]))

(def X (atom 24))
(def Y (atom 24))

(def VX (atom 0))
(def VY (atom 0))

(def shots (atom []))
(def weapon-cooldown-stamp (atom 0))

(def enemy-shots (atom []))
; (def targets (atom (for [x (range 2 14 2) y (range 4 10 2)] {:s "o" :d "l" :live true :x x :y y})))
(def targets (atom (for [x (range 2 14) y (range 8 10 2)] {:s "o" :d "l" :live true :x x :y y})))
; (def targets (atom [{:s "o" :d "l" :live true :x 10 :y 10}]))

(def frame (atom 0))

(def old-stamp (atom 0))
(def machine-on (atom true))

(defn shutdown [term]
  (s/clear term)
  (s/put-string term 0 0 "good bye")
  ; (Thread/sleep 900)
  (reset! machine-on false))

(defn handle-v [vd d]
  (cond
    (pos? @vd)
    (do
      (reset! d (min 75 (inc @d)))
      ; (reset! vd (max 0 (- @vd 3)))
      (reset! vd 0))
    (neg? @vd)
    (do
     (reset! d (max  0 (dec @d)))
    ;  (reset! vd (min 0 (+ @vd 3)))
     (reset! vd 0)))
  @d)

(defn status []
  (format "frame:%2d xy:[%2d %2d]Vxy:[%3d %3d]" @frame @X @Y @VX @VY))

(defn log [& texts]
  (spit "core.log" (str "\n" texts) :append true)
  (first texts))

(defn loose [term]
  (s/clear term)
  (s/put-string term 0 0 "You loose!")
  (s/redraw term)
  (Thread/sleep 1000)
  (reset! machine-on false))

(defn draw-ship [term]
  (let [new-x (handle-v VX X)
        new-y (handle-v VY Y)]
    (s/put-string term new-x new-y "^")))

(defn collide [a b]
  (let [x1 (:x a) y1 (:y a)
        x2 (:x b) y2 (:y b)]
    (and (= x1 x2) (= y1 y2))))

(defn collide-any [a bs]
  (some #(collide a %) bs))

(defn inbounds [obj]
  (and (<= 0 (:y obj) 50)
       (<= 0 (:x obj) 70)))

(defn draw-shots [term]
  (doseq [n @shots]
    (let [bullet ":"
          x (:x n)
          y (:y n)]
      (s/put-string term x y bullet {:fg :green})))
  (let [t @targets
        e @enemy-shots]
    (->> @shots
      (map #(assoc % :y (dec (:y %))))
      (filter inbounds)
      (reset! shots))))

(defn draw-enemy-shots [term]
  (doseq [n @enemy-shots]
    (let [bullet "*"
          x (:x n)
          y (:y n)]
      (s/put-string term x y bullet {:fg :red})
      (when (and (= y @Y) (> 1 (Math/abs (- x @X))))
        (loose term))))
  (let [s @shots]
    (->> @enemy-shots
      (map #(assoc % :y (inc (:y %))))
      (filter inbounds)
      (reset! enemy-shots))))

(defn move-target [t]
  (let [x (:x t)
        d (:d t)
        y (:y t)
        new-x (if (= "l" d) (inc x) (dec x))
        new-d (cond (< x 2) "l" (< 70 x) "r" :else d)
        new-y (if (= d new-d) y (inc y))
        new-l (and (:live t) (not (collide-any t @shots)))]
    (assoc t :x new-x :y new-y :d new-d :live new-l)))
    ; (assoc t :x x :y y :d new-d :live new-l)))
    ; (assoc t :x x :y y :d new-d)))

(defn draw-target [term]
  (reset! targets (map move-target @targets))
  (doseq [t @targets]
    (let [x (:x t)
          y (:y t)]
      (s/put-string term x y (:s t))
      (when (< 0.99 (rand))
        (swap! enemy-shots conj {:live true :x x :y (inc y)})))))

(defn check-win [term]
  (when (empty? @targets)
    (s/clear term)
    (s/put-string term 0 0 "You win!")
    (s/redraw term)
    (Thread/sleep 1000)
    (reset! machine-on false))
  (when (< 30 (apply max (conj (map :y @targets) 0)))
    (loose term)))

(defn clean-debris [term]
  (reset! targets (filter :live @targets))
  (reset! shots (filter :live @shots))
  (reset! enemy-shots (filter :live @enemy-shots)))

(defn draw [term]
  (s/clear term)
  (draw-ship term)
  (draw-shots term)
  (draw-enemy-shots term)
  (draw-target term)
  (clean-debris term)
  (check-win term)
  (s/put-string term 0 0 (status))
  (s/redraw term))

(defn delta-v [d n]
  (reset! d (min 100 (max -100 (+ @d n)))))

(defn tick-frame []
  (let [new-stamp (System/currentTimeMillis)
        diff (- new-stamp @old-stamp)
        pause (- 60 diff)]
    (swap! frame inc)
    (when (= 12 @frame)
      (reset! frame 0))
    (when (pos? pause)
      (Thread/sleep pause))
    (reset! old-stamp (System/currentTimeMillis))))

(defn fire-weapon []
  (let [new-stamp (System/currentTimeMillis)]
    (when (< 80 (- new-stamp @weapon-cooldown-stamp))
      (reset! weapon-cooldown-stamp (System/currentTimeMillis))
      (swap! shots conj {:live true :x @X :y (dec @Y)}))))

(defn -main []
  (let [term (s/get-screen :unix) ; :unix :text :swing :auto :cygwin
        size (s/get-size term)
        max-w (- 80 5)
        max-h 40]
    (s/start term)
    (s/move-cursor term 0 0)
    (future (doseq [ln (line-seq (java.io.BufferedReader. *in*))]
      (println (reverse ln))))
    (while @machine-on
      ; (Thread/sleep 100)
      ; (println "this is the core")
      ; (println @keys/keyboard)
      (let [key (s/get-key term)]
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
          \space (fire-weapon)
          nil)
        (draw term)
        (tick-frame)))
    (s/stop term)))

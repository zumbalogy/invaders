(ns invaders.core
  (require [lanterna.terminal :as t]
           [lanterna.screen :as s]
           [lanterna.constants :as c]))

(def X (atom 0))
(def Y (atom 0))

(def VX (atom 0))
(def VY (atom 0))

(def shots (atom []))
(def weapon-cooldown-stamp (atom 0))

(def targets (atom [{:s "o" :x 10 :y 5}
                    {:s "o" :x 11 :y 5}
                    {:s "o" :x 12 :y 5}]))

(def frame (atom 0))

(def old-stamp (atom 0))
(def machine-on (atom true))

(defn shutdown [term]
  (t/clear term)
  (t/put-string term "good bye")
  ; (Thread/sleep 900)
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

(defn status []
  (format "frame:%2d xy:[%2d %2d]Vxy:[%3d %3d]time:%d" @frame @X @Y @VX @VY @old-stamp))

(defn log [& texts]
  (spit "core.log" (str "\n" texts) :append true))

(defn draw-ship [term]
  (let [new-x (handle-v VX X)
        new-y (handle-v VY Y)]
    (t/put-string term "⋰⋱" new-x new-y)))

(defn collide [a b]
  (= (select-keys a [:x :y])
     (select-keys b [:x :y])))

(defn collide-any [a bs]
  (some #(collide a %) bs))

(defn inbounds [obj]
  (and (<= 0 (:y obj) 50)
       (<= 0 (:x obj) 70)))

(defn draw-shots [term]
  (doseq [n @shots]
    (let [bullet "^"
          x (:x n)
          y (:y n)]
      (t/put-string term bullet x y)))
  (let [foo @targets]
    (reset! shots
      (filter inbounds
        (map #(update % :y dec)
          (filter #(not (collide-any % foo))
            @shots))))))

(defn draw-target [term]
  (reset! targets
    (map
      (fn [t]
        (if (collide-any t @shots)
          (assoc t :s "x")
          t))
      @targets))
  (doseq [t @targets]
    (t/put-string term (:s t) (:x t) (:y t))))

(defn draw [term]
  (t/clear term)
  (draw-ship term)
  (draw-shots term)
  (draw-target term)
  (t/set-fg-color term :white)
  (t/put-string term (status) 0 0)
  (t/move-cursor term 0 0))

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

(defn fire-weapon []
  (let [new-stamp (System/currentTimeMillis)]
    ; (when (< 800 (- new-stamp @weapon-cooldown-stamp)))
    (when (< 80 (- new-stamp @weapon-cooldown-stamp))
      (reset! weapon-cooldown-stamp (System/currentTimeMillis))
      (swap! shots conj {:x (inc @X) :y (dec @Y)}))))

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
          \space (fire-weapon)
          nil)
        (draw term)
        (tick-frame)))
    (t/stop term)))

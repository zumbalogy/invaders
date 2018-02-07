(ns invaders.core
  (require [lanterna.screen :as s]
           [lanterna.constants :as c]
           [invaders.keys :as keys]))

(def game-state
  (atom {:x 24
         :y 24
         :d 0
         :vx 0
         :vy 0
         :shots []
         :enemy-shots []
         :weapon-cooldown-stamp 0
         :targets (for [x (range 2 14) y (range 8 10 2)] {:s "o" :d "l" :live true :x x :y y})
         ;; :targets (for [x (range 2 14 2) y (range 4 10 2)] {:s "o" :d "l" :live true :x x :y y})
         :frame 0
         :old-stamp 0
         :machine-on true}))

(defn change-state [prop func & args]
  (let [old-prop (get @game-state prop)
        all-args (remove nil? (concat [old-prop] args))
        new-state (apply func all-args)]
    (swap! game-state assoc prop new-state)))

; make this take n pairs
(defn set-state [prop state]
  (swap! game-state assoc prop state))

(defn get-state [prop]
  (get @game-state prop))

(defn get-ms []
  (System/currentTimeMillis))

(defn shutdown [term]
  (s/clear term)
  (s/put-string term 0 0 "good bye")
  ; (Thread/sleep 900)
  (set-state :machine-on false))

(defn handle-v [vd d]
  (cond
    (pos? vd)
    [(min 75 (inc d)) 0]
    (neg? vd)
    [(max 0 (dec d)) 0]
    :else
    [0 0]))

(defn status []
  (format "frame:%2d xy:[%2d %2d]Vxy:[%3d %3d]"
          (get-state :frame)
          (get-state :x)
          (get-state :y)
          (get-state :vx)
          (get-state :vy)))

(defn log [& texts]
  (spit "core.log" (str "\n" texts) :append true)
  (first texts))

(defn loose [term]
  (s/clear term)
  (s/put-string term 0 0 "You loose!")
  (s/redraw term)
  (Thread/sleep 1000)
  (set-state :machine-on false))

(defn draw-ship [term]
  (let [[vx x] (handle-v (get-state :vx) (get-state :x))
        [vy y] (handle-v (get-state :vy) (get-state :y))]
    (set-state :vx vx)
    (set-state :vy vy)
    (set-state :x x)
    (set-state :y y)
    (s/put-string term x y "^")))

(defn collide [a b]
  (let [x1 (:x a) y1 (:y a)
        x2 (:x b) y2 (:y b)]
    (and (= x1 x2)
         (= y1 y2))))

(defn collide-any [a bs]
  (some #(collide a %) bs))

(defn inbounds [obj]
  (and (<= 0 (:y obj) 50)
       (<= 0 (:x obj) 70)))

(defn draw-shots [term]
  (doseq [n (get-state :shots)]
    (let [bullet ":"
          x (:x n)
          y (:y n)]
      (s/put-string term x y bullet {:fg :green})))
  (let [t (get-state :targets)
        e (get-state :enemy-shots)]
    (->> (get-state :shots)
      (map #(assoc % :y (dec (:y %))))
      (filter inbounds)
      (set-state :shots))))

(defn draw-enemy-shots [term]
  (doseq [n (get-state :enemy-shots)]
    (let [bullet "*"
          x (:x n)
          y (:y n)]
      (s/put-string term x y bullet {:fg :red})
      (when (and (= y (get-state :y)) (> 1 (Math/abs (- x (get-state :x)))))
        (loose term))))
  (let [s (get-state :shots)]
    (->> (get-state :enemy-shots)
      (map #(assoc % :y (inc (:y %))))
      (filter inbounds)
      (set-state :enemy-shots))))

(defn move-target [t]
  (let [x (:x t)
        d (:d t)
        y (:y t)
        new-x (if (= "l" d) (inc x) (dec x))
        new-d (cond (< x 2) "l" (< 70 x) "r" :else d)
        new-y (if (= d new-d) y (inc y))
        new-l (and (:live t) (not (collide-any t (get-state :shots))))]
    (assoc t :x new-x :y new-y :d new-d :live new-l)))
    ; (assoc t :x x :y y :d new-d :live new-l)))
    ; (assoc t :x x :y y :d new-d)))

(defn draw-target [term]
  (set-state :targets (map move-target (get-state :targets)))
  (doseq [t (get-state :targets)]
    (let [x (:x t)
          y (:y t)]
      (s/put-string term x y (:s t))
      (when (< 0.99 (rand))
        (change-state :enemy-shots conj {:live true :x x :y (inc y)})))))

(defn check-win [term]
  (when (empty? (get-state :targets))
    (s/clear term)
    (s/put-string term 0 0 "You win!")
    (s/redraw term)
    (Thread/sleep 1000)
    (set-state :machine-on false))
  (when (< 30 (apply max (conj (map :y (get-state :targets)) 0)))
    (loose term)))

(defn clean-debris [term]
  (set-state :targets (filter :live (get-state :targets)))
  (set-state :shots (filter :live (get-state :shots)))
  (set-state :enemy-shots (filter :live (get-state :enemy-shots))))

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
  (set-state :d (min 100 (max -100 (+ (get-state :d) n)))))

(defn tick-frame []
  (let [new-stamp (get-ms)
        diff (- new-stamp (get-state :old-stamp))
        pause (- 60 diff)]
    (change-state :frame inc)
    (when (= 12 (get-state :frame))
      (set-state :frame 0))
    (when (pos? pause)
      (Thread/sleep pause))
    (set-state :old-stamp (get-ms))))

(defn fire-weapon []
  (let [new-stamp (get-ms)]
    (when (< 80 (- new-stamp (get-state :weapon-cooldown-stamp)))
      (set-state :weapon-cooldown-stamp (get-ms))
      (change-state :shots conj {:live true
                                 :x (get-state :x)
                                 :y (dec (get-state :y))}))))

(defn -main []
  (let [term (s/get-screen :unix) ; :unix :text :swing :auto :cygwin
        size (s/get-size term)
        max-w (- 80 5)
        max-h 40]
    (s/start term)
    (s/move-cursor term 0 0)
    ;; (future (doseq [ln (line-seq (java.io.BufferedReader. *in*))]
      ;; (println (reverse ln))))
    (while (get-state :machine-on)
      ; (Thread/sleep 100)
      ; (println "this is the core")
      ; (println @keys/keyboard)
      (let [key (s/get-key term)
            vx (get-state :vx)
            vy (get-state :vy)]
        (case key
          nil nil
          :up (delta-v vy -10)
          \w (delta-v vy -10)
          :left (delta-v vx -16)
          \a (delta-v vx -16)
          :down (delta-v vy 10)
          \s (delta-v vy 10)
          :right (delta-v vx 16)
          \d (delta-v vx 16)
          :escape (shutdown term)
          \space (fire-weapon)
          nil)
        (draw term)
        (tick-frame)))
    (s/stop term)))

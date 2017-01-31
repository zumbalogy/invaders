(ns invaders.core
  (require [lanterna.terminal :as t]
           [lanterna.constants :as c]))

(def X (atom 0))
(def Y (atom 0))

(def shoot (atom 0))

(def machine-on (atom true))

(defn shutdown [term]
  (t/clear term)
  (t/put-string term "good bye")
  (Thread/sleep 900)
  (reset! machine-on false))

(defn draw [term]
  (t/clear term)
  (t/put-string term "-^-" @X @Y)
  (when (< 0 @shoot)
    (let [bullet "^"]
      (case @shoot
        3 (t/set-fg-color term :red)
        2 (t/set-fg-color term :red)
        nil)
      (doseq [y (range @Y)]
        (t/put-string term bullet (inc @X) y))
      (swap! shoot dec)))
  (t/set-fg-color term :white)
  (t/move-cursor term 0 0)
  (Thread/sleep 20))

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
          :left (reset! X (max 0 (dec @X)))
          :right (reset! X (min max-w (inc @X)))
          :up (reset! Y (max 0 (dec @Y)))
          :down (reset! Y (min max-h (inc @Y)))
          ; :enter (reset! Y (min max-w (inc @Y)))
          :escape (shutdown term)
          \space (reset! shoot 5)
          nil)
          ; (do
          ;   (t/put-string term (str key))
          ;   (reset! X (min max-w (inc @X)))))
        (draw term)))
    (t/stop term)))

(ns invaders.core
  (require [lanterna.screen :as s]
           [lanterna.terminal :as t]
           [lanterna.constants :as c]))

; (def scr (s/get-screen))
(def term (t/get-terminal :unix)) ; :unix :text :swing :auto :cygwin

(def cursor (atom [0 0]))

(def machine-on (atom true))

(defn -main []
  (t/start term)
  (t/put-string term "press escape to exit.")
  (t/put-string term "type away ")
  (while @machine-on
    (let [raw-key (t/get-key-blocking term)
          key (str raw-key)]
      (t/put-string term key)
      (when (= raw-key :escape)
        (reset! machine-on false))))
  (t/stop term))

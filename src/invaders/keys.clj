(ns invaders.keys)

; NOTE: sudo showkey -k

; (doseq [ln (line-seq (java.io.BufferedReader. *in*))] (println ln))

(def keyboard (atom {:up false :down false :right false :left false}))

; (future (reset! keyboard {:what 5}))

; (while true (do (Thread/sleep 1000) (println "hello")))))

(def stream (line-seq (java.io.BufferedReader. *in*)))




; (future
;   (while true
;     (Thread/sleep 100)
;     (println 100)))

    ; (reset! keyboard {:rand (count (read-line))})))
  ; (dorun [ln (line-seq (java.io.BufferedReader. *in*))]
  ; (doseq [ln (range 100)]
    ; (Thread/sleep 100)
    ; (reset! keyboard {:count (count ln)})))
;
; (future (while true (do (Thread/sleep 200) (println 9))))

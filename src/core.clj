(ns core
  (:use huffman))

(defn test-msg [freq msg]
  (let [hf (huffman-compile freq)
        data-enc (encode hf msg)
        data-dec (decode hf data-enc)]
    (println msg)
    (println (apply str data-enc))
    (println (apply str data-dec))
    (println "Compressed Size %: " (float (/ (count data-enc) (* 8 (count msg)))))))

(defn -main []
  (test-msg {\o 12 \h 1 \e 1 \l 2 }
            "helloooooooooooo")

  (let [msg "the great white fox jumped over the lazy dog"
        freq (apply merge (map #(identity {% (rand-int 10)}) msg))]
    (test-msg freq msg)))




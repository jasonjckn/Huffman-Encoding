(ns core
  (:use huffman))

(defn test-msg [freq msg]
  (let [hf (huffman-compile freq)
        data-enc (encode hf msg)
        data-dec (decode hf data-enc)]
    (println (apply str (repeat 80 \=)))
    (println "Input:  \t" msg)
    (println "Encoded:\t" (apply str data-enc))
    (println "Decoded:\t" (apply str data-dec))
    (println "Compression %:\t" (float (/ (count data-enc) (* 8 (count msg)))))))

(defn -main []
  (test-msg {\o 12 \h 1 \e 1 \l 2 }
            "helloooooooooooo")

  (let [msg "the great white fox jumped over the lazy dog"
        freq (apply merge (map #(identity {% (rand-int 10)}) msg))]
    (test-msg freq msg)))




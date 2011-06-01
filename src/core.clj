(ns core
  (:use [huffman]
        [util]
        [clojure.contrib.seq-utils :only [indexed]]
        [clojure.walk]))



(defn optimal-freq [msg]
  (->> msg
       (map #(hash-map % 1))
       (apply merge-with +)))

(defn test-msg [freq msg]
  (let [hf (huffman-compile freq)
        data-enc (encode hf msg)
        data-dec (decode hf data-enc)]
    (println "Input:  \t" msg)
    (println "Encoded:\t" (apply str data-enc))
    (println "Decoded:\t" (apply str data-dec))
    (println "Compression %:\t" (- 1.0 (float (/ (count data-enc) (* 8 (count msg))))))
    (println (apply str (repeat 80 \=)))))


(defn -main []
  (println ">>>> Simple Case:")
  (test-msg {\o 12 \h 1 \e 1 \l 2 }
            "helloooooooooooo")

  (println ">>>> Random Huffman Tree:")
  (let [msg "huffman assumes that your alphabet is small, and thus you'll reuse characters often"
        freq (apply merge (map #(identity {% (rand-int 10)}) msg))]
    (test-msg freq msg))

  (println ">>>> Optimal Huffman Tree:")
  (let [msg "huffman assumes that your alphabet is small, and thus you'll reuse characters often"
        freq (optimal-freq msg)]
    (test-msg freq msg))
  )




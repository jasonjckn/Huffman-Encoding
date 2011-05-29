(ns core
  (:use huffman))

(defn -main []
  (def freq {\o 12 \h 1 \e 1 \l 2 })
  (def hf (huffman-compile freq))

  (def msg (seq "hellooooooooooooooo"))
  (def data-enc (encode hf "hellooooooooooooooo"))
  (def data-dec (decode hf data-enc))

  (println msg)
  (println data-enc)
  (println data-dec))




(ns core
  (:use [util]
        [clojure.walk]
        [pallet thread-expr]))

(defnl iter [start]
  (->> start (sort-by :f) (merge-two))
  :where [merge-two (fn [[a b & cs]]
                      (cons {0 a, 1 b,
                             :f (+ (:f a) (:f b))} cs))])

(defnl decode-tree [freq]
  (loop [ir initial-irepr]
    (if (= (count ir) 1)
      (sanitize-all (first ir))
      (recur (iter ir))))
  :where
  [initial-irepr (for [[ch f] freq] {:ch ch :f f})
   sanitize-part (fn [v]
                   (-> v (when-> (map? v)
                                 (if-> (:ch v) (:ch) (dissoc :f)))))
   sanitize-all (fn [tree] (postwalk sanitize-part tree))])

(defnl encode-map [decode-tree]
  (->> decode-tree (leaf-seq) (map reverse) (map vec) (into {})))

(defnl huffman-compile [freq]
  {:encode (encode-map decode-tree$) :decode decode-tree$}
  :where [decode-tree$ (decode-tree freq)])

(defnl encode [hf data]
  (flatten (map (:encode hf) data)))

(defnl decode [hf data]
  (loop [decoded []
         [d1 & ds] data
         node root-node]
    (if d1
      (if (map? node)
        (recur decoded ds (node d1))
        (recur (conj decoded node) ds (root-node d1)))
      (conj decoded node)))
  :where [root-node (:decode-tree hf)])

#_ (def freq {\o 12 \h 1 \e 1 \l 2 })
#_ (def hf (huffman-compile freq))
#_ (def data (encode hf "hellooooooooooooooo"))

#_ (decode hf enc)
#_ (count data)




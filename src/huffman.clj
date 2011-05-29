(ns huffman
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
  {:encode-map (encode-map decode-tree$) :decode-tree decode-tree$}
  :where [decode-tree$ (decode-tree freq)])

(defnl encode [hf data]
  (flatten (map (:encode-map hf) data)))

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

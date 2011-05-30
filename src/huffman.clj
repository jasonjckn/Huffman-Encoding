(ns huffman
  (:use [util]
        [clojure.walk]
        [pallet thread-expr]))

(defnl iter [ir]
  (->> ir (sort-by (fn [[_ f]] f)) (merge-two))
  :where [merge-two (fn [[[av af] [bv bf] & cs]]
                      (cons [{0 av 1 bv} (+ af bf)] cs))])

(defnl decode-tree [freq]
  (loop [ir initial-irepr]
    (if (= (count ir) 1)
      (first (first ir))
      (recur (iter ir))))
  :where
  [initial-irepr (for [[ch f] freq] [ch f])])

(defnl encode-map [decode-tree]
  (->> decode-tree (leaf-seq) (map reverse) (map vec) (into {})))

(defnl huffman-compile [freq]
  {:encode-map (encode-map decode-tree$) :decode-tree decode-tree$}
  :where [decode-tree$ (decode-tree freq)])

(defnl encode [hf data]
  (mapcat (:encode-map hf) data))

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

(ns huffman
  (:use [util]
        [clojure.walk]
        [pallet thread-expr]))


(defnl decode-tree [freq]
  (->> freq (iterate merge-lowest-2) (take (count freq))
       (last) (first) (first))
  :where
  [merge-first-2 (fn [[[av af] [bv bf] & cs]]
                   (cons [{0 av 1 bv} (+ af bf)] cs))

   merge-lowest-2 (fnl [ir] (->> ir (sort-by (fn [[_ f]] f)) (merge-first-2)))])

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


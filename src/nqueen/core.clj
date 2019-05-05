(ns nqueen.core
  (:require [clojure.pprint]
            [clojure.tools.trace])
  (:gen-class))

(defn reflect-v [b]
  (reverse b))

(defn reflect-h [b]
  (map reverse b))

(defn rotate
  ([b] (apply map list b))
  ([b n] (cond (zero? n) b
               (= 1 n) (rotate b)
               :else (rotate (rotate  b) (mod (dec n) 4)))))

(defn board-eq? [a b]
  (let [r1 (reflect-h b)
        r2 (reflect-v b)
        r3 (reflect-v (reflect-h b))]
    (or (some (partial = a) (map (partial rotate b) (range 4)))
        (some (partial = a) (map (partial rotate r1) (range 4)))
        (some (partial = a) (map (partial rotate r2) (range 4)))
        (some (partial = a) (map (partial rotate r3) (range 4))))))

(defn distinct-boards
  ([s]
   (distinct-boards [] s))
  ([s1 [i & s2]]
   (cond
     (nil? i) s1
     (every? #(not (board-eq? % i)) s1) (distinct-boards (cons i s1) s2)
     :else (distinct-boards s1 s2))))

(defn possible-locations
  [n board]
  (filter some?
          (for [i (range  n)]
            (when (allowed? n i board)
              (let [col (concat (take i (repeat " ")) ["Q"] (take (- n i 1) (repeat " ")))]
                col)))))

(defn nqueens* [n board]
  (apply concat
         (for [i (possible-locations n board)]
           (let [new-board (concat board (list i))]
             (if (= n (count new-board))
               [new-board]
               (nqueens* n new-board))))))

(defn nqueens [n]
  (nqueens* n []))

(defn print-solution [s]
  (doseq        [r (range (count s))]
    (println "|" (clojure.string/join " " (nth s r)) "|")))

(defn print-all-solutions [s]
  (doseq        [i (range (count s))]
    (println "---------")
    (println "solution " (inc i) "of" (count s))
    (print-solution (nth s i))))

(defn -main
  [& args]
  (doseq [n (range 5 6 )]
    (println "================================")
    (print "n=" n)
    (let [s (nqueens n)]
      (println " has " (count s) "solutions," (count (distinct-boards s)) "distinct")
      (println "distinct solutions")
      (print-all-solutions (distinct-boards s))
    )))

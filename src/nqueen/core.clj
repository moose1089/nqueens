(ns nqueen.core
  (:require [clojure.pprint]
            [clojure.tools.trace])
  (:gen-class))

;; TODO add checks for rotation / reflection

(defn reflect [b]
  (map reverse b))

(defn rotate
  ([b] (apply map list b))
  ([b n] (cond (zero? n) b
               (= 1 n) (rotate b)
               :else (rotate (rotate  b) (mod (dec n) 4)))))

(defn board-eq? [a b]
  (or (= a b)
      (= a (reflect b))
      (= a (rotate b 1))
      (= a (rotate b 2))
      (= a (rotate b 3))
      (= a (rotate (reflect  b) 1))
      (= a (rotate (reflect  b) 2))
      (= a (rotate (reflect  b) 3))))

(defn distinct-boards
  ([s]
   (distinct-boards [] s))
  ([s1 [i & s2]]
   (cond
     (nil? i) s1
     (every? #(not (board-eq? % i) s1)) (distinct-boards (cons i s1) s2)
     :else (distinct-boards s1 s2))))

(defn allowed? [n i board]
  (or
   (empty? board)
   (and
    (apply = " " (map #(nth % i) board))
    (apply = " " (map #(if (< (+ i %2) n) (nth % (+ i %2)) " ") board (range (count board) 0 -1)))
    (apply = " " (map #(if (<= 0 (- i %2)) (nth % (- i %2)) " ") board (range (count board) 0 -1))))))

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

  (defn -main
    [& args]
    (doseq [n (range  9)]
      (println "================================")
      (print "n=" n)
      (let [s (nqueens n)]
        (println " has " (count s) "solutions," (count (distinct-boards s)) "distinct")
        (doseq        [i (range (count s))]
          (println "---------")
          (println "n=" n "solution " (inc i) "of" (count s))
          (print-solution (nth s i))
          )))))

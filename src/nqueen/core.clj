(ns nqueen.core
  (:require [clojure.pprint]
            [clojure.tools.trace])
  (:gen-class))




(defn allowed? [n i board]
  (cond
    (empty? board)                                                                                    true
    (and
     (apply = " " (map #(nth % i) board))
     (apply = " " (map #(if (< (+ i %2) n) (nth % (+ i %2)) " ") board (range (count board) 0 -1)))
     (apply = " " (map #(if (<= 0 (- i %2)) (nth % (- i %2)) " ") board (range (count board) 0 -1)))) true
    :else                                                                                             false)
  )

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
  (nqueens* n [])
  )

(defn -main
  [& args]
  (doseq [n (range  9)]
    (println "================================")
    (println "n=" n)
    (let [s (nqueens n)]
      (doseq        [i (range (count s))]
        (println "---------")
        (println "n=" n "solution " (inc i) "of" (count s))
        (doseq        [r (range n)]
          (println (nth (nth s i) r))
)
        ))))

;(clojure.tools.trace/trace-ns 'nqueen.core)

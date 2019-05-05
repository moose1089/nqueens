(ns nqueen.core
  (:require [clojure.pprint])
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

(allowed? 3 0 [])
(allowed? 3 0 [["Q" " " " "]])
(allowed? 3 0 [[" " "Q" " "]])
(allowed? 3 0 [[" " " " "Q"]])
(allowed? 3 1 [["Q" " " " "]])
(allowed? 3 1 [[" " "Q" " "]])
(allowed? 3 1 [[" " " " "Q"]])
(allowed? 3 2 [["Q" " " " "]])
(allowed? 3 2 [[" " "Q" " "]])
(allowed? 3 2 [[" " " " "Q"]])

(allowed? 3 0 [["Q" " " " "][" " " " "Q"]])
(allowed? 3 0 [[" " "Q" " "]["Q" " " " "]])
(allowed? 3 0 [[" " " " "Q"]["Q" " " " "]])

(defn possible-locations
  [n board]
  (for [i (range  n)]
    (let [col (concat (take i (repeat " ")) ["Q"] (take (- n i 1) (repeat " ")))]
      col)))

;;(possible-locations 3 [] )

(defn nqueens* [n board]
  (apply concat
         (for [i (possible-locations n board)]
           (let [new-board (cons i board)]
             (if (= n (count new-board))
               [new-board]
               (nqueens* n new-board))))))


(defn nqueens [n]
  (nqueens* n [])
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (doseq [n (range 3)]
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

(ns crypto-analysis.core
  (:gen-class))

(def english
  [0.08167 0.01492 0.02782 0.04253 0.12702 0.02228 0.02015 0.06094 0.06966 0.00153 0.00772 
0.04025 0.02406 0.06749 0.07507 0.01929 0.00095 0.05987 0.06327 0.09056 0.02758 0.00978 
0.02360 0.00150 0.01974 0.00074])

(defn chi-squared
  "compute the chi square statistic from the list of all letter couunts"
  [counts]
  (let
    [
     full-counts (map #(get counts % 0) (map #(char (+ 65 %)) (range 26)))
     total (apply + full-counts)
     english-counts (map #(* total %) english)
     ]
    (apply + (map #(/ (Math/pow (- %1 %2) 2) %2) full-counts english-counts))
    ))

(defn get-letters
  "Remove everything but letters"
  [phrase]
  (-> phrase clojure.string/upper-case (clojure.string/replace #"[^A-Z]" "")))

(defn compute-frequencies
  "Compute the frequencies of each letter"
  [phrase]
     (-> phrase get-letters frequencies))

(defn caesar
  "
  Caesar substitution cypher.
  phrase: the text
  n: the offset to use
  "
  [phrase n]
  (apply str (map #(char (+ 65 (mod (- (+ (int %) n) 65) 26))) (seq phrase))))

(defn caesar-chi-sq
  "Performs the brute force attack using the chi-squared statistic"
  [phrase]
  (apply min-key #(-> % compute-frequencies chi-squared) (map #(caesar phrase %)  (range 26)))
)

(defn -main
  "Main program that takes the texts as arguments"
  [& args]
  (println "Caesar Attack by Chi Squared Statistic")
  (doseq [phrase args]
    (println (caesar-chi-sq (get-letters phrase)))))

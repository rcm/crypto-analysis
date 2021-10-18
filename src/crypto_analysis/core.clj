(ns crypto-analysis.core
  (:gen-class))

(def english
  [0.08167 0.01492 0.02782 0.04253 0.12702 0.02228 0.02015 0.06094 0.06966 0.00153 0.00772 
0.04025 0.02406 0.06749 0.07507 0.01929 0.00095 0.05987 0.06327 0.09056 0.02758 0.00978 
0.02360 0.00150 0.01974 0.00074])


(defn letter-to-pos
  [letter] (- (int letter) 65))

(defn pos-to-letter
  [pos] (char (+ 65 pos)))
  
(defn rot26
  [x n] (mod (+ x n) 26))
  
(defn rotate-letter
  [letter n] (-> letter letter-to-pos (rot26 n) pos-to-letter))  

(def all-uppercase-letters
  (map pos-to-letter (range 26)))

(defn chi-squared
  "compute the chi square statistic from the list of all letter couunts"
  [full-counts]
  (let
    [
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

(defn compute-digraphs
  "Compute the digraph frequencies"
  [phrase]
  (let
    [letters (get-letters phrase)]
	(frequencies (map str letters (rest letters)))))

(defn get-full-counts
  "Returns a vector with the counts for all the letters in the alphabet"
  [phrase]
  (->> (-> phrase compute-frequencies) ((fn [counts] (map #(get counts % 0) all-uppercase-letters)))))

(defn substitute-cypher
  "Substitution Cypher"
  [phrase alphabet]
  (let
    [dict (into {} (map-indexed #(vector (pos-to-letter %1) %2) (-> alphabet clojure.string/upper-case seq)))]
    (apply str (map dict (-> phrase clojure.string/upper-case get-letters seq)))))

(defn caesar
  "
  Caesar substitution cypher.
  phrase: the text
  n: the offset to use
  "
  [phrase n]
  (apply str (map #(rotate-letter % n) (seq phrase))))

(defn caesar-chi-sq
  "Performs the brute force attack using the chi-squared statistic"
  [phrase]
  (apply min-key #(-> % get-full-counts chi-squared) (map #(caesar phrase %)  (range 26)))
)

(defn order-by-counts
  "Order letters by their frequency/counts"
  [counts]
  (-> (map #(vector (pos-to-letter (first %)) (last %)) (sort-by last > (map-indexed vector counts)))))


(defn naive-subst-attack
  [cypher-text]
  (->> (map #(map first (vector %1 %2)) (-> cypher-text get-full-counts  order-by-counts ) (-> english order-by-counts )) (sort-by first) (map last) (apply str)))
  
(def rand-txt (substitute-cypher "to be or not to be, that is the question" (apply str (shuffle all-uppercase-letters))))

(defn -main
  "Main program that takes the texts as arguments"
  [& args]
  (println "Caesar Attack by Chi Squared Statistic")
  (doseq [phrase args]
    (println (caesar-chi-sq (get-letters phrase)))))

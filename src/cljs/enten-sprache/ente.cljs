(ns ente
  (:require [clojure.string :as s]))

(def z (into [] " abcdefghijklmnopqrstuvwxyz"))

(defn index-of [item coll]
  (count (take-while (partial not= item) coll)))

(defn string_to_number[string]
   (map #(index-of % z) string)
 )

(defn number_to_string[v]
   (map #(get z %) v)
 )

(defn convert-base[x y]
  (let [v (take-while #(> % 0) (iterate #(quot % x) y))]
     (reduce conj () (doall (map #(mod % x) v)))
   )
)

(defn goedel[string]
  (reduce #(+(* %1 37) %2) (string_to_number string))
 )


(defn number_to_seq[number]
  (convert-base 10 number)
)

(def quack ["Quaack?" "quack" "quaack" "quaack." "Quack" "Quaack" "Quaack." "QUACK" "QUAACK" "QUAACK!"])


(defn quackisieren[v]
  (s/join (map #(get quack (long %)) (number_to_seq v)))
)

(defn quack_deref[string]
   (str (index-of string quack))
)

(defn dequackisieren[string]
  (let [v (map #(vec [% (quack_deref %)]) ["Quaack?" "quaack." "Quaack." "QUAACK!" "quaack" "Quaack" "QUAACK" "quack" "Quack" "QUACK"])]
    (reduce (fn [a [b c]] (s/replace a b c)) string v)
  )
 )

(defn degoedel[number]
  (s/join (number_to_string (convert-base 37 number)))
)

(defn mensch-ente[string]
  (quackisieren (goedel string))
)

(defn ente-mensch[string]
  (degoedel(dequackisieren string))
)

(defn translate[string]
  (s/join " " (map mensch-ente (s/split string #"\s")) )
)

(defn detranslate[string]
  (s/join " " (map ente-mensch (s/split string #"\s")) )
)



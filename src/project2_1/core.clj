(ns project2-1.core)

(defn lookup [i m]
  "This function looks up a value, i, in map m and returns the result if it exists, and otherwise returns i."
  (get m i i))

(defn substitute [l m]
  (map (fn [i] (lookup i m)) l))

(defn deep-substitute [l m]
  (map (fn [i]
         (if (seq? i)
           (deep-substitute i m)
           (lookup i m)))
       l))

(defn boolean? [i]
  (or (= i true) (= i false))
  )

;(defn nand [i]
;  (cond
;    (some false? i) '(true)
;    (every? true? (rest i)) '(false)
;    :else (if (not(every? boolean? (rest i)))
;            (do (remove true? i));True if not all booleans
;            ())))

(defn nand [& args]
  (if ()
    (cond
    (some false? args) true
    (every? true? args) false
    :else (if (not(every? boolean? args))
            (concat '(nand) (remove true? args))
            )))
  )

(def replacements {})


(def test-list '(nand x y true (nand x true)))
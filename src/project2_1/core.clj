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


;(defn nand [& args]
;  (let [a (dedupe args)]
;    (cond
;    (some false? a) true
;    (every? true? a) false
;    :else (do (if (not(every? boolean? a))
;            (concat '(nand) (remove true? a))))
;    )
;    )
;  )

(defn nand-simplify [input]
  (let [a (dedupe input)]
    (if (or (not (= (some seq? a) nil)) (not (= (some seq? a) false)))
      (map #((replace {% (nand-simplify %)} a)) (filter seq? a))
      (cond
        (some false? a) true
        (every? true? (rest a)) false
        :else (do
                (remove true? a)

                ))
        )
      )
    )

(def test-list '(nand 'x 'y true (nand 'x true)))
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

(defn nand-eval [i]
  (cond
    (some false? i) true
    (every? true? (rest i)) false
    :else (remove true? i)
    )
  )

(declare nand-simplify)
(defn build-substitutions [i]
  (let [substitutions {}] (let [l1 ()]
														(apply hash-map (apply concat (map #(first (concat substitutions {% (nand-simplify %)})) i)))
  )))

(defn nand-simplify [input]
	(let [a (dedupe input)]
		(if (some seq? a)
			(if (and (and (= (count (nand-eval (substitute a (build-substitutions (filter seq? a))))) 2)
										(seq? (second (nand-eval (substitute a (build-substitutions (filter seq? a))))))) ;THIS IS LOOKS AWFUL
							 (= (count (second (nand-eval (substitute a (build-substitutions (filter seq? a)))))) 2)) ;IT WORKS SO IM KEEPING IT
							 (second (second (nand-eval (substitute a (build-substitutions (filter seq? a))))))
							 (do (if (and (= (count (nand-eval (substitute a (build-substitutions (filter seq? a))))) 3) ;IM SORRY
														(= (count (nth (nand-eval (substitute a (build-substitutions (filter seq? a)))) 2)) 2))
										 (do (if (= (second (nand-eval (substitute a (build-substitutions (filter seq? a)))))
																(nth (nand-eval (substitute a (build-substitutions (filter seq? a)))) 1))
													 true
													 (nand-eval (substitute a (build-substitutions (filter seq? a))))
													 ))
										 (nand-eval (substitute a (build-substitutions (filter seq? a))))
										 )))

				;(nand-eval (substitute a (build-substitutions (filter seq? a)))))
				(nand-eval a))

			)
		)


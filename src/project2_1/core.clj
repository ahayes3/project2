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

(defn special-cases [l]

				 ;(if (and (= (count (rest l)) 1) (second l))
				 ;
					; )

	)

(defn nand-simplify [input]
	(let [a (dedupe input)]
		(if (some false?
							(map (fn [i]
										 (if (seq? i)
											 (nand-simplify i)
											 i)) a))
			true
			(if (every? true? (rest (map (fn [i]
																		 (if (seq? i)
																			 (nand-simplify i)
																			 i)) a)))
				false
				(remove true? (map (fn [i]
														 (if (seq? i)
															 (nand-simplify i)
															 i)) a))))))

(defn not-nand [input]
	(deep-substitute input {'not 'nand}))

(defn or-nand [input]

	(substitute (map (fn [i]
										 (if (seq? i)
											 (or-nand i)
											 (if (= i 'or)
												 'or
												 (if (= (first input) 'or)
													 (list 'nand i)
													 i))
											 )

										 ) input) {'or 'nand}))

(defn and-nand [input]
	(if (= (first input) 'and)
		(list 'nand (substitute (map (fn [i]
																	 (if (seq? i)
																		 (and-nand i)
																		 i
																		 )
																	 ) input) {'and 'nand}))
		(substitute (map (fn [i]
																	 (if (seq? i)
																		 (and-nand i)
																		 i
																		 )
																	 ) input) {'and 'nand})

		)
	)

(defn simplify-exp [input]
	(and-nand (not-nand (or-nand input)))
	)

(defn evalexp [exp bindings]
	(nand-simplify (simplify-exp (deep-substitute exp bindings)))
	)

(def p1 '(and x (or x (and y (not z)))))
(def p2 '(and (and z false) (or x true false)))
(def p3 '(or true a))
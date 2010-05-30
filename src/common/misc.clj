(ns common.misc)

;; '#' does an autogensym on the symbol it is appended to
;; '~' is unquote
;; '~@' is unquote splice
(defmacro debug
  [expr]
  `(let [result# ~expr]
     (println '~expr "=>" result#)
     (flush)
     result#))

(defmacro unless [condition & body]
  `(when (not ~condition)
     ~@body))

(defn class-exists?
  "Fully qualified names are required"
  [cname]
  (try
   (do (Class/forName cname)
       true)
   (catch ClassNotFoundException e false)))

;; (defn palindromic? [num]
;;   (let [numstr (str num), len (count numstr)]
;;     (loop [low 0, high (- len 1)]
;;       (cond (<= (- high low) 0) true
;;             (not (= (nth numstr low) (nth numstr high))) false
;;             :else (recur (+ low 1) (- high 1))))))

;(defn palindromic-2? [num]
(defn palindrome? [num]
  (let [s (seq (str num))]
    (= s (reverse s))))

(defn make-palindrome
  "make palindrome by mirroring string starting at last character
ex. (make-palindrome 'string') => 'gnirtsstring'"
  [s]
  (reduce str (concat (reduce str (reverse s)) s)))
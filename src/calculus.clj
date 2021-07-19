(ns calculus)

;; λx. λy. x
(defn T
  [x]
  (fn [y] x))

;; λx. λy. y
(defn F
  [x]
  (fn [y] y))

(defn And
  [x]
  (fn [y] ((x y) x)))

(defn Or
  [x]
  (fn [y] ((x x) y)))

(defn Not
  [x]
  ((x F) T))

(defn Xor
  [x]
  (fn [y] ((x (Not y)) y)))

(defn If
  [p]
  (fn [x]
    (fn [y] ((p x) y))))

(defn ToBoolean
  [f]
  ((f true) false))

;; Church numerals

(defn Zero
  [_f]
  (fn [x] x))

(defn One
  [f]
  (fn [x] (f x)))

(defn Two
  [f]
  (fn [x] (f (f x))))

(defn Three
  [f]
  (fn [x] (f (f (f x)))))

(defn Succ
  [n]
  (fn [f]
    (fn [x] (f ((n f) x)))))

(defn ToNumber
  [f]
  ((f (fn [n] (+ n 1))) 0))

(defn FromNumber
  [n]
  (if (zero? n)
    Zero
    (Succ (FromNumber (- n 1)))))

(defn Add
  [x]
  (fn [y]
    ((y Succ) x)))

(defn Exp
  [x]
  (fn [y] (y x)))
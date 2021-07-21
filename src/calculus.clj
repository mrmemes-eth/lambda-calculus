(ns calculus)

;; λx. λy. x
(defn T
  [x]
  (fn [_y] x))

;; λx. λy. y
(defn F
  [_x]
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

(defn Multiply
  [x]
  (fn [y]
    (fn [f] (x (y f)))))

;; Struggled with this, but this helped: https://stackoverflow.com/a/8797545.
;; The algorithm is still pretty damned inscrutable; I grok it well enough to
;; emulate it, but I couldn't have written it from first principles...
(defn Pred
  ""
  [n]
  (fn [f]
    (fn [x]
      (((n (fn [g]
             (fn [h] (h (g f)))))
        (fn [_u] x))
       (fn [u] u)))))

(defn Subtract
  [x]
  (fn [y] ((y Pred) x)))

;; Combinators

(defn Y
  "A fun way to make stack overflows! This is a correct implementation, but
   because of eager evaluation, it recures infinitely."
  [f]
  ((fn [x] (f (x x)))
   (fn [x] (f (x x)))))

(defn Factorial
  [f]
  (fn [n]
    (println n)
    (((If n)
      ((Multiply n) (f (Pred n))))
     ;; Zero evaluates to false
     One)))

(defn Z
  [f]
   ((fn [x]
     (f (fn [g] ((x x) g))))
    (fn [x]
      (f (fn [g] ((x x ) g))))))

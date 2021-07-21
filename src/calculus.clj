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

(defn Equivalent
  "Returns T if the provided Church Booleans are the same"
  [fx]
  (fn [fy] (Not ((Xor fx) fy))))

(defn ToBoolean
  "Uses native booleans for test and debugging insight, not part of The Lambda Calculus."
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

(defn IsZero
  "Determines if a Church Numeral is Zero or not based on whether the provided
   function is invoked, since Zero doesn't invoke its passed function."
  [n]
  ((n (fn [_t] F)) T))

(defn Succ
  [n]
  (fn [f]
    (fn [x] (f ((n f) x)))))

(defn ToNumber
  "Uses native numeric primitives for test and debugging insight, not part of The Lambda Calculus."
  [f]
  ((f (fn [n] (+ n 1))) 0))

(defn FromNumber
  "Uses native numeric primitives for test and debugging insight, not part of The Lambda Calculus."
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
  "Yields the 'predecessor' of the provided Church-encoded numeral."
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
   because of eager evaluation in Clojure, it recurses infinitely."
  [f]
  ((fn [x] (f (x x)))
   (fn [x] (f (x x)))))

(defn Factorial
  [f]
  (fn [n]
    #_(((If (IsZero n)) One)
     ((Multiply n) (f (Pred n))))
    (if (zero? (ToNumber n))
      One
      ((Multiply n) (f (Pred n))))))

(defn Z
  "A Y Combinator for Generation Z... *cough* It's a Y Combinator in spirit, but
  handles eager evaluation by wrapping things that would be eagerly evaluated in
  functions to defer evaluation until JIT."
  [f]
   ((fn [x]
     (f (fn [g] ((x x) g))))
    (fn [x]
      (f (fn [g] ((x x) g))))))

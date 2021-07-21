(ns calculus-test
  (:require [calculus :as c]
            [clojure.test :as t :refer [deftest is]]))

(deftest test-T
  (is (= ((c/T "NO") "YES") "NO")))

(deftest test-F
  (is (= ((c/F "NO") "YES") "YES")))

(deftest test-And
  (is (= ((c/And c/T) c/T) c/T))
  (is (= ((c/And c/T) c/F) c/F))
  (is (= ((c/And c/F) c/T) c/F))
  (is (= ((c/And c/F) c/F) c/F)))

(deftest test-Or
  (is (= ((c/Or c/T) c/T) c/T))
  (is (= ((c/Or c/T) c/F) c/T))
  (is (= ((c/Or c/F) c/T) c/T))
  (is (= ((c/Or c/F) c/F) c/F)))

(deftest test-Not
  (is (= (c/Not c/T) c/F))
  (is (= (c/Not c/F) c/T)))

(deftest test-Xor
  (is (= ((c/Xor c/T) c/T) c/F))
  (is (= ((c/Xor c/T) c/F) c/T))
  (is (= ((c/Xor c/F) c/T) c/T))
  (is (= ((c/Xor c/F) c/F) c/F)))

(deftest test-If
  (is (= (((c/If c/T) c/T) c/F) c/T))
  (is (= (((c/If c/F) c/T) c/F) c/F))
  (is (= (((c/If c/T) c/F) c/T) c/F))
  (is (= (((c/If c/F) c/F) c/T) c/T)))

(deftest test-Equivalent
  (is (= ((c/Equivalent c/T) c/T) c/T))
  (is (= ((c/Equivalent c/T) c/F) c/F))
  (is (= ((c/Equivalent c/F) c/T) c/F))
  (is (= ((c/Equivalent c/F) c/F) c/T)))

(deftest test-ToBoolean
  (is (true? (c/ToBoolean c/T)))
  (is (false? (c/ToBoolean c/F))))

(deftest test-church-numerals
  (is (= (c/ToNumber c/Zero) 0))
  (is (= (c/ToNumber c/One) 1))
  (is (= (c/ToNumber c/Two) 2))
  (is (= (c/ToNumber c/Three) 3)))

(deftest test-IsZero
  (is (= (c/IsZero c/Zero) c/T))
  (is (= (c/IsZero c/One) c/F)))

(deftest test-Succ
  (is (= (c/ToNumber (c/Succ c/Zero)) (c/ToNumber c/One)))
  (is (= (c/ToNumber (c/Succ c/Two)) (c/ToNumber c/Three))))

(deftest test-FromNumber
  (is (= (c/FromNumber 0) c/Zero))
  (is (= (c/ToNumber (c/FromNumber 1)) 1))
  (is (= (c/ToNumber (c/FromNumber 42)) 42)))

(deftest test-Add
  (is (= (c/ToNumber ((c/Add c/Two) c/Three)) 5))
  (is (= (c/ToNumber ((c/Add (c/FromNumber 7)) (c/FromNumber 8))) 15)))

(deftest test-Exp
  (is (= (c/ToNumber ((c/Exp c/Two) c/Two)) 4))
  (is (= (c/ToNumber ((c/Exp c/Three) c/Two)) 9))
  (is (= (c/ToNumber ((c/Exp c/Three) c/Three)) 27)))

(deftest test-Multiply
  (is (= (c/ToNumber ((c/Multiply c/Two) c/Two)) 4))
  (is (= (c/ToNumber ((c/Multiply c/Three) c/Two)) 6))
  (is (= (c/ToNumber ((c/Multiply c/Three) ((c/Multiply c/Three) c/Three))) 27)))

(deftest test-Pred
  (is (= (c/ToNumber (c/Pred c/Three)) 2))
  (is (= (c/ToNumber (c/Pred c/One)) 0)))

(deftest test-Subtract
  (is (= (c/ToNumber ((c/Subtract c/Three) c/Two)) 1))
  (is (= (c/ToNumber ((c/Subtract c/Three) c/Three)) 0))
  ;; no encoding for negative numbers and this implementation bottoms out at identity or zero:
  (is (= (c/ToNumber ((c/Subtract c/Two) c/Three)) 0)))

(deftest test-Factorial
  (is (= (c/ToNumber ((c/Y c/Factorial) c/Three)) 6)) ;; boom goes the overflow
  (is (= (c/ToNumber ((c/Z c/Factorial) c/Zero)) 1))
  (is (= (c/ToNumber ((c/Z c/Factorial) c/Three)) 6)))

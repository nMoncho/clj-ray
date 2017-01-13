(ns clj-ray.math
  (:gen-class))


(def e 0.0000000001)
(defn pow2 [a] (* a a))
(defn spow2 [a] (pow2 (if (zero? a) e a)))
(defn sqrt [a] (Math/sqrt a))
(defn sin [a] (Math/sin a))
(defn cos [a] (Math/cos a))
(defn toRad [a] (* a (/ Math/PI 180.0)))
(defn toDeg [a] (* a (/ 180.0 Math/PI)))

(defn point
  "Creates a point x y"
  [x y]
  {:x x
   :y y})

(def vect point) ;; Alias of point

(defn trunc
  [p]
  (apply point (map (comp int second) p)))

(defn add
  [{xa :x ya :y} {xb :x yb :y}]
  (point (+ xa xb) (+ ya yb)))

(defn scale
  "Scales a vector by a scalar"
  [{x :x y :y} s]
  (point (* x s) (* y s)))

(defn length
  [{x :x y :y}]
  (let [ll (+ (* x x) (* y y))]
    (Math/sqrt ll)))

(defn ortho
  "Creates an orthogonal vector"
  [{x :x y :y}]
  (point (- y) x)) ;; Is just component swap

(defn neg
  "Creates a negated vector"
  [{x :x y :y}]
  (point (- x) (- y)))

(defn rotate
  "Rotates a vector by an radian angle"
  [{x :x y :y} ang]
  (let [sina (sin ang)
        cosa (cos ang)
        xx (+ (* cosa x) (* sina y))
        yy (+ (* -1 sina x) (* cosa y))]
    (point xx yy)))

(defn dot
  "Dot product of two vectors"
  [{xa :x ya :y} {xb :x yb :y}]
  (let [x (* xa xb)
        y (* ya yb)]
    (+ x y)))


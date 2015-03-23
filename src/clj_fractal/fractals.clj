(ns clj-fractal.fractals
  (:require [clojure.math.numeric-tower :as math]))

(def max-iteration 1000)
(def bailout 65536M)

(defn divide-with-precision [^BigDecimal a ^BigDecimal b]
  (with-precision 50 (/ a b)))

(defn scale [^BigDecimal v ^BigDecimal a ^BigDecimal b ^BigDecimal min ^BigDecimal max]
  (+ (divide-with-precision (* (- b a) (- v min)) (- max min)) a))

(defn scale-x [^BigDecimal x ^BigDecimal max view-port]
  (scale x (second view-port) (nth view-port 3) (* max -1) max))

(defn scale-y [^BigDecimal y ^BigDecimal  max view-port]
  (scale y (nth view-port 2) (first view-port) (* max -1) max))

(defn cartesian-x [^BigDecimal x ^BigDecimal mid]
  (- x mid))

(defn cartesian-y [^BigDecimal y ^BigDecimal mid]
  (- mid y))

(defn process-point [[^BigDecimal x0 ^BigDecimal y0]]
  ;(println "pp")
  ;(println x0 " - " y0)
  (loop [x (double x0)
         y (double y0)
         iteration 0]
    ;(println "loop")
    ;(println x " - " y)
    (let [x2  (* x x)
          y2  (* y y)]
      (if (or (>= iteration max-iteration)
              (<= bailout (+ x2 y2)))
        (if (< iteration max-iteration)
          (-> (math/sqrt (+ x2 y2))
              Math/log
              (/ (Math/log 2))
              Math/log
              (/ (Math/log 2))
              (#(- % (inc iteration))))
          iteration)
        (let [newX (+ x0 (- x2 y2))
              newY (+ y0 (* x y 2))]
          (if (and (= x newX) (= y newY))
            max-iteration
            (recur newX newY (inc iteration))))))))

(defn distance [^BigDecimal x1 ^BigDecimal y1 ^BigDecimal x2 ^BigDecimal y2]
   (math/sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1)))))

(defn calc-view-port [[^BigDecimal x ^BigDecimal y] [^BigDecimal origin-x ^BigDecimal origin-y] dim]
  (let [new-dim [(divide-with-precision (distance origin-x origin-y origin-x (first dim)) 4.0M)
                 (divide-with-precision (distance origin-x origin-y (second dim) origin-y) -10.0M)
                 (divide-with-precision (distance origin-x origin-y origin-x (nth dim 2)) -4.0M)
                 (divide-with-precision (distance origin-x origin-y (nth dim 3) origin-y) 4.0M)]]
    [(+ y (first new-dim))
     (+ x (second new-dim))
     (+ y (nth new-dim 2))
     (+ x (nth new-dim 3))]))

(defn mandelbrot [screen-dim new-origin dim origin]
  (let [mid-width (/ (first screen-dim) 2)
        mid-height (/ (second screen-dim) 2)
        prev-origin-x (first origin)
        prev-origin-y (second origin)
        origin-x (scale-x (cartesian-x (first new-origin) mid-width) mid-width dim)
        origin-y (scale-y (cartesian-y (second new-origin) mid-height) mid-height dim)
        _ (println [origin-x origin-y dim])
        view-port (calc-view-port [origin-x origin-y] [prev-origin-x prev-origin-y] dim)
        _ (println view-port)
        scaled-points (doall (for [y (range (second screen-dim))
                                   x (range (first screen-dim))]
                               [(scale-x (cartesian-x x mid-width) mid-width view-port)
                                (scale-y (cartesian-y y mid-height) mid-height view-port)]))]
    {:data   (doall (pmap process-point scaled-points))
     :dim    view-port
     :origin [origin-x origin-y]}))


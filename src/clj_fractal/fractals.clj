(ns clj-fractal.fractals
  (:require [clojure.math.numeric-tower :as math]))

(def max-iteration 100)
(def bailout 4M)
(def dim [2.5M -2.5M -2.5M 2.5M])
(def ZOOM 16)

(defn divide-with-precision [^BigDecimal a ^BigDecimal b]
  (with-precision 100 (/ a b)))

(defn scale [^BigDecimal v ^BigDecimal a ^BigDecimal b ^BigDecimal min ^BigDecimal max]
  (+ (with-precision 50 (/ (* (- b a) (- v min)) (- max min))) a))

(defn scale-x [^BigDecimal x ^BigDecimal max view-port]
  (scale x (second view-port) (nth view-port 3) (* max -1) max))

(defn scale-y [^BigDecimal y ^BigDecimal  max view-port]
  (scale y (nth view-port 2) (first view-port) (* max -1) max))

(defn cartesian-x [^BigDecimal x ^BigDecimal mid]
  (- x mid))

(defn cartesian-y [^BigDecimal y ^BigDecimal mid]
  (- mid y))

(defn process-point [[^BigDecimal x0 ^BigDecimal y0]]
  (loop [x (double x0)
         y (double y0)
         iteration 0]
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

(defn calc-view-port [[^BigDecimal x ^BigDecimal y] zoom]
  [(+ y (divide-with-precision (first dim) zoom))
   (+ x (divide-with-precision (second dim) zoom))
   (+ y (divide-with-precision (nth dim 2) zoom))
   (+ x (divide-with-precision (nth dim 3) zoom))])

(defn mandelbrot [screen-dim new-origin zoom origin]
  (let [mid-width (/ (first screen-dim) 2)
        mid-height (/ (second screen-dim) 2)
        prev-origin-x (first origin)
        prev-origin-y (second origin)
        origin-x (-> (first new-origin)
                     (cartesian-x mid-width)
                     (scale-x mid-width dim)
                     (divide-with-precision (if (= 1 zoom) 1 (/ zoom ZOOM)))
                     (+ prev-origin-x))
        origin-y (-> (second new-origin)
                     (cartesian-y mid-height)
                     (scale-y mid-height dim)
                     (divide-with-precision (if (= 1 zoom) 1 (/ zoom ZOOM)))
                     (+ prev-origin-y))
        view-port (calc-view-port [origin-x origin-y] zoom)
        scaled-points (doall (for [y (range (second screen-dim))
                                   x (range (first screen-dim))]
                               [(scale-x (cartesian-x x mid-width) mid-width view-port)
                                (scale-y (cartesian-y y mid-height) mid-height view-port)]))]
    {:data   (doall (pmap process-point scaled-points))
     :zoom   (* ZOOM zoom)
     :origin [origin-x origin-y]}))


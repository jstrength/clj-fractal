(ns clj-fractal.fractals
  (:require [clojure.math.numeric-tower :as math]))

(def max-iteration 2000)
(def bailout 65536)
(def dim [2.5 -2.5 -2.5 2.5])
(def ZOOM 16)

(defn scale [v a b min max]
  (+ (/ (* (- b a) (- v min)) (- max min)) a))

(defn scale-x [x max view-port]
  (scale x (second view-port) (nth view-port 3) (* max -1) max))

(defn scale-y [y  max view-port]
  (scale y (nth view-port 2) (first view-port) (* max -1) max))

(defn cartesian-x [x mid]
  (- x mid))

(defn cartesian-y [y mid]
  (- mid y))

(defn process-point [[x0 y0]]
  (let [y02 (Math/pow y0 2)
        q (+ (Math/pow (- x0 1/4) 2) y02)]
    (if (or (> (* 1/4 y02) (* q (+ q (- x0 1/4))))          ;cardioid checking
            (> 1/16 (+ y02 (Math/pow (inc x0) 2))))         ;period-2 bulb checking
      0
      (loop [x (double x0)
             y (double y0)
             ;e 0.000000000001
             iteration 0
             ]
        (let [x2 (* x x)
              y2 (* y y)]
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
            (let [new-x (+ x0 #_(* 2 x e) #_(* e e) (- x2 y2))
                  new-y (+ y0 #_(* 2 y e) (* x y 2))
                  ;new-e (+ (* e e) (* 2 (+ x y) e))
                  ]
              (if (and (= x new-x) (= y new-y))             ;periodicty checking
                max-iteration
                (recur new-x new-y #_new-e (inc iteration))))))))))

(defn calc-view-port [[x y] zoom]
  [(+ y (/ (first dim) zoom))
   (+ x (/ (second dim) zoom))
   (+ y (/ (nth dim 2) zoom))
   (+ x (/ (nth dim 3) zoom))])

(defn mandelbrot [screen-dim new-origin zoom origin]
  (let [mid-width (/ (first screen-dim) 2)
        mid-height (/ (second screen-dim) 2)
        prev-origin-x (first origin)
        prev-origin-y (second origin)
        origin-x (-> (first new-origin)
                     (cartesian-x mid-width)
                     (scale-x mid-width dim)
                     (/ (if (= 1 zoom) 1 (/ zoom ZOOM)))
                     (+ prev-origin-x))
        origin-y (-> (second new-origin)
                     (cartesian-y mid-height)
                     (scale-y mid-height dim)
                     (/ (if (= 1 zoom) 1 (/ zoom ZOOM)))
                     (+ prev-origin-y))
        view-port (calc-view-port [origin-x origin-y] zoom)
        scaled-points (doall (for [y (range (second screen-dim))
                                   x (range (first screen-dim))]
                               [(scale-x (cartesian-x x mid-width) mid-width view-port)
                                (scale-y (cartesian-y y mid-height) mid-height view-port)]))]
    {:data   (doall (pmap process-point scaled-points))
     :zoom   (* ZOOM zoom)
     :origin [origin-x origin-y]}))


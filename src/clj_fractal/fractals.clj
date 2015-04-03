(ns clj-fractal.fractals
  (:require [quil.core :as q]))

(def max-iteration 1000)
(def bailout 65536)
(def dim [2.5 -2.5 -2.5 2.5])

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

(def process-point
  (memoize
    (fn [[x0 y0]]
      ;(println x0 y0)
      (let [y02 (q/sq y0)
            q (+ (q/sq (- x0 1/4)) y02)]
        (if (or (> (* 1/4 y02) (* q (+ q (- x0 1/4))))      ;cardioid checking
                (> 1/16 (+ y02 (q/sq (inc x0)))))           ;period-2 bulb checking
          0
          (loop [x x0
                 y y0
                 iteration 0]
            (let [x2 (* x x)
                  y2 (* y y)]
              (if (or (>= iteration max-iteration)
                      (<= bailout (+ x2 y2)))
                iteration
                (let [new-x (+ x0 (- x2 y2))
                      new-y (+ y0 (* x y 2))]
                  (if (and (= x new-x) (= y new-y))         ;periodicty checking
                    max-iteration
                    (recur new-x new-y (inc iteration))))))))))))

(defn get-boarder-points [x y width height points->scaled]
  (let [top (map #(vector % y) (range x width))
        bottom (map #(vector % (dec height)) (range x width))
        left (map #(vector x %) (range y height))
        right (map #(vector (dec width) %) (range y height))
        points (->> (concat top bottom right left)
                    (map points->scaled)
                    (map process-point))]
    (if (apply = points)
      (first points)
      -1)))

(defn boarder-tracer [x y width height points->scaled]
  (let [boarder-points (get-boarder-points x y width height points->scaled)]
    (if (or (> 3 (q/dist x y width height))
            (not= -1 boarder-points))
      (do (q/fill (mod boarder-points 100) 100 100)
          (q/stroke (mod (* 2 boarder-points) 100) 100 100)
          (q/rect x y width height))
      (let [half-width (+ x (int (/ (- width x) 2)))
            half-height (+ y (int (/ (- height y) 2)))]
        (boarder-tracer x y half-width half-height points->scaled)
        (boarder-tracer half-width y width half-height points->scaled)
        (boarder-tracer x half-height half-width height points->scaled)
        (boarder-tracer half-width half-height width height points->scaled)))))

(defn calc-view-port [[x y] zoom]
  [(+ y (/ (first dim) zoom))
   (+ x (/ (second dim) zoom))
   (+ y (/ (nth dim 2) zoom))
   (+ x (/ (nth dim 3) zoom))])

(defn mandelbrot [[screen-width screen-height] origin new-origin curr-zoom ZOOM]
  (let [mid-width (/ screen-width 2)
        mid-height (/ screen-height 2)
        prev-origin-x (first origin)
        prev-origin-y (second origin)
        origin-x (-> (first new-origin)
                     (cartesian-x mid-width)
                     (scale-x mid-width dim)
                     (/ (if (= 1 curr-zoom) 1 (/ curr-zoom ZOOM)))
                     (+ prev-origin-x))
        origin-y (-> (second new-origin)
                     (cartesian-y mid-height)
                     (scale-y mid-height dim)
                     (/ (if (= 1 curr-zoom) 1 (/ curr-zoom ZOOM)))
                     (+ prev-origin-y))
        view-port (calc-view-port [origin-x origin-y] curr-zoom)
        points->scaled (time (doall (into {} (for [y (range screen-height)
                                                   x (range screen-width)]
                                               (hash-map [x y]
                                                         [(scale-x (cartesian-x x mid-width) mid-width view-port)
                                                          (scale-y (cartesian-y y mid-height) mid-height view-port)])))))]
    (boarder-tracer 0 0 screen-width screen-height points->scaled)
    {:data   []                                             ; (time (doall (pmap process-point scaled-points)))
     :origin [origin-x origin-y]}))


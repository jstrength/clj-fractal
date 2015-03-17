(ns clj-fractal.fractals)

(def xMin -2.5)
(def xMax 1)
(def yMin -1)
(def yMax 1)

(defn get-color [iteration]
  (mod iteration 255))

(defn scale [v a b min max]
  (+ (/ (* (- b a) (- v min)) (- max min)) a))

(defn scaleX [x max]
  (scale x xMin xMax (* -1 max) max))

(defn scaleY [y max]
  (scale y yMin yMax (* -1 max) max))

(defn cartesian [x mid]
  (- x mid))

(defn processPoint [[x0 y0]]
  (loop [x x0
         y y0
         iteration 0]
    (if (or (>= iteration 1000)
            (<= 4 (+ (* x x) (* y y))))
      (get-color iteration)
      (let [newX (+ x0 (- (* x x) (* y y)))
            newY (+ y0 (* 2 x y))]
        (recur newX newY (inc iteration))))))

(defn mandelbrot [width height]
  (let [mid-width (/ width 2)
        mid-height (/ height 2)
        scaled-points (for [y (range height)
                            x (range width)]
                        [(scaleX (cartesian x mid-width) mid-width)
                         (scaleY (cartesian y mid-height) mid-height)])]
    (pmap processPoint scaled-points)))

(ns clj-fractal.fractals)

(def xMin -2.5)
(def xMax 1)
(def yMin -1)
(def yMax 1)

(defn scale [v a b min max]
  (+ (/ (* (- b a) (- v min)) (- max min)) a))

(defn scale-x [x max]
  (scale x xMin xMax (* -1 max) max))

(defn scale-y [y max]
  (scale y yMin yMax (* -1 max) max))

(defn cartesian [x mid]
  (- x mid))

(defn process-point [[x0 y0]]
  (loop [x x0
         y y0
         iteration 0]
    (if (or (>= iteration 1000)
            (<= 4 (+ (* x x) (* y y))))
      iteration
      (let [newX (+ x0 (- (* x x) (* y y)))
            newY (+ y0 (* 2 x y))]
        (recur newX newY (inc iteration))))))

(defn create-histogram [points]
  (reduce (fn [m p] (assoc m p (inc (m p 0)))) {} points))

(defn mandelbrot [width height]
  (let [mid-width (/ width 2)
        mid-height (/ height 2)
        scaled-points (for [y (range height)
                            x (range width)]
                        [(scale-x (cartesian x mid-width) mid-width)
                         (scale-y (cartesian y mid-height) mid-height)])]
    (let [points (pmap process-point scaled-points)
          histo (create-histogram points)]
      (map #(histo %) points))))

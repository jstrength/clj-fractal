(ns clj-fractal.fractals)

(def max-iteration 1000)
(def bailout 65536)

(defn scale [v a b min max]
  (+ (/ (* (- b a) (- v min)) (- max min)) a))

(defn scale-x [x max view-port]
  (scale x (second view-port) (nth view-port 3) (* -1 max) max))

(defn scale-y [y max view-port]
  (scale y (nth view-port 2) (first view-port) (* -1 max) max))

(defn cartesian-x [x mid]
  (- x mid))

(defn cartesian-y [y mid]
  (- mid y))

(defn process-point [[x0 y0]]
  (loop [x x0
         y y0
         iteration 0]
    (if (or (>= iteration max-iteration)
            (<= bailout (+ (* x x) (* y y))))
      (if (< iteration max-iteration)
        (-> (Math/sqrt (+ (* x x) (* y y)))
            (Math/log)
            (/ (Math/log 2))
            (Math/log)
            (/ (Math/log 2))
            (#(- (inc iteration) %)))
        iteration)
      (let [newX (+ x0 (- (* x x) (* y y)))
            newY (+ y0 (* 2 x y))]
        (if (and (= x newX) (= y newY))
          max-iteration
          (recur newX newY (inc iteration)))))))

(defn calc-view-port [[x y] dim]
  (let [new-dim (map #(/ % 2) dim)]
    [(+ (first new-dim) y)
     (+ (second new-dim) x)
     (+ (nth new-dim 2) y)
     (+ (nth new-dim 3) x)]))

(defn mandelbrot [screen-dim new-origin dim]
  (let [mid-width (/ (first screen-dim) 2)
        mid-height (/ (second screen-dim) 2)
        origin-x (scale-x (cartesian-x (first new-origin) mid-width) mid-width dim)
        origin-y (scale-y (cartesian-y (second new-origin) mid-height) mid-height dim)
        _ (println [ origin-x origin-y dim])
        view-port (calc-view-port [origin-x origin-y] dim)
        _ (println view-port)
        scaled-points (for [y (range (second screen-dim))
                            x (range (first screen-dim))]
                        [(scale-x (cartesian-x x mid-width) mid-width view-port)
                         (scale-y (cartesian-y y mid-height) mid-height view-port)])]
    {:data (pmap process-point scaled-points)
     :dim view-port}))


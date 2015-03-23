(ns clj-fractal.core
  (:require [quil.core :as q]
            [clj-fractal.fractals :refer :all])
  (:gen-class))

(def width 700)
(def height 400)
(def max-color 100.0)

(defn draw-mandelbrot [data]
  (let [pixels (q/pixels)]
    (doseq [curr (range (* width height))]
      (let [hue (mod (data curr) max-color)]
        (aset pixels curr (q/color hue max-color max-color)))))
  (println "done")
  (q/update-pixels))

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsb max-color)
  (q/set-state! :zoom 1
                :origin [0.0 0.0]))

(defn draw []
  (when (or (= 1 (:zoom @(q/state-atom))) (q/mouse-pressed?))
    (println "clicked! [" (q/mouse-x) (q/mouse-y) "]")
    (println "zoomed by " (:zoom @(q/state-atom)) )
    (let [new-origin-x (if (q/mouse-pressed?) (q/mouse-x) (/ width 2))
          new-origin-y (if (q/mouse-pressed?) (q/mouse-y) (/ height 2))
          brot (time (mandelbrot [width height] [new-origin-x new-origin-y] (:zoom @(q/state-atom)) (:origin @(q/state-atom))))
          data (vec (:data brot))]
      (draw-mandelbrot data)
      (swap! (q/state-atom) assoc
             :origin (:origin brot)
             :zoom (:zoom brot)))))

(defn sketch []
  (q/defsketch fractal
               :title "Fractal"
               :setup setup
               :draw draw
               :size [width height]))

(defn -main [& args]
  (sketch))
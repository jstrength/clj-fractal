(ns clj-fractal.core
  (:require [quil.core :as q]
            [clj-fractal.fractals :refer :all])
  (:gen-class))

(def width 800)
(def height 600)
(def max-color 100.0)

(defn setup []
  (q/frame-rate 1)
  (q/color-mode :hsb max-color)
  (q/no-loop))

(defn draw []
  (let [pixels (q/pixels)
        colors (time (vec (mandelbrot width height)))]
    ;(println colors)
    (doseq [curr (range (* width height))]
      (let [hue (mod (colors curr) max-color)]
        (aset pixels curr (q/color hue max-color max-color))))
    (q/update-pixels)))

(defn sketch []
  (q/defsketch fractal
               :title "Fractal"
               :setup setup
               :draw draw
               :size [width height]))

(defn -main [& args]
  (sketch))
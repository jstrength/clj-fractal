(ns clj-fractal.core
  (:require [quil.core :as q]
            [clj-fractal.fractals :refer :all])
  (:gen-class))

(def width 800)
(def height 600)

(defn setup []
  (q/frame-rate 1)
  (q/color-mode :hsb)
  (q/no-loop))

(defn draw []
  (let [pixels (q/pixels)
        colors (time (vec (mandelbrot width height)))]
    (doseq [curr (range (* width height))]
      (let [color (colors curr)]
        (aset pixels curr (q/color (mod color 360) 300 300))))
    (q/update-pixels)))

(defn sketch []
  (q/defsketch fractal
               :title "Fractal"
               :setup setup
               :draw draw
               :size [width height]))

(defn -main [& args]
  (sketch))
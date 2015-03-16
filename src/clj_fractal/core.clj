(ns clj-fractal.core
  (:require [quil.core :as q]
            [clj-fractal.fractals :refer :all])
  (:gen-class))

(def width 1600)
(def height 800)
(def cell-size 2)
(def x-row-count (/ width cell-size))
(def y-row-count (/ height cell-size))

(defn setup []
  (q/smooth)
  (q/frame-rate 30)
  (q/background 200 200 200))

(defn draw []
  (let [colors (vec (mandelbrot x-row-count y-row-count))]
    (doseq [curr (range (* x-row-count y-row-count))]
      (let [x (* cell-size (mod curr x-row-count))
            y (* cell-size (quot curr x-row-count))]
        (comment (condp = (colors curr)
                   0 (q/fill 0 0 0)                         ;black
                   1 (q/fill 51 102 255)                    ;blue
                   2 (q/fill 255 204 51)                    ;orange
                   3 (q/fill 204 51 255)                    ;purple
                   4 (q/fill 0 255 255)))                   ;teal
        (q/fill (colors curr) (mod (colors curr) 100) (mod (colors curr) 50))
        (q/rect x y cell-size cell-size)))))

(defn sketch []
  (q/defsketch fractal
               :title "Fractal"
               :setup setup
               :draw draw
               :size [width height]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(ns clj-fractal.core
  (:require [quil.core :as q]
            [clj-fractal.fractals :refer :all])
  (:gen-class))

(def width 600)
(def height 600)
(def max-color 100.0)

(defn draw-mandelbrot [data]
  (println "ypu")
  ;(println data)
  (let [pixels (q/pixels)]
    (doseq [curr (range (* width height))]
      (let [hue (mod (data curr) max-color)]
        (aset pixels curr (q/color hue max-color max-color)))))
  (println "done")
  (q/update-pixels)
  (println "done1")
  )

(defn setup []
  (q/frame-rate 1)
  (q/color-mode :hsb max-color)
  (q/set-state! :dim [4 -4 -4 4])
  ;(draw-mandelbrot (time (vec (:data (mandelbrot [width height] [(/ width 2) (/ height 2)] @(q/state-atom))))))
  )

(defn draw []
  (when (q/mouse-pressed?)
    (println (q/pmouse-x) " - " (q/pmouse-y))
    (println (q/mouse-x) " - " (q/mouse-y))
    (let [brot (mandelbrot [width height] [(/ width 2) (/ height 2)] (:dim @(q/state-atom)))
          data (time (vec (:data brot)))]
      (draw-mandelbrot data)
      (swap! (q/state-atom) assoc :dim (:dim brot)))))

(defn sketch []
  (q/defsketch fractal
               :title "Fractal"
               :setup setup
               :draw draw
               :size [width height]))

(defn -main [& args]
  (sketch))
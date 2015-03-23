(ns clj-fractal.core
  (:require [quil.core :as q]
            [clj-fractal.fractals :refer :all])
  (:gen-class))

(def width 350)
(def height 200)
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
  (q/frame-rate 60)
  (q/color-mode :hsb max-color)
  (q/set-state! :dim [1.0M -2.5M -1.0M 1.0M]
                :origin [0.0M 0.0M])
  ;(draw-mandelbrot (time (vec (:data (mandelbrot [width height] [(/ width 2) (/ height 2)] @(q/state-atom))))))
  )

(defn draw []
  (when (q/mouse-pressed?)
    (println (q/pmouse-x) " - " (q/pmouse-y))
    (println (q/mouse-x) " - " (q/mouse-y))
    (let [brot (mandelbrot [width height] [(q/mouse-x) (q/mouse-y)] (:dim @(q/state-atom)) (:origin @(q/state-atom)))
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
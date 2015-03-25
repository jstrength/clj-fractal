(ns clj-fractal.core
  (:require [quil.core :as q]
            [clj-fractal.fractals :refer :all])
  (:gen-class))

(def width 400)
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
  (q/color-mode :hsb max-color)
  (q/no-loop)
  (q/set-state! :zoom 1
                :origin [0.0 0.0]))

(defn draw []
  (println "clicked! [" (q/mouse-x) (q/mouse-y) "]")
  (println "zoomed by " (:zoom @(q/state-atom)))
  (let [new-origin-x (if (= 1 (:zoom @(q/state-atom))) (/ width 2) (q/mouse-x))
        new-origin-y (if (= 1 (:zoom @(q/state-atom))) (/ height 2) (q/mouse-y))
        brot (time (mandelbrot [width height] [new-origin-x new-origin-y] (:zoom @(q/state-atom)) (:origin @(q/state-atom))))
        data (vec (:data brot))]
    (time (draw-mandelbrot data))
    (swap! (q/state-atom) assoc
           :origin (:origin brot)
           :zoom (:zoom brot))))

(defn zoom-status [options]
  (let [draw (:draw options (fn []))]
    (assoc options :draw (fn []
                           (let [zoom (:zoom @(q/state-atom))]
                             (draw)
                             (q/fill 0)
                             (q/text (str zoom "x Zoom") 0 10))))))

(defn sketch []
  (q/defsketch fractal
               :title "Fractal"
               :setup setup
               :draw draw
               :mouse-clicked q/redraw
               :size [width height]
               :middleware [zoom-status]))

(defn -main [& args]
  (sketch))
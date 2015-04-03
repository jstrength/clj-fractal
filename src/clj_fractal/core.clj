(ns clj-fractal.core
  (:require [quil.core :as q]
            [clj-fractal.fractals :refer :all])
  (:gen-class))

(def width 800)
(def height 800)
(def max-color 100.0)
(def ZOOM 8)

(defn draw-mandelbrot [data]
  (doseq [x (range width)
          y (range height)]
    (let [hue (mod (data (+ x (* width y))) max-color)]
      (q/set-pixel x y (q/color hue max-color max-color))))
  (println "done"))

(defn setup []
  (q/color-mode :hsb max-color)
  (q/no-loop)
  (q/set-state! :zoom 1
                :origin [0.0 0.0]
                :click-origin [(/ width 2) (/ height 2)]
                :prev-states []))

(defn pop-state []
  (if-let [prev-state (peek (:prev-states @(q/state-atom)))]
    (swap! (q/state-atom) assoc
           :zoom (:zoom prev-state)
           :origin (:origin prev-state)
           :click-origin (:click-origin prev-state)
           :prev-states (pop (:prev-states @(q/state-atom))))))

(defn push-state []
  (swap! (q/state-atom) assoc
         :prev-states (conj (:prev-states @(q/state-atom)) (dissoc @(q/state-atom) :prev-states :new-origin))))

(defn draw []
  (println "clicked! [" (q/mouse-x) (q/mouse-y) "]")
  (condp = (q/mouse-button)
    :left (do (push-state)
              (swap! (q/state-atom) assoc
                     :zoom (* ZOOM (:zoom @(q/state-atom)))
                     :origin (:new-origin @(q/state-atom))
                     :click-origin [(q/mouse-x) (q/mouse-y)]))
    :right (pop-state)
    nil)

  (println "Origin before - " (:origin @(q/state-atom)))
  (let [brot (time (mandelbrot [width height]
                               (:origin @(q/state-atom))
                               (:click-origin @(q/state-atom))
                               (:zoom @(q/state-atom))
                               ZOOM))]
    ;(time (draw-mandelbrot (vec (:data brot))))
    (println "Origin after - " (:origin brot))
    (swap! (q/state-atom) assoc :new-origin (:origin brot))))

(defn zoom-status [options]
  (let [draw (:draw options (fn []))]
    (assoc options :draw (fn []
                           (draw)
                           (q/fill 0)
                           (q/text (str (:zoom @(q/state-atom)) "x Zoom") 0 (q/text-ascent))))))

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
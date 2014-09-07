(ns nbody.render
  (:require [quil.core :as q]
            [nbody.sim :as s]))

(def screen-w 640)
(def screen-h 640)
(def screen-center [(/ screen-w 2.0) (/ screen-h 2.0)])


(def yellow [255 255 0])
(def yellow-orange [225 175 0])
(def light-blue [100 100 255])
(def red [255 75 0])
(def blue-green [0 200 150])
(def yellow-green [125 255 0])
(def white [255 255 255])

(def colors
  {:sun yellow
   :jupiter yellow-orange
   :saturn yellow-orange
   :earth light-blue
   :mars red
   :uranus blue-green
   :neptune blue-green
   :mercury yellow-green
   :venus white})

(defn setup []
  (q/no-stroke)
  (q/fill 100 100 255)
  (q/frame-rate 120))

(defn universe->screen [ux uy]
  (let [scale (* 2.0 screen-w (/ 1.0 1.0e13))]
    [(* scale ux) (* scale uy)]))

(def bodies (atom s/sol-particles))

(defn render-body [body]
  (let [{name :name [ux uy] :position mass :mass} body
        [sx sy] (universe->screen ux uy)
        screen-size (* screen-w 0.002 (- (Math/log10 mass) 22.5))]
    (q/push-matrix)
    (apply q/fill (colors name))
    (q/translate sx sy)
    (q/sphere screen-size)
    (q/pop-matrix)))

(defn draw []
  (q/lights)
  (q/background 0)
  (q/no-stroke)
  (apply q/translate (conj screen-center -1200))

  (doseq [b (swap! bodies s/inc-time (* 5 s/day))]
    (render-body b)))

(q/defsketch solar-system
  :title "Solar System"
  :setup setup
  :draw draw
  :renderer :p3d
  :size [screen-w screen-h])

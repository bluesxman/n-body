(ns nbody.render
  (:require [quil.core :as q]
            [nbody.sim :as s]))

(def screen-w 1300)
(def screen-h 700)
(def screen-center [(/ screen-w 2.0) (/ screen-h 2.0)])
(def timestep (* 1 s/day))
(def mouse-sensitivity (/ Math/PI screen-h 2.0))
(def wheel-sensitivity 0.05)

(def yellow [255 255 0])
(def yellow-orange [225 175 0])
(def blue [0 0 255])
(def red [255 75 0])
(def blue-green [0 200 150])
(def yellow-green [125 255 0])
(def white [255 255 255])

(def colors
  {:sun yellow
   :jupiter yellow-orange
   :saturn yellow-orange
   :earth blue
   :mars red
   :uranus blue-green
   :neptune blue-green
   :mercury yellow-green
   :venus white})

(defn setup []
  (q/no-stroke)
  (q/fill 100 100 255)
  (q/frame-rate 120))

(def universe-scale (* 4.0 screen-w (/ 1.0 1.0e13)))
(defn universe->screen
  ([u]
   (* universe-scale u))
  ([ux uy]
   [(* universe-scale ux) (* universe-scale uy)]))

(def bodies (atom s/sol-particles))

(defn render-body [body]
  (let [{name :name [ux uy] :position mass :mass} body
        [sx sy] (universe->screen ux uy)
        screen-size (* screen-w 0.002 (- (Math/log10 mass) 22.5))]
    (q/push-matrix)
    (q/no-stroke)
    (apply q/fill (colors name))
    (q/translate sx sy)
    (q/sphere screen-size)
    (q/pop-matrix)))

(defn render-orbit [body]
  (let [{uradius :radius} body
        diam (* 2 (universe->screen uradius))]
    (q/push-matrix)
    (q/stroke 100 255 255 25)
    (q/no-fill)
    (q/ellipse 0 0 diam diam)
    (q/pop-matrix)))

(def pitch (atom 180)) ;; quil is left-handed; start +y-axis up
(def yaw (atom 0))

(defn update-rot [rot0 pixels]
  (+ rot0 (* mouse-sensitivity pixels)))

(defn handle-drag []
  (let [dx (- (q/mouse-x) (q/pmouse-x))
        dy (- (q/mouse-y) (q/pmouse-y))]
    (swap! pitch update-rot dy)
    (swap! yaw update-rot dx)
    nil))

(def zoom (atom 1))
(defn handle-wheel [wheel-rot]
    (swap! zoom #(+ % (* wheel-rot wheel-sensitivity))))

(defn draw []
  (q/lights)
  (q/background 0)
  (q/no-stroke)

  (apply q/translate (conj screen-center (min 500 (* -2500 @zoom))))
  (q/rotate-x @pitch)
  (q/rotate-y @yaw)

  (doseq [b (swap! bodies s/inc-time timestep)]
    (render-orbit b)
    (render-body b)))

(defn run []
  (q/sketch
   :title "Solar System"
   :setup setup
   :draw draw
   :renderer :p3d
   :size [screen-w screen-h]
   :mouse-dragged handle-drag
   :mouse-wheel handle-wheel))

(run)

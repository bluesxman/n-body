(ns nbody.render
  (:require [quil.core :as q]
            [nbody.sim :as s]))

(def screen-w 640)
(def screen-h 640)
(def screen-center [(/ screen-w 2.0) (/ screen-h 2.0)])

(defn setup []
  (q/no-stroke)
  (q/fill 100 100 255))

(defn universe->screen [ux uy]
  (let [scale (* 2.0 screen-w (/ 1.0 1.0e13))]
    [(* scale ux) (* scale uy)]))

(apply universe->screen (get-in s/sol-particles [:neptune :position]))

(defn render-body [body]
  ;; scale universe to screen
  ;; translate body to position
  ;;
  (let [[_ {[ux uy] :position}] body
        [sx sy] (universe->screen ux uy)]
    (q/push-matrix)
    (q/translate sx sy)
    (q/sphere (* 0.01 screen-w))
    (q/pop-matrix)))

(defn draw []
  (q/lights)
  (q/background 0)
  (q/no-stroke)
  (q/fill 100 100 255)
  (apply q/translate (conj screen-center -2000))

  (doseq [b s/sol-particles]
    (render-body b))

;;   (q/sphere (* 0.2 screen-w))
  )


;; (defn draw []
;;   (q/lights)
;;   (q/background 0)
;;   (q/no-stroke)
;;   (q/fill 100 100 255)

;;   (apply q/translate (conj screen-center -2000))
;;   (q/push-matrix)
;;   (q/rotate-x 0.25)
;;   (q/rotate-y 0.65)
;;   (q/box (* 0.2 screen-w))
;;   (q/pop-matrix)

;;   (q/translate [300 -700 900])
;;   (q/sphere (* 0.2 screen-w)))

(q/defsketch solar-system
  :title "Solar System"
  :setup setup
  :draw draw
  :renderer :p3d
  :size [screen-w screen-h])

(ns nbody.sim
  (:require [nbody.physics :refer :all]))


(defn net-gravity [p1 particles]
  (let [{m1 :mass p1 :position} p1
        forces (for [{m2 :mass p2 :position} particles
                     :when (not= p1 p2)]
                 (gravity m1 m2 p1 p2))]
    (reduce vadd forces)))

(def day (* 24 60 60))

(defn inc-time [particles time-step]
  (for [particle0 particles
        :let [grav (net-gravity particle0 particles)
              {mass :mass vel0 :velocity pos0 :position} particle0
              new-vel (velocity mass vel0 grav time-step)
              new-pos (position pos0 new-vel time-step)]]
    (assoc particle0 :position new-pos :velocity new-vel)))

(def sol-system
  (list
   {:name :sun     :mass 1.9880e30 :speed     0 :semi-major          0.0}
   {:name :mercury :mass 3.3022e23 :speed 47362 :semi-major   57909050e3}
   {:name :venus   :mass 4.8676e24 :speed 35020 :semi-major  108208000e3}
   {:name :earth   :mass 5.9721e24 :speed 29780 :semi-major  149598261e3}
   {:name :mars    :mass 6.4185e23 :speed 24077 :semi-major  227939100e3}
   {:name :jupiter :mass 1.8986e27 :speed 13070 :semi-major  778547200e3}
   {:name :saturn  :mass 5.6846e26 :speed  9690 :semi-major 1433449370e3}
   {:name :uranus  :mass 8.6810e25 :speed  6810 :semi-major 2870671400e3}
   {:name :neptune :mass 1.0243e26 :speed  5430 :semi-major 4498542600e3}))

(defn body->particle [{name :name mass :mass speed :speed smaj :semi-major}]
  (let [radius smaj  ;; fudge by treating ellipse as circle
        phi (- (* Math/PI 2 (rand)) Math/PI)  ;; vary from -PI to PI
        vel [(- (* speed (Math/sin phi))) (* speed (Math/cos phi))] ;; counter-clockwise
        pos [(* radius (Math/cos phi)) (* radius (Math/sin phi))]]
    {:name name :mass mass :velocity vel :position pos :radius radius}))

(def sol-particles
  (into (list) (map body->particle sol-system)))

(ns nbody.sim
  (:require [nbody.physics :refer :all]))


(defn net-gravity [p1 particles]
  (let [{m1 :mass p1 :position} p1
        forces (for [{m2 :mass p2 :position} particles
                     :when (not= p1 p2)]
                 (gravity m1 m2 p1 p2))]

;;     forces
    (reduce vadd forces)
    ))

(net-gravity (first sol-particles) sol-particles)

(defn accel-particle [particle force dt]
  (let [{mass :mass v0 :velocity} particle]
    (assoc particle :velocity (velocity mass v0 force dt))))

;; for each particle
;; for each other particle
;; sum the force of gravity of the second particle on the first
;; modify the first particle's velocity by applying the net force to its mass
(defn accelerate [particles dt]
  (map #(accel-particle % (net-gravity % particles) dt) particles))

(map #(net-gravity % sol-particles) sol-particles)
(accel-particle (first sol-particles) '(-1.3910009732229538E20 -9.459471068622453E19) day)

(defn move-particle [particle dt]
  (let [{p0 :position vel :velocity} particle]
    (assoc particle :position (position p0 vel dt))))

;; for each particle
;; move the particle by its velocity vector for an amount of time
(defn translate [particles dt]
    (map #(move-particle % dt) particles))

;; Repeat:
;; render all particles
;; For each particle
;; accelerate by net force of gravity on that particle
;; translate position by new velocity
;; (defn run [particles timestep]
;;   (render particles)
;;   (let [next-particles (->             ;; translate(accelerate(particles dt), dt)
;;                          particles
;;                          (accelerate timestep)
;;                          (translate timestep))]
;;     (recur next-particles timestep)))

(def day (* 24 60 60))
(def time-step (* 1 day))

(defn inc-time [particles timestep]
  (->
   particles
   (accelerate timestep)
   (translate timestep)))

(inc-time sol-particles day)
(accelerate sol-particles day)

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
  (let [radius (* smaj 2)  ;; fudge by treating ellipse as circle
        phi (- (* Math/PI 2 (rand)) Math/PI)  ;; vary from -PI to PI
        vel [(- (* speed (Math/sin phi))) (* speed (Math/cos phi))] ;; counter-clockwise
        pos [(* radius (Math/cos phi)) (* radius (Math/sin phi))]]
    {:name name :mass mass :velocity vel :position pos}))

(def sol-particles
  (into (list) (map body->particle sol-system)))

(seq sol-particles)

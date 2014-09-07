(ns nbody.physics)

;;
;; Vector math helpers
;;
(defn vadd [v1 v2]
  (map + v1 v2))

(defn vsub [v1 v2]
  (map - v1 v2))

(defn scale [v scalar]
  (map #(* % scalar) v))

(defn vdiv [v1 v2]
  (map / v1 v2))

(defn mag-squared [v]
  (apply + (map #(* % %) v)))

(defn magnitude [v]
  (Math/sqrt (mag-squared v)))

(defn vunit [v]
  (scale v (/ 1 (magnitude v))))


;;
;; Simplistic physics
;;
(def grav-const 6.67384e-11)

(defn position [pos-initial vel delta-t]
  (vadd pos-initial (scale vel delta-t)))

(defn velocity [mass vel-initial net-force delta-t]
  ;; v = F * dt / m + v0
  (vadd (scale net-force (/ delta-t mass)) vel-initial))

(defn gravity
  "force vector pointing from p1 to p2"
  [m1 m2 p1 p2]
  (let [dist-vec (vsub p2 p1)
        f-scalar (/ (* grav-const m1 m2) (mag-squared dist-vec))
        direction (vunit dist-vec)]
    (scale direction f-scalar)))

(defn net-force [particle universe]
  (let [{m1 :mass p1 :position} particle
        net-grav (fn [{m2 :mass p2 :position}]
                   (if (= p1 p2)
                     0
                     (gravity m1 m2 p1 p2)))]
    (apply vadd (map net-grav universe))))


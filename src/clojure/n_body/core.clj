(ns n-body.core)

(defrecord particle [id color mass position velocity]) ;; position and velocity are signals, i.e. values that change over time

(def grav-const 6.67384e-11)

(defn vadd [v1 v2]
  (map + v1 v2))

(defn vsub [v1 v2]
  (map - v1 v2))

(defn scale [v scalar]
  (map #(* % scalar) v))

(defn vdiv [v1 v2]
  (map / v1 v2))

(defn magnitude [v]
  (Math/sqrt (apply + (map #(* % %) v))))

(defn vunit [v]
  (scale v (/ 1 (magnitude v))))

(defn velocity [mass vel-initial net-force delta-t]
  ;; v = F * dt / m + v0
  (vadd (vdiv (scale net-force delta-t) mass) vel-initial))

;; force vector pointing from m1 to m2
(defn gravity [m1 m2 p1 p2]
  (let [dist-vec (vsub p2 p1)
        distance (magnitude dist-vec)
        f-scalar (/ (* grav-const m1 m2) (* distance distance))
        direction (vunit dist-vec)]
    (scale direction f-scalar)))

(defn net-force [particle universe]
  (let [{m1 :mass p1 :position} particle
        net-grav (fn [{m2 :mass p2 :position}]
                   (gravity m1 m2 p1 p2))]
    (apply + (map net-grav universe))))




;; (def universe [t] (map #(% t) particles))

;; (defrecord particle [id color mass position velocity]) ;; position and velocity are signals

;; (defn create-velocity [init-time init-vel mass net-force])

;; (defn create-position [pos vel]
;;   (fn [t]
;;     (let [delta (map #(* t %) vel)
;;           pos-new ()]
;;       ())))

;; (defn create-particle [id color mass init-pos init-vel]
;;   (fn ))

;; (defprotocol particle [t]
;;   [p-new, [pos, vel]])

;; ;; signal takes time and returns a new version of itself and an object-array
;; (defprotocol signal [t]
;;   [new-signal, yield])


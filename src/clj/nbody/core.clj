(ns nbody.core
;;   (:require [clojure.string :refer :all]))
  (:require [clojure.core.async :refer [chan pub sub]]))

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

(defn magnitude [v]
  (Math/sqrt mag-squared))

(defn mag-squared [v]
  (apply + (map #(* % %) v)))

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
  (vadd (vdiv (scale net-force delta-t) mass) vel-initial))

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
                   (gravity m1 m2 p1 p2))]
    (apply + (map net-grav universe))))

;;
;; Simulation in FRP style
;;
(defrecord particle [id color mass position velocity]) ;; position and velocity are signals, i.e. values that change over time

(def clock (repeat 1))

(defn position-sig [ts pos vel]
  (map #(position pos vel %) ts))

(position-sig clock [0 0 0] [0 0 20])


;; pos-sig: t -> [pos, pos-sig] // recursive type
(defn create-pos-sig [init-pos init-vel init-time]
  (fn this
    ([t] (if (= t init-time)  ;; Take advantage of this closure caching a position
           [init-pos, this]   ;; avoid creating extra lambda
           (let [[vel, vel-sig] (init-vel t)
                 delta-t (- t init-time)
                 pos (position init-pos vel delta-t)]
             [pos (create-pos-sig pos vel-sig t)])))))

(defn create-const-vel-sig [vel]
  (fn this
    ([t] [vel this])))

(def vsig (create-const-vel-sig[1 2 3]))
(def psig (create-pos-sig [0 0 0] vsig 0))

(let [[p1 psig1] (psig 5)]
  (psig1 7))


(defn lift [fun sig]
  (fn [t]
    (let [[value next-sig] (sig t)]
      [(fun value) (lift fun next-sig)])))


;; time is an event stream.  when time changes all dependencies should be updated.
;; dependencies should subscribe to the time channel.  clock thread which creates
;; the events should publish them to the channel.
;;
;; In effect, signals are channels and lifting a function on a channel creates a
;; subscription using the fn that was passed.

;; choice:  lazy or update all on event

;; event = time -> Some('a)  // If the value of a signal changed at that time, then some 'a else none
;; signal = time -> 'a       // always has a value at a time.  Should it be lazy?  Should it remember?  Should it block if in future?

;; functions take input streams and when a value arrives via that stream the function is immediately evaluated to compute a new output

;; event = instantanous
;; signal = continuous

;; frp limits re-evaluation.  if the signal doesnt change then no things depending on that signal should be re-computed
;; frp avoids polling.  it uses a publish-subscribe model.  changes are pushed to listeners rather than them polling for changes
;; frp uses mutability to efficiently multicast:  eval once and share a ref

;; seems frp retains only the history that is still needed to compute "now" and lets GC clean up history not being used
;; for example, new position depends on velocity now, last position, and delta time therefore most prior positions and velocities
;; would not have a reference and could be GC'd

;; Rx has the scan() method.  Acts similarly to reduce.  Would be useful for representing a position over time
;; given an initial position and velocity signal
























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


(ns nbody.signals)

(defprotocol signal
  (defn value [])
  (defn subscribe []))

(defn create-clock [interval])


;; position is a signal of three signals:
;; its last value, velocity, and delta-time
;; they are composed as follows:
;; (+ last-value (* velocity delta-time))

;; velocity is a signal of three signals
;; its last value, net force, and delta-time
;; it also depends on t particles mass which is constant
;; they are composed as follows:
;; (+ last-value (/ (* net-force delta-time) mass))

;; net-force is a signal of a set of signals
;; the set of signals is the force gravity of each particle on the particle in question
;; they are composed as follows:
;; (reduce + forces)

;; the force of gravity is a signal of two signals and two constants:
;; the positions of two particles and their masses
;; they are composed as follows:
;;


f = ma
f = m (v1 - v0) / dt
f dt / m = v1 - v0
v0 + f dt / m = v1

(defn lift
  "Create a new signal which takes values from the input signals and
  passes them as arguments to f"
  [sig1 sig2 sig3 f]
    ())


(ns clj-ray.input
  (:import [java.awt.event KeyListener FocusListener MouseListener MouseMotionListener]))

(def key-limit 65536)

(defn valid-key
  [code]
  (and (pos? code) (< code key-limit)))

(defn key-handler
  [inputs e swap-fn]
    (let [code (.getKeyCode e)]
      (if (valid-key code)
        (do (swap! inputs #(swap-fn % code))
            nil)
        nil)))

(defn key-pressed
  [inputs e]
  (let [swap-fn (fn [key-codes code] (conj key-codes [code true]))]
    (key-handler inputs e swap-fn)))

(defn key-released
  [inputs e]
  (let [swap-fn (fn [key-codes code] (dissoc key-codes code))]
    (key-handler inputs e swap-fn)))

(defn lost-focus
  [inputs e]
  (do (reset! inputs {})
      (println "lost focus")))

(defn create-handler
  [inputs]
  (let [inputs (atom {})
        handler (proxy [KeyListener FocusListener] []
                  (keyTyped [_] nil)
                  (focusLost [e] (lost-focus inputs e))
                  (keyPressed [e] (key-pressed inputs e))
                  (keyReleased [e] (key-released inputs e))
                  (focusGained [e] (println "focus gained")))]
    {:inputs inputs :handler handler}))

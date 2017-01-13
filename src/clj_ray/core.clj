(ns clj-ray.core
  (:gen-class)
  (:use [clj-ray.math])
  (:require [clj-ray.render :as r])
  (:import [java.awt.image BufferedImage]
           [java.io File]
           [java.awt.event KeyEvent]
           [javax.imageio ImageIO]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def level1
  [[1 1 1 1 1 1 1 1 1]
   [1 3 2 0 0 0 2 3 1]
   [1 0 0 0 0 0 0 0 1]
   [1 3 0 0 0 0 0 3 1]
   [1 3 0 0 0 0 0 3 1]
   [1 0 0 0 0 0 0 0 1]
   [1 3 0 0 0 0 0 3 1]
   [1 0 0 0 0 0 0 0 1]
   [1 3 0 0 0 0 0 3 1]
   [1 0 0 0 0 0 0 0 1]
   [1 3 0 0 0 0 0 3 1]
   [1 0 2 0 0 0 2 0 1]
   [1 1 1 1 1 1 1 1 1]])

(def rotate-angle (toRad 10))
(def moving-scale 1)

(defn rotate-player
  [player angle]
  (-> player
      (update-in [:dir] rotate angle)
      (update-in [:pla] rotate angle)))

(defn move-player
  [player delta]
  (update-in player [:pos] add delta))

(defn move-forward
  [{dir :dir :as player}]
  (let [delta (scale dir moving-scale)]
    (move-player player delta)))

(defn move-backwards
  [{dir :dir :as player}]
  (let [delta (scale dir (* -1 moving-scale))]
    (move-player player delta)))

(defn move-right
  [{pla :pla :as player}]
  (let [delta (scale pla moving-scale)]
    (move-player player delta)))

(defn move-left
  [{pla :pla :as player}]
  (let [delta (scale pla (* -1 moving-scale))]
    (move-player player delta)))

(defn turn-left
  [player]
  (rotate-player player (* -1 rotate-angle)))

(defn turn-right
  [player]
  (rotate-player player rotate-angle))

(def action-map
  {KeyEvent/VK_UP move-forward
   KeyEvent/VK_DOWN move-backwards
   KeyEvent/VK_LEFT turn-left
   KeyEvent/VK_RIGHT turn-right})

(defn foo-bar
  [player inputs]
  (let [reducer (fn [player [code action]] (if (inputs code) (action player) player))]
    (reduce reducer player action-map)))

(defn update-player
  [{{inputs :inputs} :screen :as game}]
  (let [input @inputs]
    (update-in game [:player] foo-bar input)))

(defn create-player
  [x y fov]
  {:pos (point x y)
   :dir (vect -1 0)
   :pla (vect 0 0.75) ;; TODO - calculate plane in base of FOV
   :fov fov})

(defn create-game
  []
  {:screen (r/create-window (r/create-canvas 512 512))
   :player (create-player 5 5 70)
   :level  level1
   :now    (System/nanoTime)
   :elapse 0})

(defn tick
  [game now elapsed]
  (let [game-now (-> game (conj [:now now] [:elapsed elapsed]))
        updated  (update-player game-now)
        rendered (r/render-buffer updated)]
    rendered))

(defn wait-tick
  [game now elapsed]
  (Thread/sleep 1) ;; TODO - should wrap this in a try/catch
  game)

(defn game-loop
  [{before :now :as game}]
  (let [now       (System/nanoTime)
        elapsed   (max 0 (- now before)) ;; make elapsed between 0-100000000
        ticked    ((if (> elapsed 100000000) tick wait-tick) game now elapsed)]
    (recur ticked)))

(defn start-game
  "Start a new game"
  []
  (let [game (create-game)
        runn (proxy [Runnable] []
              (run [] (game-loop game)))
        t    (proxy [Thread] [runn])
        _    (.start t)]
    t))

(def screen (r/create-screen 10 10))
(def big-screen (r/create-screen 512 512))
(def big-canvas (r/create-canvas 512 512))
(def player (create-player 5 5 70))
(defn test-window [] (r/create-window big-canvas))
(defn test [] (r/cast-rays screen player level1))
(defn test2 [] (r/render-info screen player level1))
(defn test3 [] (r/render-frame big-screen player level1))

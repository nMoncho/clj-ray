(ns clj-ray.render
  (:gen-class)
  (:use [clj-ray.math])
  (:require [clj-ray.input :as i])
  (:import [java.awt BorderLayout Canvas Color Dimension Graphics]
           [java.awt.image BufferedImage BufferStrategy DataBufferInt]
           [java.io File]
           [javax.imageio ImageIO]
           [javax.swing JFrame JPanel]))

(defn map-get [level x y] ((level y) x))

(defn get-color
  [c]
  (condp = c
    1 0xff0000
    2 0x00ff00
    3 0x0000ff
    4 0x00ffff
    0x333333))

(defn create-buffer-strategy
  [{canvas :canvas :as screen}]
  (if-let [bs (.getBufferStrategy canvas)]
    (conj screen [:bs bs])
    (do (.createBufferStrategy canvas 3)
        (Thread/sleep 1)
        (create-buffer-strategy screen))))

(defn create-window
  "Creates a new frame to host the game (Swing component)"
  [{canvas :canvas :as screen}]
  (let [frame (JFrame. "Raycaster")
        panel (JPanel. (BorderLayout.))
        _     (.add panel canvas)]
    (doto frame
      (.setContentPane panel)
      (.pack)
      (.setLocationRelativeTo nil)
      (.setResizable false)
      ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE) ;; Enable this on JAR, for REPL no.
      (.setVisible true))
    (create-buffer-strategy screen)))

(defn create-screen
  "Creates a new screen"
  [width height]
  (let [img    (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        pixels (.getData (.getDataBuffer (.getRaster img)))]
    {:w      width
     :h      height
     :img    img
     :pixels pixels}))

(defn create-canvas
  [width height]
  (let [buffer (create-screen width height)
        dimens (Dimension. width height)
        {handl  :handler
         inputs :inputs} (i/create-handler)
        canvas (proxy [Canvas] [])
        canvas (doto canvas
                 (.setSize dimens)
                 (.setMaximumSize dimens)
                 (.setMinimumSize dimens)
                 (.addKeyListener handl)
                 (.addFocusListener handl))]
    (conj buffer [:canvas canvas] [:inputs inputs])))

(defn steps
  "Creates the step direction vectors"
  [{width :w} {plane :pla dir :dir}]
  (let [hwidth (int (/ width 2))
        step   (/ (length plane) width)
        cols   (range (- hwidth) hwidth)]
    (map #(add dir (scale plane (* step %))) cols))) ;; make this more readable

(defn initial-step
  [ray-dir ray-pos map-pos delta]
  (if (neg? ray-dir)
    (let [step -1
          side (* delta (- ray-pos map-pos))]
      {:step step :side side})
    (let [step 1
          side (* delta (+ map-pos 1.0 (- ray-pos)))]
      {:step step :side side})))

(defn calc-step
  [ray-dir ray-pos map-pos delta]
  (let [calcul #(apply initial-step (map % [ray-dir ray-pos map-pos delta]))
        {step-x :step side-x :side} (calcul :x)
        {step-y :step side-y :side} (calcul :y)]
    {:step (point step-x step-y) :side (point side-x side-y)}))

(defn step-level
  [{map-x :x map-y :y} {step-x :x step-y :y} {side-x :x side-y :y} {delta-x :x delta-y :y}]
;;  (println map-x map-y step-x step-y side-x side-y delta-x delta-y)
  (if (< side-x side-y)
    {:side 0 :map-pos (point (+ map-x step-x) map-y) :side-dist (point (+ side-x delta-x) side-y)}
    {:side 1 :map-pos (point map-x (+ map-y step-y)) :side-dist (point side-x (+ side-y delta-y))}))

(defn stepper
  [map-pos step side-dist delta level]
  (let [nstep  (step-level map-pos step side-dist delta)
        {:keys [side map-pos side-dist]}  nstep
        {map-x :x map-y :y}  map-pos
        cell (map-get level map-x map-y)]
    (if (zero? cell)
      (stepper map-pos step side-dist delta level) ;; recur
      (conj nstep [:color (get-color cell)] [:step step]))))

(defn calc-ray-dir
  [dir plane camera-x]
  (let [scaled (scale plane camera-x)]
    (add dir scaled)))

(defn calc-delta
  [{ray-dir-x :x ray-dir-y :y}]
  (let [delta-x   (sqrt (+ 1 (/ (pow2 ray-dir-y) (spow2 ray-dir-x))))
        delta-y   (sqrt (+ 1 (/ (pow2 ray-dir-x) (spow2 ray-dir-y))))]
    (point delta-x delta-y)))

(defn cast-ray
  [col width {pos :pos dir :dir plane :pla} level]
  ;;(println "Casting ray for column:" col)
  (let [camera-x  (- (/ (* 2 col) (double width)) 1)
        ray-pos   pos
        map-pos   (trunc ray-pos)
        ray-dir   (calc-ray-dir dir plane camera-x)
        delta     (calc-delta ray-dir)
        {step :step
         side :side} (calc-step ray-dir ray-pos map-pos delta)
        hit       (stepper map-pos step side delta level)]
    (conj hit [:ray-dir ray-dir] [:ray-pos ray-pos] [:col col]))) ;; ray-pos should not be needed since it's the player's position

(defn cast-rays
  [{width :w} player level]
  (let [colums (range width)]
    (map #(cast-ray % width player level) colums)))

(defn perp-wall-dist
  [map-pos ray-pos step ray-dir]
  (let [a (/ (- 1 step) 2)
        b (- map-pos ray-pos)
        c (+ a b)]
    (/ c ray-dir)))

(defn perspective-ray
  [{side :side map-pos :map-pos ray-pos :ray-pos step :step ray-dir :ray-dir :as hit}]
  (let [key (if (zero? side) :x :y)
        args (map key [map-pos ray-pos step ray-dir])
        perp (apply perp-wall-dist args)]
    (conj hit [:perp-wall perp])))

(defn calc-line
  [hit {height :h}]
  (let [{perp-wall :perp-wall} (perspective-ray hit)
        line-height (int (/ height perp-wall))
        hheight     (/ height 2)
        draw-start  (max 0 (+ (/ line-height -2) hheight))
        draw-end    (min height (+ (/ line-height 2) hheight))]
    (conj hit [:slide {:start draw-start :end draw-end}])))

(defn perspective-rays
  [screen rays]
  (map #(calc-line % screen) rays))

(defn render-info
  [screen player level]
  (let [rays  (cast-rays screen player level)
        prays (perspective-rays screen rays)]
    prays))

(defn render-column
  [{pixels :pixels width :w} {color :color {start :start end :end} :slide col :col side :side}]
  (let [start-idx (int start)
        end-idx   (int end)
        color     (if (zero? side) color (int (/ color 2)))
        strip-idx (map #(+ col (* width %)) (range start-idx end-idx))]
    (doall (map #(aset-int pixels % color) strip-idx)))) ;; side effect (how do I remove this two ``doall``)

(defn render-frame
  [{img :img pixels :pixels :as screen} player level]
  (let [file    (File. "frame.png")
        info    (render-info screen player level) ;; TODO - need to clear the buffer to get a clean frame
        columns (map #(render-column screen %) info)
        inflate (doall columns)]
    (ImageIO/write img "png" file)))

(defn render-buffer
  [{{img :img bs :bs w :w h :h px :pixels :as screen} :screen player :player level :level :as game}]
  (let [info    (render-info screen player level) ;; TODO - need to clear the buffer to get a clean frame
        _ (java.util.Arrays/fill px 0)
        columns (map #(render-column screen %) info)
        inflate (doall columns)
        graphic (.getDrawGraphics bs)]
    (doto graphic
      (.setColor Color/black)
      (.fillRect 0 0 w h)
      (.drawImage img 0 0 w h nil)
      (.dispose))
    (.show bs)
    game))

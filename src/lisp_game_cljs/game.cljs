(ns lisp-game-cljs.game
  (:require [testdouble.cljs.csv :as csv]
            [cljs.reader]
            [cljs.core.async :refer [go chan >! <!]]))

(def lastlevel 5)

(def robot-pos (atom {:x 1 :y 1}))
(def ctx2d (atom nil))
(def current-level (atom 1))
(def tile-size 48) ; pixels, each tile is 16x16px upscaled *3



(declare level-collision-map!)

(defn check-inc [n]
  (if (< n lastlevel)
    (inc n)
    n))

(defn ^:private set-context! [ctx]
  (reset! ctx2d ctx))

(defn ^:private draw-level! []
  (let [img (new js/Image)]
    (aset img "src" (str "assets/levels/level" @current-level ".png"))
    (.drawImage @ctx2d img 0 0 528 528)))

(defn ^:private draw-robot! []
  (let [rx (:x @robot-pos)
        ry (:y @robot-pos)
        img (new js/Image)]
    (aset img "src" "assets/robot.png")
    (.drawImage @ctx2d img rx ry 48 48)))

(defn ^:public level-won? []
  (if (= @robot-pos {:x (* 48 9) :y (* 48 9)}) true false))

(defn ^:public update! []
  (.clearRect @ctx2d 0 0 (.-width @ctx2d) (.-height @ctx2d))
  (draw-level!)
  (draw-robot!))

(defn ^:public start-game! [context2d lnum]
  (set-context! context2d)
  (aset @ctx2d "imageSmoothingEnabled" false)
  (reset! current-level lnum)
  (reset! robot-pos {:x 48 :y 48})
  (update!)
  (level-collision-map!))

(defn level-won! []
  (swap! current-level check-inc)
  (let [ch (chan)]
    (doall (go
      (js/setTimeout
       (fn [_] (go (start-game! @ctx2d @current-level)
                   (>! ch :done))) 3000)
      (let [img (js/Image.)]
        (aset img "src" "assets/level-complete.png")
        (.drawImage @ctx2d 232 248)
        (let [_ (<! ch)] nil))
      (.click (.getElementById js/document "reload-button"))))))

(defn ^:private move-robot!
  [& {:keys [x y]}]
  (reset! robot-pos {:x x :y y})
  (update!))


(defn ^:private relative-move-robot!
  [& {:keys [x y]}]
  (move-robot! :x (+ (:x @robot-pos) x)
               :y (+ (:y @robot-pos) y)))

(defn ^:private collidable? [col-map & {:keys [x y]}]
  (not= "0" (get (get col-map y) x)))

(defn ^:public level-collision-map! []
    (-> (.fetch js/window (str "./assets/levels/level" @current-level "-collmap.csv"))
        (.then #(.text %))
        (.then csv/read-csv)
        (.then #(.setItem (.-sessionStorage js/window) (str "collmap" @current-level) %)))
    (.getItem (.-sessionStorage js/window) (str "collmap" @current-level)))

(defn can-move-there? [direction collision-map]
  (let [rbx (:x @robot-pos)
        rby (:y @robot-pos)]
    (not
     (case direction
       :left
       (collidable? collision-map :x (dec (/ rbx 48)) :y (/ rby 48))
       :right
       (collidable? collision-map :x (inc (/ rbx 48)) :y (/ rby 48))
       :up
       (collidable? collision-map :x (/ rbx 48) :y (dec (/ rby 48)))
       :down
       (collidable? collision-map :x (/ rbx 48) :y (inc (/ rby 48)))))))

(defn ^:public check-move-direction! [direction]
  (if (can-move-there? direction
                       (cljs.reader/read-string
                        (.getItem
                         (.-sessionStorage js/window)
                         (str "collmap" @current-level))))
    (relative-move-robot!
     :x (case direction
          :left
          -48
          :right
          48
          0)
     :y (case direction
          :up
          -48
          :down
          48
          0))
    false))
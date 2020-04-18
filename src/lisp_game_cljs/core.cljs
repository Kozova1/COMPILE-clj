(ns lisp-game-cljs.core
    (:require [lisp-game-cljs.lang :as lang]
              [lisp-game-cljs.game :as game]
              [clojure.string :as str]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:error-message ""
                          :debug nil}))
(defonce code-box (.getElementById js/document "code-box"))
(defonce reg-debug-box (.getElementById js/document "reg-debug-box"))
(defonce run-button (.getElementById js/document "run-button"))
(defonce reset-button (.getElementById js/document "reset-button"))
(defonce error-box (.getElementById js/document "error-box"))
(defonce game-canvas (.getElementById js/document "game-canvas"))

(defn dbg-str []
  (swap! app-state #(assoc % :debug @lang/output)))

(declare get-reg-text)

(defn print-error [msg toscroll]
  (doall
   (do
     (if toscroll (.scroll js/window 0 0) nil)
     (swap! app-state #(assoc % :error-message msg))
      (if (not= msg "All OK")
        (set! (-> error-box
                  .-style
                  .-borderColor) "red")
        (set! (-> error-box
                  .-style
                  .-borderColor) "darkgray"))
     (.toUpperCase (get-reg-text)))))

(defn get-reg-text []
  (str (str/join
        "\n"
        (for [reg @lang/regs]
          (let [treg (get reg 0)
                regval (treg @lang/regs)]
            (str (name treg)
                 ": "
                 (if (nil? regval) "nil" regval)))))
       "\n\nOUTPUT:"))

(dbg-str)

(defonce _
  (.addEventListener 
   reset-button "click"
   (fn [_]
     (doall
      (do
        (game/level-collision-map!)
        (game/start-game! (.getContext game-canvas "2d") 1))))))

(defonce _
  (.addEventListener
   run-button "click"
   (fn [_]
     (doall 
      (do
        (.scroll js/window 0 0)
        (game/start-game! (.getContext game-canvas "2d") @game/current-level)
        (print-error "All OK" false)
        (set!
         (.-innerHTML reg-debug-box)
         (try
           (let [result
                 (lang/exec-string (.toUpperCase (.-value code-box)))]
             (str
              (.toUpperCase (get-reg-text))
              (:debug @app-state)))
           (catch :default e
             (case (ex-message e)
               "JUMP-LABEL-MISSING"
               (print-error "JUMP Label mismatch" true)
               "BOOL-VALUE-WRONG-REG"
               (print-error "Boolean assignment to non boolean register" true)
               "NUMERIC-VALUE-WRONG-REG"
               (print-error "Numeric assignment to non numeric register" true)
               "INVALID-REGISTER"
               (print-error "Invalid register" true)
               "CANT-GO-THERE"
               (print-error "Can't go there!" true)
               "SYNTAX-ERROR"
               (print-error "Syntax Error!" true)
               (print-error "All OK" false)))))
        (set! (.-innerHTML error-box) (:error-message @app-state))
        (if (game/level-won?)
            (game/level-won!)
            ;(game/start-game! (.getContext game-canvas "2d") (game/check-inc @game/current-level))
            ))))))

(game/level-collision-map!)
(game/start-game! (.getContext game-canvas "2d") 1)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application-
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  
)

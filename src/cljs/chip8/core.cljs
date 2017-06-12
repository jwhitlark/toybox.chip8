(ns chip8.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as r]
            [cljs.pprint :as pp]
            [clojure.set :as set]
            [chip8.utils :as u]
            [chip8.cpu :as cpu]
            [goog.events :as events]
            [cljs.core.async :as csp])
  (:import [goog.events EventType]))

(enable-console-print!)

(declare init)
(.addEventListener js/window "onload" init)

(def keymap {49 1
             50 2
             51 3
             52 0xC
             81 4
             87 5
             69 6
             82 0xD
             65 7
             83 8
             68 9
             70 0xE
             90 0xA
             88 0
             67 0xB
             86 0xF})

(def key-input (csp/chan (csp/sliding-buffer 1) (map keymap)))

(events/listen js/window EventType.KEYDOWN
               #(csp/put! key-input (.-keyCode %)))

(extend-protocol ISeqable
  js/Uint8Array
  (-seq [array] (array-seq array 0)))

(defn scale [x]
  (* x 4))

(defonce canvas (atom (js/document.getElementById "screen")))
(defonce ctx (atom (.getContext @canvas "2d")))

(defonce app-state (r/atom (cpu/make-cpu)))
(set-validator! app-state cpu/validate)

(swap! app-state cpu/setup-input key-input)

(defonce history (r/atom []))

(declare draw-pixel)
(add-watch app-state :change
           (fn [ky ref old new]
             (println (str "PC: " (:pc new)))
             (when (not= old new)
               (swap! history conj old))
             (let [gfx (apply str (map u/int->binstr (subvec (:gfx new) 0 256)))]
               (doall
                (map #(draw-pixel (mod %1 64)
                                  (quot %1 64)
                                  (if (= "1" %2) true false))
                     (range (* 64 32))
                     gfx)))))

(defonce rom (r/atom nil ))
(add-watch rom :change
           (fn [ky ref old new]
             (when (and new
                        (not= old new))
               (swap! app-state cpu/load-rom new)
               (println "Loaded new rom"))))

(defn debug-view [cpu]
  [:p (str (with-out-str (pp/pprint cpu)))])

(defn resize-canvas! []
  (doto @canvas
    (set! -width (scale 64))
    (set! -height (scale 32))))

(defn draw-pixel [x y v]
  (let [color (if v "black" "white")]
    (when v
      (println (scale x)  y v))
    (doto @ctx
      (.save)
      (set! -fillStyle color)
      (.fillRect (scale x) (scale  y) (scale 1) (scale 1))
      (.restore))))

(declare start! stop! run)

(defn load-pong [app-state]
  [:button.btn.btn-sm.btn-primary-outline {:on-click #(cpu/fetch-rom rom "PONG")} "Load Pong"])

(defn start-button [app-state]
  [:button.btn.btn-sm.btn-primary-outline {:on-click #(do
                                                        ;;(swap! app-state cpu/setup-input key-input)
                                                 (swap! app-state cpu/start)
                           (run))} "Start"])

(defn back-button [app-state]
  [:button.btn.btn-sm.btn-primary-outline {:on-click #(do (reset! app-state (peek @history))
                           (swap! history pop))} "<-"])

(defn step-button [app-state]
  [:button.btn.btn-sm.btn-primary-outline {:on-click #(swap! app-state cpu/step)} "Step"])

(defn stop-button [app-state]
  [:button.btn.btn-sm.btn-primary-outline {:on-click #(swap! app-state cpu/stop)} "Stop"])

(defn toggle-debug []
  [:button.btn.btn-sm.btn-primary-outline {:on-click #(set! js/goog.DEBUG (not js/goog.DEBUG))} "Toggle debug"])

(defn labeled-item [name value show-by-default?]
  (let [show? (r/atom show-by-default?)]
    (fn [name value]
      [:li.list-group-item
       [:span
        [:span.label.label-default {:on-click #(swap! show? not)
                                    :style {:margin-right "8px"}} name]
        (when @show? [:span value])]])))

(defn registers [app-state]
  [:div.card.card-block
   [:div.card-title "Registers"]
   [:ul.list-group.list-group-flush
    [labeled-item "V" (str (:V @app-state)) true]
    [labeled-item "I" (str (:I @app-state)) true]
    [labeled-item "PC" (str (:pc @app-state)) true]
    [labeled-item "Stack" (str (:stack @app-state)) true]
    [labeled-item "Delay timer" (str (:delay-timer @app-state)) true]
    [labeled-item "Sound timer" (str (:sound-timer @app-state)) true]
    [labeled-item "Keyboard" (str (:keyboard @app-state)) false]
    [labeled-item "Gfx" (str (:gfx @app-state)) false]
    [labeled-item "Memory" (str (:memory @app-state)) false]
    [labeled-item "Opcodes seen" (str @u/opcodes-seen) false]
    ]])

(defn controls [app-state]
  [:div.card.card-block
   [:div.card-title "Controls"]
   [:div.card-body.btn-group
    [load-pong app-state]
    [start-button app-state]
    [back-button app-state]
    [step-button app-state]
    [stop-button app-state]
    [toggle-debug]]])

(defn main []
  [:div
   [controls app-state]
   [registers app-state]])

(r/render-component [main] (. js/document (getElementById "app")))


(defn on-figwheel-reload []
  (println "Reloaded at " (js/Date.))
  (cpu/fetch-rom rom "PONG")
  ;; (swap! app-state cpu/cycle)
  )

(defn run []
  (swap! app-state cpu/cpu-cycle)
  (when (= (:state @app-state) :running)
    (js/window.requestAnimationFrame run)))

(defn start! []
  ;; (swap! app-state cpu/setup-input key-input)
  (swap! app-state cpu/start)
  (run))

(defn stop! []
  (swap! app-state cpu/stop))

(defn init []
  (resize-canvas!))

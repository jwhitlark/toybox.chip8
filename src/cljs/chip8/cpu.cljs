(ns chip8.cpu
  (:require [goog.events :as events]
            [goog.net.EventType :as event-type]
            [goog.net.XhrIo :as gxhr]
            [chip8.utils :as u :include-macros true]
            [chip8.opcodes :as op]
            [cljs.core.async :as csp]))

(def fonts
  [0xF0 0x90 0x90 0x90 0xF0 ; 0
   0x20 0x60 0x20 0x20 0x70 ; 1
   0xF0 0x10 0xF0 0x80 0xF0 ; 2
   0xF0 0x10 0xF0 0x10 0xF0 ; 3
   0x90 0x90 0xF0 0x10 0x10 ; 4
   0xF0 0x80 0xF0 0x10 0xF0 ; 5
   0xF0 0x80 0xF0 0x90 0xF0 ; 6
   0xF0 0x10 0x20 0x40 0x40 ; 7
   0xF0 0x90 0xF0 0x90 0xF0 ; 8
   0xF0 0x90 0xF0 0x10 0xF0 ; 9
   0xF0 0x90 0xF0 0x90 0x90 ; A
   0xE0 0x90 0xE0 0x90 0xE0 ; B
   0xF0 0x80 0x80 0x80 0xF0 ; C
   0xE0 0x90 0x90 0x90 0xE0 ; D
   0xF0 0x80 0xF0 0x80 0xF0 ; E
   0xF0 0x80 0xF0 0x80 0x80 ; F
   ])

(defn beep [x]
  (when (pos? x)
    (println "beep"))
  x)

(defn dec-if-pos [x]
  (if (pos? x)
    (dec x)
    x))

(defn read-keypress [this]
  (if-let [in (:input this)]
    (if-let [c (csp/poll! in)]
      (assoc-in this [:keyboard 0] c)
      this)
    this
    )

  )


(defprotocol emulate
  (validate [this] "Check invariants")
  (load-rom [this rom] "Load a rom")
  (start [this] "Start the cpu")
  (step [this] "Step one frame")
  (stop [this] "Stop the cpu")
  (cpu-cycle [this] "Emulate one cpu cycle")
  (update-timers [this] "Decrement timers")
  (draw-screen [this screen] "Draw the screen")
  (setup-input [this input-chan] "Set the chan to receive input on")
  (get-opcode [this] "Lookup and return the current opcode")
  )

(defrecord CPU
    [pc I V
     stack
     delay-timer sound-timer
     memory
     gfx
     keyboard
     state
     input]
  emulate
  (validate [this] (and (= (- 0x1000 256 96) (count (:memory this)))
                        (= (quot (* 64 32) 8) (count (:gfx this)))))
  (load-rom [this rom] (update-in this [:memory] #(u/mem-copy % 0x200 rom)))
  (update-timers [this]
    (-> this
        (update-in [:delay-timer] dec-if-pos)
        (update-in [:sound-timer] #(-> % beep dec-if-pos))))
  (start [this] (assoc-in this [:state] :running))
  (step [this]
    (-> this
        (assoc-in [:state] :step)
        (cpu-cycle)))
  (stop [this] (assoc-in this [:state] :stopped))
  (cpu-cycle [this] (if (= :stopped (:state this))
                  this
                  (let [op (get-opcode this)]
                    (-> (cond
                          (u/match? :00E0 op) (op/clear-screen :00E0 this op)
                          (u/match? :00EE op) (op/return :00EE this op)
                          (u/match? :1NNN op) (op/jump :1NNN this op)
                          (u/match? :2NNN op) (op/call-sub :2NNN this op)
                          (u/match? :3XNN op) (op/skip-if :3XNN this op)
                          (u/match? :4XNN op) (op/skip-if-not :4XNN this op)
                          (u/match? :5XY0 op) (op/skip-if-eq :5XY0 this op)
                          (u/match? :6XNN op) (op/set-vx :6XNN this op)
                          (u/match? :7XNN op) (op/add-n :7XNN this op)
                          (u/match? :8XY0 op) (op/set-vx-to-vy :8XY0 this op)
                          (u/match? :8XY1 op) (op/set-vx-to-vx-or-vy :8XY1 this op)
                          (u/match? :8XY2 op) (op/set-vx-to-vx-and-vy :8XY2 this op)
                          (u/match? :8XY3 op) (op/set-vx-to-vx-xor-vy :8XY3 this op)
                          (u/match? :8XY4 op) (op/add-vy-to-vx :8XY4 this op)
                          (u/match? :8XY5 op) (op/sub-vy-from-vx :8XY5 this op)
                          ;; :8XY6
                          ;; :8XY7
                          ;; :8XYE
                          (u/match? :9XY0 op) (op/skip-if-not-eq :9XY0 this op)
                          (u/match? :ANNN op) (op/set-i :ANNN this op)
                          (u/match? :BNNN op) (op/jump-plus :BNNN this op)
                          (u/match? :CXNN op) (op/set-random :CXNN this op)
                          (u/match? :DXYN op) (op/draw-sprite :DXYN this op)
                          (u/match? :EX9E op) (op/skip-if-keypress :EX9E this op)
                          (u/match? :EXA1 op) (op/skip-if-not-keypress :EXA1 this op)
                          (u/match? :FX07 op) (op/set-vx-to-delay :FX07 this op)
                          (u/match? :FX0A op) (op/get-keypress :FX0A this op)
                          (u/match? :FX15 op) (op/set-delay-timer :FX15 this op)
                          (u/match? :FX18 op) (op/set-sound-timer :FX18 this op)
                          (u/match? :FX1E op) (op/add-vx-to-i :FX1E this op)
                          (u/match? :FX29 op) (op/set-i-to-sprite-loc :FX29 this op)
                          (u/match? :FX33 op) (op/store-binary-representation :FX33 this op)
                          (u/match? :FX55 op) (op/fill-memory-from-v :FX55 this op)
                          (u/match? :FX65 op) (op/fill-v-from-memory :FX65 this op)

                          :default (println (str "Bad opcode: " (u/hex op))))
                        (update-timers)
                        (read-keypress)
                        ))))
  (draw-screen [this screen] (println "draw??/"))
  (setup-input [this input-chan] (-> this
                                     (assoc-in [:input] input-chan)))
  (get-opcode [this] (let [pc-start (:pc this)
                           pc-end (+ 2 pc-start)]
                       (-> (:memory this)
                           (subvec pc-start pc-end)
                           u/mk-opcode))))

;; TODO: Draw screen, beep, get keys, and fetch rom should be in cljs, everything else cljc (jw 2016-03-29)

;; Because the system does not draw every cycle, we should set a draw flag when we need to update our screen. Only two opcodes should set this flag:
;; 0x00E0 – Clears the screen
;; 0xDXYN – Draws a sprite on the screen


(defn make-cpu []
  (map->CPU {:memory (u/mem-copy (vec (repeat (- 0x1000 256 96) 0)) 0 fonts)
             :V (vec (repeat 16 0))
             :I 0
             :pc 0x200
             ;; :gfx u/test-gfx
             :gfx u/empty-gfx
             :delay-timer 0
             :sound-timer 0
             :stack []
             :keyboard (vec (repeat 16 0))
             :state :stopped
             :input nil})
  )

(defn ^:export fetch-rom [atm name]
  (when (#{"PONG" "PONG2"} name)
    (let [req (goog.net.XhrIo.)]
      (.setResponseType req "arraybuffer")
      (events/listen req event-type/SUCCESS
                     (fn [n]
                       ;; stop emulator
                       ;; reset a new app-state
                       (reset! atm (js/Uint8Array. (.getResponse req)))
                       ;; start the emulator
                       ))
      (.send req (str "roms/" name) "GET")
      (.log js/console "Select rom: " name))))

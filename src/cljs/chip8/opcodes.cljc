(ns chip8.opcodes
  (:require #?@(:clj [[chip8.utils :as u]
                      [clojure.core.async :as csp]]
                     :cljs [[chip8.utils :as u :include-macros true]
                            [cljs.core.async :as csp]]))
  )

(defn inc-pc [cpu]
  (update-in cpu [:pc] #(+ 2 %)))

(defn update-row [gfx [offset row]]
  (assoc gfx offset row))

(defn update-rows [gfx updates]
  (reduce update-row gfx updates))

(u/defopcode-handler draw-sprite
  (let [i (:I this)
        x-cord (get-in this [:V X])  ;;(quot (get-in this [:V X]) 8)
        _ (println x-cord)
        y-cord  (get-in this [:V Y])
        _ (println y-cord)
        rows N
        width 8
        sprite (-> this :memory (subvec i (+ i rows)))
        _ (println sprite)
        off-idx (u/offset-indexes (+ x-cord (* 8 y-cord)) width rows)
        _ (println off-idx)
        updates (map #(vector %1 %2) off-idx sprite)
        _ (println updates)]
    (-> this
        (update-in [:gfx] #(update-rows % updates))
        inc-pc)))

(u/defopcode-handler call-sub
  (let [pc (+ 2 (:pc this))]
    (-> this
        (update-in [:stack] #(conj % pc))
        (assoc-in [:pc] N))))

(u/defopcode-handler return
  (let [saved-pc (-> this :stack peek)]
    (-> this
        (update-in [:stack] pop)
        (assoc-in [:pc] saved-pc)
        ;; (inc-pc)
        )))

(u/defopcode-handler jump
  (-> this
      (assoc-in [:pc] N)))

(u/defopcode-handler set-vx
  (-> this
      (assoc-in [:V X] N)
      inc-pc))

(u/defopcode-handler set-vx-to-vy
  (-> this
      (assoc-in [:V X] (get-in this [:V Y]))
      inc-pc))

(u/defopcode-handler set-vx-to-vx-or-vy
  (-> this
      (assoc-in [:V X]
                (bit-or (get-in this [:V X])
                        (get-in this [:V Y])))
      inc-pc))

(u/defopcode-handler set-vx-to-vx-and-vy
  (let [x (get-in this [:V X])
        _ (println (str "X is: " x))
        y (get-in this [:V Y])
        _ (println y)
        new-x (bit-and x y)
        _ (println new-x)]
    (-> this
        (assoc-in [:V X] new-x)
        inc-pc)))

(u/defopcode-handler set-vx-to-vx-xor-vy
  (-> this
      (assoc-in [:V X]
                (bit-xor (get-in this [:V X])
                         (get-in this [:V Y])))
      inc-pc))

(u/defopcode-handler set-vx-to-delay
  (let [delay (:delay-timer this)]
    (-> this
        (assoc-in [:V X] delay)
        inc-pc)))

(u/defopcode-handler skip-if
  (if (= (get-in this [:V X]) N)
    (-> this
        inc-pc
        inc-pc)
    (-> this
        inc-pc)))

(u/defopcode-handler skip-if-not
  (if (not= (get-in this [:V X]) N)
    (-> this
        inc-pc
        inc-pc)
    (-> this
        inc-pc)))

(u/defopcode-handler skip-if-eq
  (if (= (get-in this [:V X])
         (get-in this [:V Y]))
    (-> this
        inc-pc
        inc-pc)
    (-> this
        inc-pc)))

(u/defopcode-handler skip-if-not-eq
  (if (not= (get-in this [:V X])
            (get-in this [:V Y]))
    (-> this
        inc-pc
        inc-pc)
    (-> this
        inc-pc)))

(u/defopcode-handler clear-screen
  (-> this
      (assoc-in [:gfx] u/empty-gfx)
      inc-pc))

(u/defopcode-handler get-keypress
  ;; block until keypress is available.
  (-> this
      (assoc-in [:V X] (csp/<!! (:input this)))
      inc-pc))

(u/defopcode-handler skip-if-keypress
  (if-let [k (csp/poll! (:input this))]
    (if (= (get-in this [:V X]) k)
      (-> this inc-pc inc-pc)
      (-> this inc-pc))
    (-> this inc-pc)))

(u/defopcode-handler skip-if-not-keypress
  (if-let [k (csp/poll! (:input this))]
    (if (= (get-in this [:V X]) k)
      (-> this inc-pc)
      (-> this inc-pc inc-pc))
    (-> this inc-pc inc-pc)))

(u/defopcode-handler add-n
  (-> this
      (update-in [:V X] #(+ % N))
      inc-pc))

(u/defopcode-handler add-vy-to-vx
  (let [vy (get-in this [:V Y])
        vx (get-in this [:V X])
        vx-vy (+ vy vx)
        new-vx (bit-and u/mask-8-bit vx-vy)
        carry (if (not= new-vx vx-vy) 1 0)]
    (-> this
        (assoc-in [:V X] new-vx)
        (assoc-in [:V 0xf] carry)
        inc-pc)))

(u/defopcode-handler add-vx-to-i
  (let [vx (get-in this [:V X])
        i  (get-in this [:I])
        vx-i (+ vx i)
        ;; REVIEW: Not 0xffff? Doc mistake? (jw 2016-03-23)
        new-i (bit-and 0xfff vx-i)
        carry (if (not= vx-i new-i) 1 0)]
    (-> this
        (assoc-in [:I] new-i)
        (assoc-in [:V 0xF] carry)
        inc-pc)))

(u/defopcode-handler set-i
  (-> this
      (assoc-in  [:I] N)
      (inc-pc)))

(u/defopcode-handler sub-vy-from-vx
  (let [x (get-in this [:V X])
        y (get-in this [:V Y])
        new-x (- x y)]
    (-> this
        (assoc-in [:V X] new-x)
        ;;(assoc-in [:V 0xF] 1)
        inc-pc)))

(u/defopcode-handler set-random
  (-> this
      (assoc-in [:V X] (bit-and N (rand-int 0xff)))
      inc-pc))

(u/defopcode-handler store-binary-representation
  (let [vx (get-in this [:V X])
        i (get-in this [:I])
        ones (mod vx 10)
        tens (-> vx (quot 10) (mod 10))
        hundreds (-> vx (quot 100) (mod 10))]
    (-> this
        (assoc-in [:memory i] hundreds)
        (assoc-in [:memory (+ i 1)] tens)
        (assoc-in [:memory (+ i 2)] ones)
        inc-pc)))

;; Fills V0 to VX (including VX) with values from memory starting at address I.
(u/defopcode-handler fill-v-from-memory
  (let [i (:I this)
        mem-values (-> this :memory (subvec i (+ i X)))]
    (-> this
          (update-in [:V] #(u/mem-copy % 0 mem-values))
          inc-pc)))

;; Stores V0 to VX (including VX) in memory starting at address I
(u/defopcode-handler fill-memory-from-v
  (let [i (:I this)
        v-values (-> this :V (subvec 0 X))]
    (-> this
        (update-in [:memory] #(u/mem-copy % i v-values))
        inc-pc)))

;; Sets I to the location of the sprite for the character in
;; VX. Characters 0-F (in hexadecimal) are represented by a 4x5 font.
(u/defopcode-handler set-i-to-sprite-loc
  (let [vx (get-in this [:V X])
        i (u/font-char-loc vx)]
    (-> this
        (assoc-in [:I] i)
        inc-pc)))

(u/defopcode-handler jump-plus
  (-> this
      (assoc-in [:pc] (+ N (get-in this [:V 0])))))

(u/defopcode-handler set-delay-timer
  (let [new-timer (get-in this [:V X])]
    (-> this
        (assoc-in [:delay-timer] new-timer)
        inc-pc)))

(u/defopcode-handler set-sound-timer
  (let [new-timer (get-in this [:V X])]
    (-> this
        (assoc-in [:sound-timer] new-timer)
        inc-pc)))

(ns chip8.opcode-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [chip8.utils :as u]
            [chip8.opcodes :refer :all]
            [clojure.walk :refer [macroexpand-all]]))

;; Opcodes seen
;;  #{set-vx-to-vx-and-vy set-vx-to-delay add-vy-to-vx draw-sprite set-random jump set-vx call-sub set-i return skip-if-not-keypress set-delay-timer set-i-to-sprite-loc add-n skip-if-not fill-v-from-memory store-binary-representation skip-if}

(deftest opcode-tests

  ;; add-n
  (is (= {:V [0 0 0x13] :pc 2}
         (add-n :7XNN {:V [0 0 2] :pc 0} 0x7211)))

  ;; add-vy-to-vx
  (is (=  {:V [0 0 4 0, 0 1 0 0, 0 0 0 0, 0 0 0 0] :pc 2}
         (add-vy-to-vx :8XY4 {:V [0 0 3 0, 0 1 0 0, 0 0 0 0, 0 0 0 0] :pc 0} 0x8254)))
  (is (= {:V [0 0 4 0, 0 5 0 0, 0 0 0 0, 0 0 0 1] :pc 2}
         (add-vy-to-vx :8XY4 {:V [0 0 0xff 0, 0 5 0 0, 0 0 0 0, 0 0 0 0] :pc 0} 0x8254)))

  ;; sub-vv-from-vx
  ;; VY is subtracted from VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
  (is (= {:V [0 5 1 3 2 0 0 0 0 0 0 0 0 0 0 0], :pc 4}
         (sub-vy-from-vx :8XY5 {:V [0 5 4 3, 2 0 0 0, 0 0 0 0, 0 0 0 0] :pc 2} 0x8235)))


  ;; call-sub
  (is (= {:stack [2] :pc 0xAAA}
         (call-sub :2NNN {:stack [] :pc 0} 0x2AAA)))

  ;; draw-sprite
  ;; Sprites stored in memory at location in index
  ;; register (I), 8bits wide. Wraps around the screen. If when drawn,
  ;; clears a pixel, register VF is set to 1 otherwise it is zero. All
  ;; drawing is XOR drawing (i.e. it toggles the screen pixels). Sprites
  ;; are drawn starting at position VX, VY. N is the number of 8bit rows
  ;; that need to be drawn. If N is greater than 1, second line continues
  ;; at position VX, VY+1, and so on.
  (is (=  {:I 4,
           :memory [0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0],
           :pc 2,
           :V [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0],
           :gfx [17 0 0 0 0 0 0 0 0 0 0 0]}
         (draw-sprite :DXYN {:I 4 :memory [0 0 0 0, 0x11 0 0 0, 0 0 0 0, 0 0 0 0]
                             :pc 0 :V [0 0 0 0, 0 0 0 0, 0 0 0 0, 0 0 0 0]
                             :gfx [0 0 0 0, 0 0 0 0, 0 0 0 0]} 0xD011 )))

  ;; X isn't working properly...
  (is (=  {:I 4,
           :memory [0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0],
           :pc 2,
           :V [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0],
           :gfx [0 17 0 0 0 0 0 0 0 0 0 0]}
          (draw-sprite :DXYN {:I 4 :memory [0 0 0 0, 0x11 0 0 0, 0 0 0 0, 0 0 0 0]
                              :pc 0 :V [1 0 0 0, 0 0 0 0, 0 0 0 0, 0 0 0 0]
                              :gfx [0 0 0 0, 0 0 0 0, 0 0 0 0]} 0xD011 )))

  (is (=  {:I 4,
           :memory [0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0],
           :pc 2,
           :V [0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0],
           :gfx [0 0 0 0, 0 0 0 0, 17 0 0 0]}
          (draw-sprite :DXYN {:I 4 :memory [0 0 0 0, 0x11 0 0 0, 0 0 0 0, 0 0 0 0]
                              :pc 0 :V [0 1 0 0, 0 0 0 0, 0 0 0 0, 0 0 0 0]
                              :gfx [0 0 0 0, 0 0 0 0, 0 0 0 0]} 0xD011 )))

  (is (=  {:I 4,
           :memory [0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0],
           :pc 2,
           :V [1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0],
           :gfx [0 0 0 0 0 0 0 0 0 17 0 0]}
          (draw-sprite :DXYN {:I 4 :memory [0 0 0 0, 0x11 0 0 0, 0 0 0 0, 0 0 0 0]
                              :pc 0 :V [1 1 0 0, 0 0 0 0, 0 0 0 0, 0 0 0 0]
                              :gfx [0 0 0 0, 0 0 0 0, 0 0 0 0]} 0xD011 )))

  ;; test erase
  ;; test rows - although that already works visually.
  #_   {:I 4,
        :memory [0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0],
        :pc 2,
        :V [2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0],
        :gfx [0 0 0 0, 0 0 0 0, 17 0 0 0]}

  ;; fill-v-from-memory
  (is (= {:V [1 2 3 0, 0 0] :pc 2 :I 2 :memory [0 0 1 2, 3 4 5 6]}
         (fill-v-from-memory :FX65 {:V [0 0 0 0, 0 0] :pc 0 :I 2 :memory [0 0 1 2, 3 4 5 6]} 0xF365)))

  ;; jump
  (is (= {:pc 0xAAA}
         (jump :1NNN {:pc 0} 0x1AAA)))

  ;; return
  (is (= {:stack [] :pc 2}
         (return :00EE {:stack [2] :pc 0xAAA} 0x00EE)))

  ;; set-delay-timer
  (is (= {:delay-timer 3 :V [0 0 3 0] :pc 2}
         (set-delay-timer :FX15 {:delay-timer 0 :V [0 0 3 0] :pc 0} 0xF215)))

  ;; set-i
  (is (= {:I 0xCCC :pc 2}
         (set-i :ANNN {:I 0 :pc 0} 0xACCC)))

  ;; set-i-to-sprite-loc

  ;; set-random

  ;; set-vx
  (is (= {:V [0 0 3 0] :pc 2}
         (set-vx :6XNN {:V [0 0 0 0] :pc 0} 0x6203)))

  ;; set-vx-to-delay
  (is (= {:V [0 0 8 0] :delay-timer 8 :pc 2}
         (set-vx-to-delay :FX07 {:V [0 0 0 0] :delay-timer 8 :pc 0} 0xF207)))

  ;; set-vx-to-vx-and-vy
  (is (= {:V [0 0 0 5, 0] :pc 2}
         (set-vx-to-vx-and-vy :8XY2 {:V [0 8 0 5 0 ] :pc 0} 0x8132)))
  (is (= {:V [0 1 0 5, 0] :pc 2}
         (set-vx-to-vx-and-vy :8XY2 {:V [0 3 0 5 0 ] :pc 0} 0x8132)))

  ;; set-vx-to-vy
  (is (= {:pc 2 :V [ 0 4 0 0, 4 0 0 0]}
         (set-vx-to-vy :8XY0 {:pc 0 :V [0 4 0 0, 0 0 0 0]} 0x8410)))

  ;;   set-vx-to-vx-or-vy

  ;;   set-vx-to-vx-and-vy

  ;;  set-vx-to-vx-xor-vy




  ;; skip-if
  (is (= {:V [0 0 3 0] :pc 4}
         (skip-if :3XNN {:V [0 0 3 0] :pc 0} 0x3203)))
  (is (= {:V [0 0 1 0] :pc 2}
         (skip-if :3XNN {:V [0 0 1 0] :pc 0} 0x3203)))

  ;; skip-if-not
  (is (= {:V [0 0 1 0] :pc 4}
         (skip-if-not :4XNN {:V [0 0 1 0] :pc 0} 0x3203)))
  (is (= {:V [0 0 3 0] :pc 2}
         (skip-if-not :4XNN {:V [0 0 3 0] :pc 0} 0x3203)))

  ;; skip-if-not-keypress

  ;; store-binary-represe0ntation
  (is (= {:I 1 :V [ 0 0 123 0] :pc 2 :memory [0 1 2 3, 0 0 0]}
         (store-binary-representation :FX33 {:I 1 :V [0 0 123 0] :pc 0  :memory [0 0 0 0, 0 0 0]} 0xF233)))

  )

;; Start here implementing the next thigs to do.
;; ;; clean-rereen
;; (is (= {:gfs u/empty-gfx :pc 0}
;;        (clear-screen [gfx])))


;; t-vx-to-delay

;; ;; skip-if-not

(ns chip8.utils
  (:require [clojure.string :as str]
            #?(:clj [clojure.walk :refer [macroexpand-all]])
            ))

(def opcodes-seen (atom #{}))

(def empty-gfx (vec (repeat (quot (* 64 32) 8) 0)))
(def test-gfx (vec (take (quot (* 64 32) 8)
                         (cycle [0 128]))))
(def mask-8-bit 0xff)
(def mask-16-bit 0xffff)

(def hexchars ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"])

(defn mk-opcode [[b1 b2]]
  (bit-xor (bit-shift-left b1 8)
           b2))

(defn font-char-loc [ch]
  (* ch 5))

(defn offset-source [offset source]
  (map-indexed #(vector (+ offset %1) %2) source))

(defn mem-copy [target offset source]
  ;; will throw if offset is outside vector, but can grow vector beyond old limit
  (apply assoc target (flatten (offset-source offset source))))


(defn hex
  [byte]
  (let [q (quot byte 16)
        r (rem byte 16)]
    (str (if (zero? q) "" (hex q))(nth hexchars r))))


(defn int->binstr [i]
  (let [as-bin #?(:clj (Integer/toBinaryString i)
                  :cljs (.toString i 2))]
    (apply str (take-last 8 (into (seq as-bin) "00000000")))))

(defn int->hexstr [i]
  (if i
    (str "0x" (str/upper-case
               #?(:clj (Integer/toHexString i)
                  :cljs (.toString i 16))))
    "nil"))

(defn hexstr->int [s]
  #?(:clj (Integer/parseInt s 16)
     :cljs (js/parseInt s 16)))

(def hex-digits (->> "0123456789ABCDEF" (map str) set))

(defn has-vars? [x]
  (not (every? hex-digits (map str (name x)))))

(defn mask-num [x]
  (if (hex-digits (name x))
    "F"
    "0"))

(defn var-count [vr patt]
  (let [cnt (->> patt
              (map str)
              (filter #(#{vr} %))
              count)]
    (if (zero? cnt) nil cnt)))

(defn mask-cnt [cnt]
  (case cnt 1 0xF 2 0xFF 3 0xFFF))

(defn get-var[vr patt v]
  (let [pt (name patt)]
    (if-let [cnt (var-count vr pt)]
      (let [idx (- 3 (str/index-of pt vr))
            shift (* 4 (- idx (dec cnt)))
            msk (bit-shift-left (mask-cnt cnt) shift)]
        ;; (println msk)
        (-> (bit-and msk v)
            (bit-shift-right shift))))))

(defn build-mask [x]
  (->> x
       name
       (map str)
       (map mask-num)
       (apply str)
       hexstr->int))

(defn replace-vars [patt]
  (->> patt
       name
       (map str)
       (replace {"X" "0" "Y" "0" "N" "0"})
       (apply str)
       hexstr->int))

(defn match? [patt v]
  ;; put build-mask in here
  (let [msk (build-mask patt)
        msk-patt (->> patt
                      name
                      replace-vars
                      (bit-and msk))]
    (= msk-patt (bit-and msk v))))

(defmacro defopcode-handler [nm & body]
  `(defn ~nm [~'op-patt ~'this ~'op]
     (swap! opcodes-seen conj (quote ~nm))
     (let [~'X (get-var "X" ~'op-patt ~'op)
           ~'Y (get-var "Y" ~'op-patt ~'op)
           ~'N (get-var "N" ~'op-patt ~'op)]
       (println (str "op: " (int->hexstr ~'op)
                     ", hdlr: " (quote ~nm)
                     ", X: " (int->hexstr ~'X)
                     ", Y: " (int->hexstr ~'Y)
                     ", N: " (int->hexstr ~'N)))
      ~@body)))


;; drawing

(defn get-row [target offset]
  (nth target offset))

(defn set-row [target offset v]
  (assoc target offset v))

(defn offset-indexes [offset width rows]
  (map #(+ offset (* % width))
       (range rows)))

(defn xor-row [a b cleared?]
  (let [reset? (pos? (bit-and a b))]
       (when reset?
         (reset! cleared? true))
    (bit-xor a b)))

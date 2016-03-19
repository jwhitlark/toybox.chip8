(ns user
  (:require [chip8.server]
            [ring.middleware.reload :refer [wrap-reload]]
            [figwheel-sidecar.repl-api :as figwheel]

            [clojure.java.shell]))

;; Let Clojure warn you when it needs to reflect on types, or when it does math
;; on unboxed numbers. In both cases you should add type annotations to prevent
;; degraded performance.
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defn start-sass []
  (future
    (println "Starting sass.")
    (clojure.java.shell/sh "lein" "auto" "sassc" "once")))

(def http-handler
  (wrap-reload #'chip8.server/http-handler))

(defn run []
  (figwheel/start-figwheel!)
  (start-sass))

(def browser-repl figwheel/cljs-repl)

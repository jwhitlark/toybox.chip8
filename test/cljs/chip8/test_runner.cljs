(ns chip8.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [chip8.core-test]))

(enable-console-print!)

(doo-tests 'chip8.core-test)

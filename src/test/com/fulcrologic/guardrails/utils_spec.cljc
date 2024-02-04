(ns com.fulcrologic.guardrails.utils-spec
  (:require
    [fulcro-spec.core :refer [specification component => assertions]]
    [com.fulcrologic.guardrails.utils :as u]))

(specification "Call tracing"
  (let [call-stack (fn [] (mapv :f (u/current-backtrace)))]
    (component "Enter"
      (binding [u/*backtrace* (u/new-backtrace)]
        (component
          "Shows only non-empty entries"
          (u/backtrace-enter 'foo.bar 'f 1 2)
          (assertions (call-stack) => '[foo.bar/f])
          (u/backtrace-enter 'foo.bar 'g 2 2)
          (assertions (call-stack) => '[foo.bar/g foo.bar/f])
          (u/backtrace-enter 'foo.bar 'h 3 2)
          (assertions (call-stack) => '[foo.bar/h foo.bar/g foo.bar/f])
          (u/backtrace-enter 'foo.bar 'i 4 2)
          (assertions (call-stack) => '[foo.bar/i foo.bar/h foo.bar/g foo.bar/f])
          (u/backtrace-enter 'foo.bar 'j 5 2)
          (assertions (call-stack) => '[foo.bar/j foo.bar/i foo.bar/h foo.bar/g foo.bar/f]))
        (component
          "Starts dropping at 5"
          (u/backtrace-enter 'foo.bar 'k 6 2)
          (assertions (call-stack) => '[foo.bar/k foo.bar/j foo.bar/i foo.bar/h foo.bar/g]))))
    (component "Exit"
      (binding [u/*backtrace* (u/new-backtrace)]
        (component "Properly backs up the call trace"
          (u/backtrace-enter 'foo.bar 'f 1 2)
          (u/backtrace-enter 'foo.bar 'g 2 2)
          (u/backtrace-enter 'foo.bar 'h 3 2)
          (assertions (call-stack) => '[foo.bar/h foo.bar/g foo.bar/f])
          (u/backtrace-exit)
          (assertions (call-stack) => '[foo.bar/g foo.bar/f])
          (u/backtrace-exit)
          (assertions (call-stack) => '[foo.bar/f])
          (u/backtrace-enter 'foo.bar 'i 4 2)
          (assertions (call-stack) => '[foo.bar/i foo.bar/f]))

        (assertions
          "Can be converted to a string-friendly form"
          (u/backtrace-str) => "    (foo.bar/i 4 2)\n    (foo.bar/f 1 2)")

        (component "Can roll backwards over an overflow"
          (u/backtrace-enter 'foo.bar 'f 1 2)
          (u/backtrace-enter 'foo.bar 'g 2 2)
          (u/backtrace-enter 'foo.bar 'h 3 2)
          (u/backtrace-enter 'foo.bar 'i 4 2)
          (u/backtrace-enter 'foo.bar 'j 4 2)
          (u/backtrace-enter 'foo.bar 'k 4 2)
          (assertions (call-stack) => '[foo.bar/k foo.bar/j foo.bar/i foo.bar/h foo.bar/g])
          (u/backtrace-exit)
          (assertions (call-stack) => '[foo.bar/j foo.bar/i foo.bar/h foo.bar/g])
          (u/backtrace-exit)
          (assertions (call-stack) => '[foo.bar/i foo.bar/h foo.bar/g])
          (u/backtrace-exit)
          (assertions (call-stack) => '[foo.bar/h foo.bar/g])
          (u/backtrace-exit)
          (assertions (call-stack) => '[foo.bar/g])
          (u/backtrace-exit)
          (assertions (call-stack) => '[])
          (u/backtrace-exit)
          (assertions
            "Is OK backing up on an empty stack"
            (call-stack) => '[]))))))

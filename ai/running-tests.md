If you have a REPL available (clojure-mcp), then running tests in any namespace N should be done as:

```
(do
    (require 'N)
    (in-ns 'N)
    (require '[kaocha.repl :as k]) 
    (k/run 'N))
```

This gives a minimal output on success to prevent context size from getting out of hand.


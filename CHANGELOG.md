# 1.2.5

* Removed async support (new optimizations are better)

# 1.2.1 - 1.2.4

* Minor bug fixes and optimizations

# 1.2.0-2 Malli Support and Output Improvements

Guardrails now implements all of clojure.spec, Orchestra and Malli’s combined function call validation functionality,
including a number of unique features and a minimal, adjustable runtime performance impact during development. 

In this release:

Malli Support

* Full support for Malli schemas (on par with clojure.spec)
* Substantial performance improvements 
  * runtime check throttling 
  * granular namespace/function exclusions for internal application and library code
  * library authors can declare exclusions for their own non-public-facing namespaces
* Full support for checking such-that predicates on the return value
* Automatic Malli (m/=>) function schema generation for use with native or third-party tooling

Improved output support for schema failures
    
    * Stack pruning/elision
    * Trace tracking
    * Able to select stderr
    * Better config reporting on startup
    * Compact reporting option
    * Added validator memoization for malli


    Non-exhaustive malli checks, various fixes, documentation, code clean-up and reformatting

# 1.2.0 Malli Support, but use 1.2.0-2 instead.

# 1.1.0

* Async reporting

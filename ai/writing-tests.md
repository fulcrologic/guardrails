# How to Write Tests

## Use Fulcro Spec

Be sure the namespace requires the library we use for writing tests. For a test of namespace N:

```
(ns N-test
  (:require 
    [fulcro-spec.core :refer [assertions specification behavior component => =fn=> =throws=>]]))
```

## Basic Rules

* Cover all behaviors you can find in the source function
* Each behavior should aim at a structure of Setup, Run, and Assert. There should be whitespace between each section to
  help visually separate the parts, and make it obvious which thing is under test. It is fine to share setup among
  behaviors, but beware of too much combination that harms comprehension.

## Writing Tests

Each test is defined by a specification (a macro that outputs a clojure deftest):

```
(specification "name of description of the thing being tested" 
  ...)
```

the content of a specification can nest optional `component` and `behavior` sections. These are synomymous and choose
one based on whichever fits the context.

```
(specification "subject" 
   (component "description of subelement"
      ...)
```

when doing this nesting, the combination of the strings for each nesting level should combine to make a readable
sentence. For example:

```
(specification "trim" 
  (behavior "removes whitespace from start and end."
    ...))
```

results in "trim removes whitespace from start and end".

Assertions are created with the `assertions` macro. Which takes behavior strings followed by assertions. The string is
optional (e.g. if you surrounded things with a behavior block), but useful to talk about nested aspects. The assertion
clauses are an expression, the symbol `=>`, and then an expected expression:

```
   (assertions 
      "behavior string"
      actual1 => expected1
      actual2 => expected2
      "behavior string"
      actual3 => expected3)
```

NOTE: The behavior string must be a LITERAL string and NOT an expression. You can, however, use an
expression with the `component` or `behavior` wrapper:

```
(doseq [element (things)]
  (component (str element)
     (assertions
        "has blah"
        (:blah element) => true)))
```

So, A complete test looks like this:

```
(specification "The trim function" 
  (behavior "removes whitespace"
    (assertions
      "from the beginning"
      (trim " \tfoo") => "foo"
      "from the end"
      (trim "bar\n\n   \t") => "bar"))
  (behavior "treats nil as an empty string"
    (assertions
      (trim nil) => "")))
```

Notice how sentences are created by the nesting. E.g.
"The trim function removes whitespace from the beginning"
"The trim function treats nil as an empty string".

### Using Predicates

Sometimes a test is more clear when using predicate functions:

```
(assertions
  "returns a string"
  return-value =fn=> string?)
```

IMPORTANT: Think about how a failing test will read when a predicate is used. Does the reader need to see the result to
comprehend what went wrong? If so, use values instead of predicates.

Assume we have a schema system where the predicate `(conforms? :schema value)` and a helper `(explain :schema value)`
exist. The latter returns nil if there are no problems, but a human-readable explanation if there is a problem.

```
;; Bad. A failure gives no comprehension to the reader
(assertions
  "conforms to a schema"
  result =fn=> (partial comforms :schema))
```

vs.

```
;; Much better. User will see WHY the test failed, not just THAT the test failed
(assertions
  "conforms to the schema"
  (explain :schema result) => nil)
```

### Testing Error Conditions

## Mocking

Use 

```
(when-mocking
   (function-name a1 a2) => value
   ...

   code-and-tests)
```

Each triple (function-like call with binding symbols on the left of => and a result on the right) will replace the given
function, and will capture the real arguments into the binding symbols. In the example, a1 and a2 will be available on the 
right-hand side as real values, and will be captured into the mocking system.

So:

```
(when-mocking
   (function-name a1 a2) => (+ a1 a2)
   ...

   (function-name 4 5))
```

will return mock the function, then call it, and of course the whole block returns the last expression.

The argument lists are recorded, and can be checked with `(mock/calls-of function-name)` or `(mock/call-of function-name 0)`
where the number is which call. The result(s) are maps whose keys are the binding symbols (e.g. `'a1`).

NOTE: You CANNOT mock something that will not be called. If you mock it, it MUST be called by the code.

### Scripting 

It is possible to specify how many times an item should be mocked by a given line. This lets you do a script-like setup. Use
an arrow with a call count in it (not count means greedily consume all calls):

For example:

```
(when-mocking
   (f x) =1x=> (* x x)
   (f x) =1x=> (+ x x)
   (f x) => (- x x)

   (assertions
     (f 4) => 16 ;; first mock
     (f 4) => 8  ;; second mock
     (f 4) => 0  ;; last mock is greedy and will represent the rest
     (f 5) => 0
     (f 6) => 0))
```

If you call a function MORE times than it is mocked (e.g. don't use =>) then that will cause the test to fail as well.


### Causing Exceptions

A mock can intentionally cause exceptions simply by throwing. The assertions can check for an exception using a 
regular expression that will match against the message in the exception.

```
(when-mocking
  (f x) => (throw (Exception. "Hello world"))

  (assertions
    (f 4) =throws=> #"world"))
```

### Spying

You can spy on something by using the `mock/real-return` function in a mock. This will retain the behavior, but capture the args and return value. Use `(mock/return-of f ncall)` to see the return value. See `(require [fulcro-spec.mocking :as mock])`

```
(defn f [x] (* x x))

(when-mocking
  (f x) => (mock/real-return)

  (f 10)
  
  (assertions
    (mock/return-of f 0) => 100))
```

Other useful helpers are `mock/calls-of`, `mock/call-of`, `mock/returns-of`, and `mock/spied-value`.

Remember that you can use /ai/clojure-library-source-and-documentation.md along with the clojure-mcp to
read source and documentation of namespaces.

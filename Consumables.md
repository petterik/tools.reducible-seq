# Consumables

```
con·sume
verb: use up (a resource).

con·sum·a·ble
noun: a commodity that is intended to be used up relatively quickly.

source: merriam-webster.com
```

## Idea

Reducing space complexity on any K "one time use" sequence functions to O(1) when reduced and O(N) when used as a sequence, as opposed to O(N*K) for both cases.

## Proposal

Create building blocks for collapsing transformations into transducers when they're only used once.

Create a new sequence transducing context which is as fast as most of the custom lazy-seq implementations.

### Current implementation details

Introducing a new interface IConsumable with one method for extracting a transducer.

Extending the IConsumable interface to all LazySeq's returned by all sequence functions that have a transducer.

Introducing a new function `consumable!` which recursively extracts the transducers from the sequence functions and returns a reducible object. 

The `consumable!` function is currently destructible, rendering the original collection useless. The reason for this is to make sure the sequence functions are not run more than once, to stay true to Clojure's current semantics.

When `seq` is called on the `consumable!` return object, it's results are cached, just like a lazy-seq (because it is a lazy-seq).

The object returned by `consumable!` should be as fast or faster to iterate over in both reducing contexts as in seq contexts (first, rest) both chunked and not.

## Problems
1. Making the feature a la carte. Sequence functions can opt-in to use the consumable features of:
* Consumable in reducing contexts
* Consumable in sequence contexts
  * when composing with at least one other consumable
  * when used alone.

2. Keeping the semantics of the functions, i.e. the lazyness. In a sequence context, the transducers are not allowed buffer results and must call the reducing functions a constant number of times per item. Concatenation is not allowed. But in a reducing context, both of these are legal.

3. Implementing a single transducer sequence as fast as the current custom lazy-seq implementations.
  * I.e. making these two as fast:
    - (run! identity (map inc (range 1e6)))
    - (run! identity (consumable! (map inc (range 1e6))))
  * The current transducer sequence implementation is way too slow:
    - (run! identity (sequence (map inc) (range 1e6)))

4. Getting the compiler to add the consumable optimizations when possible. It would not be ideal if Clojure devs introduced calls to a function like `consumable!` all over their code, when most of the time it's not beneficial.


## Progress



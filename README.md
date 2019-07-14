# tools.reducible-seq

Experimenting with optimizing the sequence functions with a transducer to implement IReduceInit. 

Near 5x performance speed up in a very good case.

There are some downsides though. One being a breaking change, where sequences could be calculated twice if one is not careful. There's a warning when this happens though.

## Usage

```sh
clj -Sdeps '{:deps {petterik/tools.reducible-seq {:git/url "https://github.com/petterik/tools.reducible-seq" :sha "5a7496212433cbafc4058a62b79ac35292430264"}}}'
```

```clj
(require '[petterik.tools.reducible-seq])
(in-ns 'petterik.tools.reducible-seq)

;; Using clojure.core's map, mapcat, filter and keep
(let [map clojure.core/map
      filter clojure.core/filter
      mapcat clojure.core/mapcat]
  (time (->> (range (long 1e6))
          (map str)
          (mapcat seq)
          (map int)
          (filter even?)
          (keep #(when (< 2 %) %))
          (map inc)
          (map inc)
          (map inc)
          (map inc)
          (map inc)
          (reduce + 0))))
;; Elapsed time: 3086.734262 msecs

;; With experiment's implementation of map, mapcat, filter and keep
(time (->> (range (long 1e6))
        (map str)
        (mapcat seq)
        (map int)
        (filter even?)
        (keep #(when (< 2 %) %))
        (map inc)
        (map inc)
        (map inc)
        (map inc)
        (map inc)
        (reduce + 0)))
;; Elapsed time: 575.475232
```

## License

Copyright © 2019 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.

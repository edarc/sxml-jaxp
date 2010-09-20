(ns sxml-jaxp.util
  "Miscellaneous utility functions for internal use.")

(defn any-of
  "Takes a variable number of predicates and returns a predicate P which will
  be true if at least one of the predicates applied to P's arguments is true."
  ([p1] p1)
  ([p1 p2]
   (fn [& x] (or (apply p1 x) (apply p2 x))))
  ([p1 p2 & ps]
   (fn [& x] (or (apply p1 x)
                 (apply p2 x)
                 (some #(apply % x) ps)))))

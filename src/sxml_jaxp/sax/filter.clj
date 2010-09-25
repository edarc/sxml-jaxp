(ns sxml-jaxp.sax.filter
  "Tools for XML stream filtering using the SAX event seq format."
  (:require [sxml-jaxp.sax :as sax]))

(defn filter-with
  "Returns a SAX event seq resulting from applying filters to form, in
  left-to-right order."
  [filters form]
  (let [filter-chain (apply comp (reverse filters))]
    (with-meta
      (if (not= (type form) ::sax/compiled-sxml)
        (filter-chain (sax/compile-sxml form))
        (filter-chain form))
      {:type ::sax/compiled-sxml})))

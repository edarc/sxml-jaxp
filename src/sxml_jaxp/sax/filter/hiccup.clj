(ns sxml-jaxp.sax.filter.hiccup
  "Filter for using Hiccup-style shortcuts for XHTML class and id
  specification."
  (:require [clojure.set :as sets]))

(def hiccup-tag-re #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(defn- parse-hiccup-kw
  "Parse a Hiccup-style element keyword into the constituent pieces. Returns
  [tag id classes], where tag is a keyword specifying the proper element name,
  id is the id attribute value, and classes is a set of strings specifying the
  class names to be applied."
  [kw]
  (let [[tag id classes] (rest (re-matches hiccup-tag-re (name kw)))
        classes          (if classes
                           (set (.split classes "\\."))
                           #{})]
    (when (nil? tag)
      (throw (IllegalArgumentException.
               (str "Bogus Hiccup tag syntax: " kw))))
    [(keyword (namespace kw) tag) id classes]))

(defn- rewrite-element-event
  "Rewrite an element event such that Hiccup-specified properties in the tag
  name are merged with the attribute list as specified in the 'hiccup
  docstring. If attrs is nil, attribute merging is skipped and the element name
  only is rewritten and returned."
  [tag attrs]
  (let [[tag id hic-cls] (parse-hiccup-kw tag)]
    (if attrs
      (let [attr-cls  (:class attrs)
            attr-cls  (cond
                        (set? attr-cls)    attr-cls
                        (string? attr-cls) (set (filter #(pos? (.length %))
                                                        (.split attr-cls " ")))
                        :else              #{})
            eff-cls   (sets/union attr-cls hic-cls)
            eff-cls   (when (seq eff-cls)
                        (apply str (interpose " " eff-cls)))
            eff-attrs (merge (when id {:id id})
                             attrs
                             (when eff-cls {:class eff-cls}))]
        [tag eff-attrs])
      tag)))

(defn- event-filter
  "Hiccup-filter a single event."
  [event]
  (let [[ev tag attrs] event]
    (condp = ev
      :start-element
      (let [[new-tag new-attrs] (rewrite-element-event tag attrs)]
        [:start-element new-tag new-attrs])
      :end-element
      [:end-element (rewrite-element-event tag nil)]
      event)))

(defn hiccup
  "Filter that translates Hiccup-style id and class shortcut syntax, e.g.

    :div#id.class1.class2
    => [:div {:id \"id\", :class \"class1 class2\"}]

  If the :id key appears in the tag attribute map, it overrides the
  Hiccup-specified id. If a :class key appears in the tag attribute map, the
  classes listed in the attribute value (which may be an HTML-style
  space-delimited string, or a set of strings) will be unioned with the
  Hiccup-specified classes:

    [:div.a.b {:class \"b c\"}]
    => [:div {:class \"a b c\"}]"
  [form]
  (map event-filter form))

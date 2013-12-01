(ns sxml-jaxp.core
  "Tools for using SXML-inspired XML representations with JAXP."
  (:use [sxml-jaxp.util :only [any-of]]))

(defn attr-is-xmlns?
  "Predicate which returns true if the given keyword (corresponding to an
  attribute name) is an XML namespace declaration."
  [kw]
  (or (= :xmlns kw)
      (= "xmlns" (namespace kw))))

(defn xmlnsify
  "Converts a keyword naming an XML namespace prefix and converts it into a
  keyword representing the attribute name of an xmlns declaration."
  [kw]
  (if kw
    (keyword "xmlns" (name kw))
    :xmlns))

(defn de-xmlnsify
  "Accepts a keyword corresponding to an xmlns attribute and returns a keyword
  representing the namespace prefix itself."
  [kw]
  (when-not (= kw :xmlns)
    (keyword (name kw))))

(defn normalize-1
  "Return the given SXML representation with the top level element in
  normalized, long form."
  [form]
  (cond
    (vector? form)
    (let [[tag maybe-attrs & tail] form]
      (cond
        (map? maybe-attrs) form
        (nil? maybe-attrs) [tag {}]
        :else (vec (concat [tag {}] (conj tail maybe-attrs)))))
    (keyword? form) [form {}]
    :else form))

(defn tag
  "Get the tag name of the given SXML element."
  [elem]
  (first (normalize-1 elem)))

(defn attrs
  "Get the attribute map of the given SXML element."
  [elem]
  (nth (normalize-1 elem) 1))

(defn prefix-decls
  "Get any XML namespace prefix declarations on the given SXML element."
  [elem]
  (let [elem-attrs (attrs elem)]
    (into {} (for [k (filter attr-is-xmlns? (keys elem-attrs))]
               [(de-xmlnsify k) (elem-attrs k)]))))

(defn update-attrs
  "Return the element with updated attribute values."
  [new-attrs elem]
  (let [[tag attrs & children] (normalize-1 elem)]
    (reduce conj [tag (merge attrs new-attrs)] children)))

(defn children
  "Get the child nodes of the given SXML element."
  [elem]
  (subvec (normalize-1 elem) 2))

(defn alter-children
  "Apply a function to the children of the given SXML element."
  [elem f]
  (let [[tag attrs & children] (normalize-1 elem)]
    (into [tag attrs] (f children))))

(defn normalize
  "Return the normalized, long form of the given SXML representation. All tag
  elements will be of the form [tag attrs & children], where tag is a keyword
  naming the element, attrs is a possibly-empty map of attributes, and children
  is a possibly empty sequence of child elements."
  [form]
  (cond
    (vector? form)
    (vec (let [[tag maybe-attrs & tail] form]
           (cond
             (map? maybe-attrs) (concat [tag maybe-attrs] (map normalize tail))
             (nil? maybe-attrs) [tag {}]
             :else (concat [tag {}] (map normalize (conj tail maybe-attrs))))))
    (keyword? form) [form {}]
    :else form))

(defn- simplify*
  "Implementation of 'simplify; assumes form is already normalized."
  [form]
  (if ((any-of seq? vector?) form)
    (let [[tag attrs & content] form]
      (cond
        (= attrs {})       (simplify* (conj content tag))
        (= 1 (count form)) tag
        :else              (vec (map simplify* form))))
    form))

(def ^{:doc "Returns the simplified, short form of the given SXML
            representation. Tags with attributes and children will be of the
            form [tag attrs & children], tags with no attributes will be of the
            form [tag & children], and tags with neither attributes nor
            children will be of the form tag; where tag is a keyword naming the
            element, attrs is a non-empty map of attributes, and children is a
            non-empty sequence of child elements."
       :arglists '([form])}
  simplify (comp simplify* normalize))

(defn apply-namespaces
  "Inserts xmlns attributes into the root element of form which declare the
  namespaces defined in xmlns-map. Assumes form is normalized."
  [form xmlns-map]
  (let [xmlns (into {} (for [[k v] xmlns-map] [(xmlnsify k) v]))
        [root-tag attrs & content] form]
    (vec (concat [root-tag] [(merge xmlns attrs)] content))))

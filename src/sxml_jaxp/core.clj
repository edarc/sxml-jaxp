(ns sxml-jaxp.core
  "Tools for using SXML-inspired XML representations with JAXP.")

;; Normalization/simplification
;;
(def ^{:doc "Return true if elem is a tag (and not a text node or some other
            thing)."
      :arglists '([elem])}
  tag-elem? (some-fn keyword? vector?))

(defmacro for-tag
  "Evaluate the body if the element is a tag, otherwise produce an identity."
  [elem-form & body]
  `(let [elem# ~elem-form]
     (if (tag-elem? elem#)
       (do ~@body)
       elem#)))

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
        :else (reduce conj [tag {} maybe-attrs] tail)))
    (keyword? form) [form {}]
    :else form))

(defn normalize
  "Return the normalized, long form of the given SXML representation. All tag
  elements will be of the form [tag attrs & children], where tag is a keyword
  naming the element, attrs is a possibly-empty map of attributes, and children
  is a possibly empty sequence of child elements."
  [form]
  (for-tag form
    (let [[tg at & ch] (normalize-1 form)]
      (into [tg at] (map normalize ch)))))

(defn simplify
  "Returns the simplified, short form of the given SXML representation. Tags
  with attributes and children will be of the form [tag attrs & children], tags
  with no attributes will be of the form [tag & children], and tags with
  neither attributes nor children will be of the form tag; where tag is a
  keyword naming the element, attrs is a non-empty map of attributes, and
  children is a non-empty sequence of child elements."
  [form]
  (for-tag form
    (let [[tg at & ch] (normalize-1 form)]
      (cond
        (and (= at {})
             (not (seq ch))) tg
        (= at {})            (into [tg] (map simplify ch))
        :else                (into [tg at] (map simplify ch))))))

;; Namespace helpers
;;
(defn ns-decl?
  "Predicate which returns true if the given keyword (corresponding to an
  attribute name) is an XML namespace declaration."
  [kw]
  (and (keyword? kw)
       (or (= :xmlns kw)
           (= "xmlns" (namespace kw)))))

(defn prefix->ns-decl
  "Converts a keyword or string naming an XML namespace prefix and converts it
  into a keyword representing the attribute name of an xmlns declaration. A
  prefix of nil or empty string indicates the default namespace; i.e.
  (prefix->ns-decl nil) returns :xmlns."
  [pfx]
  (cond
    (keyword? pfx) (keyword "xmlns" (name pfx))
    (= "" pfx) :xmlns
    (string? pfx) (keyword "xmlns" pfx)
    (nil? pfx) :xmlns))

(defn ns-decl->prefix
  "Accepts a keyword corresponding to an xmlns attribute name and returns a
  keyword representing the namespace prefix itself, or nil if the attribute
  name is for the default namespace declaration."
  [kw]
  (cond
    (= :xmlns kw) nil
    (and (keyword? kw)
         (= "xmlns" (namespace kw))) (keyword (name kw))
    :else (throw (IllegalArgumentException.
                   (format "%s is not a namespace declaration"
                           (pr-str kw))))))

(defn- ns-decls->xmlns-attrs
  "Convert a map of namespace-prefixes-to-URIs into a map of
  xmlns-attributes-to-URIs."
  [nsds]
  (into {} (for [[pfx uri] nsds] [(prefix->ns-decl pfx) uri])))

(defn- xmlns-attrs->ns-decls
  "Convert a map of xmlns-attributes-to-URIs into a map of
  namespace-prefixes-to-URIs."
  [nsattrs]
  (into {} (for [[nsattr uri] nsattrs] [(ns-decl->prefix nsattr) uri])))

(defn xmlns-attrs
  "Get the XML namespace prefix declarations from a set of attributes, without
  converting them into a prefix map (that is, the keys will still be in
  :xmlns/foo attribute form, not prefix-only form)."
  [at]
  (select-keys at (filter ns-decl? (keys at))))

(defn strip-xmlns-attrs
  "Remove all XML namespace prefix declarations from a set of attributes."
  [at]
  (select-keys at (filter (complement ns-decl?) (keys at))))

(defn ns-decls
  "Get the XML namespace prefix declarations on the given SXML element."
  [elem]
  (when (tag-elem? elem)
    (let [[_ at & _] (normalize-1 elem)]
      (-> at xmlns-attrs xmlns-attrs->ns-decls))))

(defn- replace-attrs-and-ns-decls
  "Updates the attributes in an element (without regard to whether they are
  namespace declarations or normal attributes) to the value of calling f on the
  attributes."
  [elem new-attrs-nsds]
  (for-tag elem
    (let [[tg _ & ch] (normalize-1 elem)]
      (into [tg new-attrs-nsds] ch))))

(declare attrs)

(defn alter-ns-decls
  "Replaces the XML namespace declarations in the root element with those given
  by calling f on the element's current namespace declarations. Does not modify
  the namespace prefixes of any elements or attributes."
  [elem f]
  (for-tag elem
    (let [nsd (ns-decls elem)
          at (attrs elem)]
      (replace-attrs-and-ns-decls
        elem (merge at (-> nsd f ns-decls->xmlns-attrs))))))

(defn update-ns-decls
  "Updates the XML namespace declarations in the root element to include the
  namespace declarations specified in xmlns-map. Does not modify the namespace
  prefixes of any elements or attributes."
  [elem xmlns-map]
  (alter-ns-decls elem (fn [nsm] (merge nsm xmlns-map))))

(defn replace-ns-decls
  "Replaces the XML namespace declarations in the root element with the ones in
  xmlns-map. Does not modify the namespace prefixes of any elements or
  attributes."
  [elem xmlns-map]
  (alter-ns-decls elem (constantly xmlns-map)))

;; Access helpers
;;
(defn tag
  "Get the tag name of the given SXML element."
  [elem]
  (first (normalize-1 elem)))

(defn attrs
  "Get the attribute map of the given SXML element."
  [elem]
  (for-tag elem
    (let [[_ at & _] (normalize-1 elem)]
      (strip-xmlns-attrs at))))

(defn children
  "Get the child nodes of the given SXML element."
  [elem]
  (for-tag elem
    (let [ch (subvec (normalize-1 elem) 2)
          parent-ns-decls (ns-decls elem)]
      (into [] (map #(alter-ns-decls
                       % (partial merge parent-ns-decls))
                    ch)))))

;; Modification helpers
;;
(defn alter-tag
  "Return the element's attributes and children attached to a tag produced by
  calling f on the tag."
  [elem f]
  (for-tag elem
    (let [[tg at & ch] (normalize-1 elem)]
      (into [(f tg) at] ch))))

(defn replace-tag
  "Return the element's attributes and children attached to a different tag."
  [elem new-tag]
  (alter-tag elem (constantly new-tag)))

(defn alter-attrs
  "Replace the element's attributes with those given by calling f on the
  element's current attributes."
  [elem f]
  (for-tag elem
    (let [[tg at & ch] (normalize-1 elem)
          normal-attrs (strip-xmlns-attrs at)
          ns-attrs (xmlns-attrs at)]
      (into [tg (merge (f normal-attrs) ns-attrs)] ch))))

(defn update-attrs
  "Merge the attributes given in new-attrs into the element's current
  attributes."
  [elem new-attrs]
  (alter-attrs elem (fn [old-attrs] (merge old-attrs new-attrs))))

(defn replace-attrs
  "Replace the element's attributes with the ones in new-attrs."
  [elem new-attrs]
  (alter-attrs elem (constantly new-attrs)))

(defn alter-children
  "Return the element with its children replaced with those given by calling f
  on the current chilren. This calls f once on the entire sequence of child
  elements and expects f to return a sequence.

  See also map-children for a version which maps f across each child element
  individually."
  [elem f]
  (for-tag elem
    (let [[tg at & ch] (normalize-1 elem)]
      (into [tg at] (f ch)))))

(defn replace-children
  "Return the element with its children replaced with the ones in
  new-children."
  [elem new-children]
  (alter-children elem (constantly new-children)))

(defn map-children
  "Return the element with its children replaced by those given by mapping f
  across the current children. This calls f once on each child element and
  expects f to return a new element."
  [elem f]
  (alter-children elem (partial map f)))

(defn on-tags
  "Combinator for applying functions to tag elements only. ((on-tags f) elem)
  returns (f elem) if elem is a tag element, and returns elem otherwise. Useful
  for applying a function only to tag elements but leaving other nodes
  unmodified, particularly when used with map-children."
  [f]
  (fn [elem] (if (tag-elem? elem) (f elem) elem)))

(defn on-text
  "Combinator for applying functions to text nodes only. ((on-text f) elem)
  returns (f elem) if elem is a text node, and returns elem otherwise. Useful
  for applying a function only to text nodes but leaving other elements
  unmodified, particularly when used with map-children."
  [f]
  (fn [elem] (if (string? elem) (f elem) elem)))

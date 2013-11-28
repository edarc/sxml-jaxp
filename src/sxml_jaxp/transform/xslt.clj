(ns sxml-jaxp.transform.xslt
  "Syntactic sugar for constructing XSL templates in SXML."
  (:refer-clojure :exclude [comment key import sort]))

(def xslt-ns "http://www.w3.org/1999/XSL/Transform")

(defmacro ^{:private true} deftag
  "Define a function that emits an SXML XSLT tag."
  [fn-name tag args & expansion]
  (let [tag (if (= '_ tag) fn-name tag)]
    `(defn ~fn-name
       ~args
       (reduce conj [~(keyword "xsl" (name tag))]
               ~@expansion))))

(defmacro ^{:private true} def-trivial-tags
  "Define all trivial tags, which accept no positional attributes but do accept
  map attributes and child elements."
  [& tags]
  (conj (for [tag tags]
          `(deftag ~tag ~'_ [& children#] children#))
        `do))

(defn- syms->key-sym-map
  "Take a sequence of symbols and return a map from keywords to symbols, where
  each symbol is keyed by a keyword of the same name."
  [syms]
  (into {} (for [sym syms]
             [(keyword (name sym)) sym])))

(defmacro ^{:private true} def-pos-tag
  "Define a tag which accepts positional arguments and converts them into
  attributes, and which may accept child elements."
  [fn-name tag args]
  (let [children-sym (gensym "children_")
        arg-list     (vec (concat args ['& children-sym]))
        pos-attrs    (syms->key-sym-map args)]
    `(deftag ~fn-name ~tag ~arg-list
       (let [[maybe-more-attrs# & other-children#] ~children-sym
             more-attrs?# (map? maybe-more-attrs#)
             attr-map# (if more-attrs?#
                         (merge ~pos-attrs maybe-more-attrs#)
                         ~pos-attrs)]
         (if more-attrs?#
           (conj other-children# attr-map#)
           (conj ~children-sym attr-map#))))))

(defmacro ^{:private true} def-pos-tag-empty
  "Define a tag with positionally-passed attributes that accepts no child
  elements."
  [fn-name tag args]
  (let [opt-attrs-sym (gensym "opt-attrs_")
        arg-list      (vec (concat args ['& opt-attrs-sym]))
        pos-attrs     (syms->key-sym-map args)]
    `(deftag ~fn-name ~tag ~arg-list
       [(merge ~pos-attrs ~opt-attrs-sym)])))

(def-trivial-tags comment copy decimal-format fallback message number
                  #_otherwise output sort text)

(def-pos-tag-empty apply-imports _ [])
(def-pos-tag apply-templates-to apply-templates [select])
(def-pos-tag apply-templates _ [])
(def-pos-tag attribute _ [name])
(def-pos-tag attribute-set _ [name])
(def-pos-tag call-template _ [name])
;(def-pos-tag choose _ [])
(def-pos-tag-empty copy-of _ [select])
(def-pos-tag element _ [name])
(def-pos-tag for-each _ [select])
(def-pos-tag if* if [test])
(def-pos-tag-empty import _ [href])
(def-pos-tag-empty include _ [href])
(def-pos-tag key _ [name match use])
(def-pos-tag namespace-alias _ [stylesheet-prefix result-prefix])
(def-pos-tag param _ [name])
(def-pos-tag-empty preserve-space _ [elements])
(def-pos-tag processing-instruction _ [name])
(def-pos-tag-empty strip-space _ [elements])
(def-pos-tag stylesheet* stylesheet [version])
(def-pos-tag match-template template [match])
(def-pos-tag named-template template [name])
(def-pos-tag transform _ [version])
(def-pos-tag-empty value-of _ [select])
(def-pos-tag variable _ [name])
;(def-pos-tag when _ [test])
(def-pos-tag with-param _ [name])

(defn cond*
  "Create an xsl:choose form using a likeness of ordinary Clojure cond syntax.
  The body of the form is a sequence of condition/consequent forms, where the
  condition is a string containing an XSLT predicate expression, and the
  consequent is a vector of SXML forms to appear inside that xsl:when (or
  xsl:otherwise if the condition is the keyword :else). For convenience, if the
  consequent is a SXML tag element (that is, a vector starting with a keyword),
  or not a vector at all, it will be treated as a sequence containing that
  single value, so that one may directly write consequents that contain only
  single elements or text nodes."
  [& forms]
  (let [consequent-singular? (fn [e] (not (and (vector? e)
                                               (-> e first keyword? not))))
        clauses (for [[condition consequent] (partition 2 forms)]
                  (let [consequent (if (consequent-singular? consequent)
                                     [consequent]
                                     consequent)]
                  (if (= :else condition)
                    (vec (concat [:xsl/otherwise] consequent))
                    (vec (concat [:xsl/when {:test condition}] consequent)))))]
    (vec (concat [:xsl/choose] clauses))))

(defn stylesheet
  "Special sauce for the stylesheet tag, which needs to merge the XML namespace
  with any optional extra attribute map."
  [version & args]
  (let [[maybe-attrs & rest-args] args
        extra-attrs? (map? maybe-attrs)
        xmlns {:xmlns/xsl xslt-ns}]
    (apply stylesheet*
      (if extra-attrs?
        (concat [version (merge xmlns maybe-attrs)] rest-args)
        (concat [version xmlns] args)))))

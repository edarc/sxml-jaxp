(ns sxml-sax.core
  "Tools for using SXML-inspired XML representations with SAX2."
  (:import
    (org.xml.sax SAXNotRecognizedException XMLReader InputSource)
    (org.xml.sax.helpers AttributesImpl)
    (javax.xml.transform.sax SAXSource)))

(def ^{:doc "The default map of XML namespace prefixes. This is referenced when
            SAX events are generated to resolve namespace prefixes that are not
            defined inline with xmlns attributes. Essentially, xmlns attributes
            declaring these namespaces are automatically inserted into the root
            element before parsing begins."}
  *default-xmlns* {})

(def ^{:private true
       :doc "The internal map of XML namespace prefixes. This accumulates
            namespace definitions as they are encountered during traversal of
            the SXML tree. This map is referred to when resolving the namespace
            URI of elements and attributes." }
  *xmlns* {})

(defn- all-of
  "Takes a variable number of predicates and returns a predicate P which will
  only be true when all of the predicates applied to P's arguments are true."
  [& preds]
  (fn [& x] (every? #(apply % x) preds)))

(defn- any-of
  "Takes a variable number of predicates and returns a predicate P which will
  be true if at least one of the predicates applied to P's arguments is true."
  [& preds]
  (fn [x] (some #(% x) preds)))

(defn normalize
  "Return the normalized, long form of the given SXML representation. All tag
  elements will be of the form [tag attrs & children], where tag is a keyword
  naming the element, attrs is a possibly-empty map of attributes, and children
  is a possibly empty sequence of child elements."
  [form]
  (cond
    (vector? form)
    (vec (let [[tag maybe-attrs & tail] form]
           (if (map? maybe-attrs)
             (concat [tag maybe-attrs] (map normalize tail))
             (concat [tag {}] (map normalize (conj tail maybe-attrs))))))
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

(defn- qualify-name
  "Accepts a keyword representing a tag or attribute name, and returns [qname
  lname uri], where qname is the qualified name, lname is the local name, and
  uri is the URI of the namespace it belongs to, as defined by the current
  value of *xmlns*."
  [kw]
  (let [qname          (name kw)
        [before after] (.split qname ":")
        prefix-kw      (when after (keyword before))
        lname          (or after qname)
        uri            (if (attr-is-xmlns kw) ""
                         (or (*xmlns* prefix-kw) ""))]
    [qname lname uri prefix-kw]))

(defn- apply-namespaces
  "Inserts xmlns attributes into the root element of form which declare the
  namespaces currently defined in *xmlns*. Assumes form is normalized."
  [form]
  (let [xmlnsify (fn [kw] (if kw
                            (keyword (str "xmlns:" (name kw)))
                            :xmlns))
        xmlns    (into {} (for [[k v] *xmlns*] [(xmlnsify k) v]))
        [root-tag attrs & content] form]
    (concat [root-tag] [(merge xmlns attrs)] content)))

(defn- attr-is-xmlns
  "Predicate which returns true if the given keyword (corresponding to an
  attribute name) is an XML namespace declaration."
  [kw]
  (or (= :xmlns kw)
      (.startsWith (name kw) "xmlns:")))

(defn- de-xmlnsify
  "Accepts a keyword corresponding to an xmlns attribute and returns a keyword
  representing the namespace prefix itself."
  [kw]
  (when-not (= kw :xmlns)
    (keyword (.substring (name kw) 6))))

(defn- mk-sax-attributes
  "Given an SXML attribute map, produce a SAX Attributes instance representing
  them. The function must also know the tag's namespace prefix, so that
  unqualified attributes will be assigned to the correct namespace URI."
  [tag-prefix attr-map]
  (binding [*xmlns* (merge *xmlns* {nil (*xmlns* tag-prefix)})]
    (let [attrs (AttributesImpl.)]
      (doseq [[attr-kw attr-val] attr-map]
        (let [[q l u _] (qualify-name attr-kw)]
          (.addAttribute attrs u l q "CDATA" attr-val)))
      attrs)))

(defn- fire-events*
  "Given an SXML form and SAX ContentHandler, walk the form firing events on
  the ContentHandler corresponding to the XML that the given form represents.
  This only fires events appropriate for non-root elements, so that it may call
  itself recursively."
  [form ch]
  (cond
    ((any-of vector? seq?) form)
    (let [[tag attrs & children] form
          [q l u tag-prefix] (qualify-name tag)
          xmlns-decls (into {} (for [k (filter attr-is-xmlns (keys attrs))]
                                 [(de-xmlnsify k) (attrs k)]))]
      (binding [*xmlns* (merge *xmlns* xmlns-decls)]
        (doseq [[m _] xmlns-decls] (.startPrefixMapping
                                     ch (if m (name m) "") (*xmlns* m)))
        (.startElement ch u l q (mk-sax-attributes tag-prefix attrs))
        (doseq [child children] (fire-events* child ch))
        (.endElement ch u l q)
        (doseq [[m _] xmlns-decls] (.endPrefixMapping ch (if m (name m) "")))))
    (string? form) (.characters ch (char-array form) 0 (.length form))
    :else (recur (str form) ch)))

(defn- fire-events
  "Applies default namespace declarations, fires events appropriate for the
  root element of the given SXML form, then hands off to 'fire-events* to
  generate all other SAX events."
  [form ch]
  (.startDocument ch)
  (binding [*xmlns* *default-xmlns*]
    (fire-events* (apply-namespaces form) ch))
  (.endDocument ch))

(defn sax-reader
  "Produce a SAX XMLReader instance which, when .parse is invoked, will
  generate SAX events corresponding to the XML represented by the given SXML
  form.
  
  Note 1: The XMLReader interface specifies that the .parse method may only
  take an argument of a type which is useless for the purpose of this library,
  therefore the form which is to be read is simply captured in the instance,
  and the argument to .parse is ignored.
  
  Note 2: SAX mandates that XMLReader accept setFeature requests for two
  features, in particular 'namespaces' and 'namespace-prefixes'. However, as
  far as the author can determine, when each feature is on, the corresponding
  behavior is compulsory, but when it is off it is optional. So this
  implementation simply ignores the feature setting and always enables these
  two behaviors, which should still be compliant. This may change in the future
  if necessary."
  [form]
  (let [content-handler (atom nil)
        entity-resolver (atom nil)
        dtd-handler     (atom nil)
        err-handler     (atom nil)
        form            (normalize form)
        not-recognized!
        #(throw (SAXNotRecognizedException.
                  "SXML-SAX source has no such feature or property."))]
    (reify
      XMLReader
      (getContentHandler [_] @content-handler)
      (setFeature [_ feat mode]
        (condp = feat
          "http://xml.org/sax/features/namespaces" nil
          "http://xml.org/sax/features/namespace-prefixes" nil
          (not-recognized!)))
      (getFeature [_ feat]
        (condp = feat
          "http://xml.org/sax/features/namespaces" true
          "http://xml.org/sax/features/namespace-prefixes" true
          (not-recognized!)))
      (setProperty [_ _ _] (not-recognized!))
      (getProperty [_ _] (not-recognized!))
      (setEntityResolver [_ er] (reset! entity-resolver er))
      (getEntityResolver [_] @entity-resolver)
      (setDTDHandler [_ dh] (reset! dtd-handler dh))
      (getDTDHandler [_] @dtd-handler)
      (setContentHandler [_ ch] (reset! content-handler ch))
      (setErrorHandler [_ eh] (reset! err-handler eh))
      (getErrorHandler [_] @err-handler)
      (^void parse [_ ^InputSource _] (fire-events form @content-handler)))))

(defn sax-source
  "Produce a SAXSource for use in XML transformations which supplies SAX events
  corresponding to the XML represented by the given form. This first creates a
  SAX reader and then instantiates a source using a dummy InputSource, which is
  simply ignored by the SXML reader."
  [form]
  (let [reader (sax-reader form)]
    (SAXSource. reader (InputSource. ""))))

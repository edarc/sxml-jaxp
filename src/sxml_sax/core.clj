(ns sxml-sax.core
  "Tools for using SXML-inspired XML representations with SAX2."
  (:import
    (org.xml.sax SAXNotRecognizedException XMLReader InputSource
                 ContentHandler Attributes)
    (org.xml.sax.helpers AttributesImpl)
    (javax.xml.transform.sax SAXSource SAXResult)))

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

(defn- attr-is-xmlns?
  "Predicate which returns true if the given keyword (corresponding to an
  attribute name) is an XML namespace declaration."
  [kw]
  (or (= :xmlns kw)
      (.startsWith (name kw) "xmlns:")))

(defn- qualify-name
  "Accepts a keyword representing a tag or attribute name, and returns [qname
  lname uri prefix-kw], where qname is the qualified name, lname is the local
  name, uri is the URI of the namespace it belongs to, as defined by the
  current value of *xmlns*, and prefix-kw is a keywordized version of the
  namespace prefix."
  [kw]
  (let [qname          (name kw)
        [before after] (.split qname ":")
        prefix-kw      (when after (keyword before))
        lname          (or after qname)
        uri            (if (attr-is-xmlns? kw) ""
                         (or (*xmlns* prefix-kw) ""))]
    [qname lname uri prefix-kw]))

(defn- xmlnsify
  "Converts a keyword naming an XML namespace prefix and converts it into a
  keyword representing the attribute name of an xmlns declaration."
  [kw]
  (if kw
    (keyword (str "xmlns:" (name kw)))
    :xmlns))

(defn- apply-namespaces
  "Inserts xmlns attributes into the root element of form which declare the
  namespaces currently defined in *xmlns*. Assumes form is normalized."
  [form]
  (let [xmlns (into {} (for [[k v] *xmlns*] [(xmlnsify k) v]))
        [root-tag attrs & content] form]
    (concat [root-tag] [(merge xmlns attrs)] content)))

(defn- de-xmlnsify
  "Accepts a keyword corresponding to an xmlns attribute and returns a keyword
  representing the namespace prefix itself."
  [kw]
  (when-not (= kw :xmlns)
    (keyword (.substring (name kw) 6))))

(defn- make-sax-attributes
  "Given an SXML attribute map, produce a SAX Attributes instance representing
  them. The function must also know the tag's namespace prefix, so that
  unqualified attributes will be assigned to the correct namespace URI."
  [tag-prefix attr-map]
  (let [attrs (AttributesImpl.)]
    (doseq [[attr-kw attr-val] attr-map]
      (let [[q l u _] (qualify-name attr-kw)]
        (.addAttribute attrs u l q "CDATA" attr-val)))
    attrs))

(defn- sax-event-seq*
  "Given an SXML form, produce a sequence of events corresponding to the SAX
  events that represent the corresponding XML."
  [form]
  (cond
    ((any-of vector? seq?) form)
    (let [[tag attrs & children] form
          xmlns-decls (into {} (for [k (filter attr-is-xmlns? (keys attrs))]
                                 [(de-xmlnsify k) (attrs k)]))]
      (binding [*xmlns* (merge *xmlns* xmlns-decls)]
        (let [xmlns *xmlns*]
          (concat
            (for [[m _] xmlns-decls] [:start-prefix m (xmlns m)])
            [[:start-element tag attrs]]
            (apply concat (for [child children] (sax-event-seq* child)))
            [[:end-element tag]]
            (for [[m _] xmlns-decls] [:end-prefix m])))))
    (string? form) [[:text-node form]]
    :else (recur (str form))))

(def ^{:doc "Given an SXML form, produce a sequence of events corresponding to
            the SAX events that represent the corresponding XML."
       :arglists '([form])}
  sax-event-seq (comp sax-event-seq* normalize))

(defn- fire-events*
  "Given a SAX event seq and a SAX ContentHandler, iterate through the events,
  firing the appropriate handler methods. This does not fire start and end
  document events."
  [event-seq ^ContentHandler ch]
  (doseq [[ev & params] event-seq]
    (case ev
      :start-prefix
      (let [[prefix uri] params
            prefix (if prefix (name prefix) "")]
        (.startPrefixMapping ch prefix uri))
      :start-element
      (let [[tag attrs] params
            [q l u tag-prefix] (qualify-name tag)]
        (.startElement ch u l q (make-sax-attributes tag-prefix attrs)))
      :text-node
      (let [^String text (first params)]
        (.characters ch (char-array text) 0 (.length text)))
      :end-element
      (let [tag (first params)
            [q l u _] (qualify-name tag)]
        (.endElement ch u l q))
      :end-prefix
      (let [prefix (first params)
            prefix (if prefix (name prefix) "")]
        (.endPrefixMapping ch prefix)))))

(defn fire-events
  "Applies default namespace declarations, fires events appropriate for the
  root element of the given SXML form, then hands off to 'fire-events* to
  generate all other SAX events."
  [form ^ContentHandler ch]
  (.startDocument ch)
  (binding [*xmlns* *default-xmlns*]
    (fire-events* (sax-event-seq* (apply-namespaces (normalize form))) ch))
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
        not-recognized!
        #(throw (SAXNotRecognizedException.
                  "SXML-SAX source has no such feature or property."))
        default-xmlns   *default-xmlns*]
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
      (^void parse [_ ^InputSource _]
         (binding [*default-xmlns* default-xmlns]
           (fire-events form @content-handler))))))

(defn sax-source
  "Produce a SAXSource for use in XML transformations which supplies SAX events
  corresponding to the XML represented by the given form. This first creates a
  SAX reader and then instantiates a source using a dummy InputSource, which is
  simply ignored by the SXML reader."
  [form]
  (let [reader (sax-reader form)]
    (SAXSource. reader (InputSource. ""))))

(defn push-text
  "Push a text node onto an SXML parser stack."
  [stack text]
  (conj stack text))

(defn push-prefix
  "Push a an XML namespace prefix declaration onto an SXML parser stack. All
  such prefix declarations are inserted onto the element immediately following
  when the element is reduced."
  [stack prefix uri]
  (conj stack [::start-prefix prefix uri]))

(defn push-element
  "Push a start-element marker along with attributes onto an SXML parser stack.
  Any namespace prefix delcarations immediately preceding the start-element
  marker will be inserted onto the element when it is reduced."
  [stack tag attrs]
  (conj stack [::start-element tag attrs]))

(defn reduce-element
  "Reduce an element on an SXML parser stack. When an element-end event is
  received, the element is reduced by searching backwards for the matching
  start-element marker, and insert all stack entries above it as children. Any
  namespace prefix declarations immediately preceding the start-element marker
  are inserted into the element's attributes."
  ([stack tag] (reduce-element stack tag ()))
  ([stack tag children]
   (let [[top & remain] stack
         start-mark? (and (vector? top)
                          (->> top (take 2) (= [::start-element tag])))]
     (when (nil? top) (throw (IllegalStateException. "Stack underflow.")))
     (if start-mark?
       (reduce-element remain tag (nth top 2) children)
       (recur remain tag (conj children top)))))
  ([stack tag attrs children]
   (let [[top & remain] stack
         prefix-mark? (and (vector? top)
                           (->> top first (= ::start-prefix)))]
     (if prefix-mark?
       (let [[_ prefix uri] top]
         (recur remain tag (assoc attrs (xmlnsify prefix) uri) children))
       (conj stack (vec (concat [tag attrs] children)))))))

(defn extract-sax-attributes
  "Convert a SAX2 Attributes object into an SXML attribute map."
  [^Attributes attrs]
  (into {} (for [index (range (.getLength attrs))]
             [(keyword (.getQName attrs index))
              (.getValue attrs ^Integer index)])))

(defn sax-handler
  "Produce a SAX ContentHandler instance, plus an atom. Initially the atom
  contains an empty list (the stack). The handler implements a shift-reduce
  parser, and as parsing events occur, tokens are shifted onto the stack, and
  portions of the top-of-stack reduced into SXML elements. When the document
  ends (assuming it was well-formed), the stack contains a single SXML element
  (the root element), which replaces the stack as the atom's value."
  []
  (let [sxml-stack (atom ())]
    [sxml-stack
     (reify
       ContentHandler
       (startDocument [_] nil)
       (startPrefixMapping [_ prefix uri]
          (swap! sxml-stack push-prefix (keyword prefix) uri))
       (startElement [_ _ _ qname attrs]
          (swap! sxml-stack push-element (keyword qname)
                 (extract-sax-attributes attrs)))
       (characters [_ ch start length]
          (swap! sxml-stack push-text (String. ch start length)))
       (ignorableWhitespace [_ _ _ _] nil)
       (processingInstruction [_ _ _] nil)
       (setDocumentLocator [_ _] nil)
       (skippedEntity [_ _] nil)
       (endElement [_ _ _ qname]
          (swap! sxml-stack reduce-element (keyword qname)))
       (endPrefixMapping [_ _] nil)
       (endDocument [_]
          (swap! sxml-stack first)))]))

(defn sax-result
  "As with sax-handler, except wraps the handler in a SAXResult instance, for
  use with XSL transformations."
  []
  (let [[result handler] (sax-handler)]
    [result (SAXResult. handler)]))

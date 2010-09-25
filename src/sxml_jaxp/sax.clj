(ns sxml-jaxp.sax
  "Implements the details of parsing and emitting SXML via SAX."
  (:use [sxml-jaxp.core :only [normalize xmlnsify apply-namespaces prefix-decls
                               attr-is-xmlns?]])
  (:import
    (org.xml.sax SAXNotRecognizedException ContentHandler XMLReader InputSource
                 Attributes)
    (org.xml.sax.helpers AttributesImpl)
    (javax.xml.parsers SAXParserFactory)
    (javax.xml.transform.sax SAXSource SAXResult)
    (java.io Reader InputStream File FileReader StringReader)))

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

(defn- qualify-name
  "Accepts a keyword representing a tag or attribute name, and returns [qname
  lname uri prefix-kw], where qname is the qualified name, lname is the local
  name, uri is the URI of the namespace it belongs to as defined by the prefix
  mappings in prefixes, and prefix-kw is a keywordized version of the namespace
  prefix."
  [prefixes kw]
  (let [lname          (name kw)
        prefix         (namespace kw)
        qname          (if prefix
                         (str prefix ":" lname)
                         lname)
        prefix-kw      (keyword prefix)
        uri            (if (attr-is-xmlns? kw) ""
                         (or (prefixes prefix-kw) ""))]
    [qname lname uri prefix-kw]))

(defn- qname-to-kw
  "Convert an XML qualified name to a keyword."
  [qn]
  (let [[before after] (.split qn ":")
        prefix         (when after before)
        lname          (or after before)]
    (keyword prefix lname)))

(defn- make-sax-attributes
  "Given an SXML attribute map, produce a SAX Attributes instance representing
  them. The function must also know the current namespace prefixes, so that
  qualified attributes will be assigned to the correct namespace URI."
  [prefixes attr-map]
  (let [attrs (AttributesImpl.)]
    (doseq [[attr-kw attr-val] attr-map]
      (let [[q l u _] (qualify-name prefixes attr-kw)]
        (.addAttribute attrs u l q "CDATA" (str attr-val))))
    attrs))

(defn- sax-event-seq*
  "Given a normalized SXML form, produce a sequence of events corresponding to
  the SAX events that represent the equivalent XML."
  [form]
  (cond
    (vector? form)
    (let [[tag attrs & children] form
          new-prefixes           (prefix-decls form)
          merged-prefixes        (merge *xmlns* new-prefixes)
          ev (reduce conj! (transient [])
                     (for [[m u] new-prefixes] [:start-prefix m u]))
          ev (if (seq new-prefixes)
               (conj! ev [:prefix-map merged-prefixes])
               ev)
          ev (conj! ev [:start-element tag attrs])
          ev (binding [*xmlns* merged-prefixes]
               (reduce (partial reduce conj!) ev
                       (map sax-event-seq* children)))
          ev (conj! ev [:end-element tag])
          ev (if (seq new-prefixes)
               (conj! ev [:prefix-map *xmlns*])
               ev)
          ev (reduce conj! ev (for [[m _] new-prefixes] [:end-prefix m]))]
      (persistent! ev))
    :else [[:text-node form]]))

(defn sax-event-seq
  "Given an SXML form (which must have a root element), produce a sequence of
  events corresponding to the SAX events that represent the corresponding XML."
  [form]
  (-> form normalize (apply-namespaces *default-xmlns*) sax-event-seq*))

(defn- fire-events*
  "Given a SAX event seq and a SAX ContentHandler, iterate through the events,
  firing the appropriate handler methods. This does not fire start and end
  document events."
  ([event-seq ^ContentHandler ch] (fire-events* event-seq ch {}))
  ([event-seq ^ContentHandler ch prefix-map]
   (when-let [[ev & params] (first event-seq)]
     (case ev
       :start-prefix
       (let [[prefix uri] params
             prefix (if prefix (name prefix) "")]
         (.startPrefixMapping ch prefix uri)
         (recur (next event-seq) ch prefix-map))
       :start-element
       (let [[tag attrs] params
             [q l u _] (qualify-name prefix-map tag)]
         (.startElement ch u l q (make-sax-attributes prefix-map attrs))
         (recur (next event-seq) ch prefix-map))
       :text-node
       (let [^String text (str (first params))]
         (.characters ch (.toCharArray text) 0 (.length text))
         (recur (next event-seq) ch prefix-map))
       :end-element
       (let [tag (first params)
             [q l u _] (qualify-name prefix-map tag)]
         (.endElement ch u l q)
         (recur (next event-seq) ch prefix-map))
       :end-prefix
       (let [prefix (first params)
             prefix (if prefix (name prefix) "")]
         (.endPrefixMapping ch prefix)
         (recur (next event-seq) ch prefix-map))
       :prefix-map
       (let [new-prefix-map (first params)]
         (recur (next event-seq) ch new-prefix-map))))))

(defn fire-events
  "Applies default namespace declarations, fires events appropriate for the
  root element of the given SXML form, then hands off to 'fire-events* to
  generate all other SAX events."
  [form ^ContentHandler ch]
  (.startDocument ch)
  (if (= (type form) ::compiled-sxml)
    (fire-events* form ch)
    (fire-events* (sax-event-seq form) ch))
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

(defn coalesce-text
  "Coalesce a contiguous series of text nodes at the top of the SXML parser
  stack into a single string. This is needed because many SAX parsers emit long
  strings of 'characters' events in succession, often with very short payloads,
  especially when there are many entity references in the source document."
  [stack]
  (if (->> stack (take 2) (every? string?))
    (loop [strs ()
           [top & tail] stack]
      (if (string? top)
        (recur (conj strs top) tail)
        (-> tail
          (conj top)
          (conj (apply str strs)))))
    stack))

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
  (conj (coalesce-text stack) [::start-element tag attrs]))

(defn reduce-element
  "Reduce an element on an SXML parser stack. When an element-end event is
  received, the element is reduced by searching backwards for the matching
  start-element marker, and insert all stack entries above it as children. Any
  namespace prefix declarations immediately preceding the start-element marker
  are inserted into the element's attributes."
  ([stack tag] (reduce-element (coalesce-text stack) tag ()))
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
             [(qname-to-kw (.getQName attrs index))
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
          (swap! sxml-stack push-element (qname-to-kw qname)
                 (extract-sax-attributes attrs)))
       (characters [_ ch start length]
          (swap! sxml-stack push-text (String. ch start length)))
       (ignorableWhitespace [_ _ _ _] nil)
       (processingInstruction [_ _ _] nil)
       (setDocumentLocator [_ _] nil)
       (skippedEntity [_ _] nil)
       (endElement [_ _ _ qname]
          (swap! sxml-stack reduce-element (qname-to-kw qname)))
       (endPrefixMapping [_ _] nil)
       (endDocument [_]
          (swap! sxml-stack first)))]))

(defn sax-result
  "As with sax-handler, except wraps the handler in a SAXResult instance, for
  use with XSL transformations."
  []
  (let [[result handler] (sax-handler)]
    [result (SAXResult. handler)]))

(defn compile-sxml
  "Given an SXML form, compile the SAX event sequence and return it,
  type-tagged. Pre-compiled SXML can be used where the result will be emitted
  as SAX events."
  [form]
  (with-meta (vec (sax-event-seq form)) {:type ::compiled-sxml}))

(defmacro compiled-sxml
  "This macro will pre-compile, as a speed optimization, an SXML literal that
  is to be used as input to a JAXP API. Expressions may appear in the SXML form
  in place of text nodes, and cannot affect the element structure. Attributes
  must be literal maps for now, but may contain expressions in both key and
  value positions."
  [form]
  (compile-sxml form))

(derive clojure.lang.IPersistentVector ::sxml)
(derive clojure.lang.LazySeq ::sxml)

(defmulti ^{:private true} get-xml-reader
  "Get the appropriate kind of XMLReader for the given input source type."
  class)
;; I have no idea why you would want to do this, but might as well be
;; orthogonal as possible.
(defmethod get-xml-reader ::sxml [sxml]
  (sax-reader sxml))
(defmethod get-xml-reader Object [_]
  (.. SAXParserFactory (newInstance) (newSAXParser) (getXMLReader)))

(defmulti ^{:private true} to-parser-input
  "Convert the input source to the appropriate type for passing to the
  XMLReader for parsing."
  class)
(defmethod to-parser-input ::sxml [_] nil)
(defmethod to-parser-input InputStream [^InputStream is]
  (InputSource. is))
(defmethod to-parser-input Reader [^Reader r]
  (InputSource. r))
(defmethod to-parser-input File [^File f]
  (to-parser-input (FileReader. f)))
(defmethod to-parser-input String [^String s]
  (to-parser-input (StringReader. s)))

(defn read-sxml
  "Convert some source of XML data to an SXML structure."
  [src]
  (let [[output handler] (sax-handler)
        ^XMLReader reader (get-xml-reader src)
        ^InputSource input (to-parser-input src)]
    (doto reader
      (.setContentHandler handler)
      (.parse input))
    @output))

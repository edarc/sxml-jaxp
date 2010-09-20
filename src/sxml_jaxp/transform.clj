(ns sxml-jaxp.transform
  "Tools for using SXML to perform XML transformations."
  (:require [sxml-jaxp.transform.xslt :as xsl])
  (:require [sxml-jaxp.sax :as sax])
  (:import
    (javax.xml.transform TransformerFactory Transformer Templates Source
                         Result)
    (javax.xml.transform.stream StreamSource StreamResult)
    (java.io InputStream OutputStream Reader Writer File StringReader
             StringWriter)))

(defn identity-stylesheet
  "Create an SXML XSLT template that is the identity transform, that is it
  copies the source document to the output document unchanged. In general this
  is only useful for testing; use copy! if you really want to accomplish this."
  []
  (xsl/stylesheet "1.0"
    (xsl/match-template "@*|node()"
      (xsl/copy (xsl/apply-templates-to "@*|node()")))))

(defn identity-transformer
  "Create a Transformer that copies the source to the result without any
  transformation."
  []
  (.. (TransformerFactory/newInstance) (newTransformer)))

(derive clojure.lang.IPersistentVector ::sxml)

(defmulti to-source
  "Convert any sort of thing that might function as a source of XML into a
  Source object for transforms."
  class)
(defmethod to-source ::sxml [sx] (sax/sax-source sx))
(defmethod to-source Source [s] s)
(defmethod to-source InputStream [^InputStream ir] (StreamSource. ir))
(defmethod to-source Reader [^Reader r] (StreamSource. r))
(defmethod to-source File [^File f] (StreamSource. f))
(defmethod to-source String [s] (to-source (StringReader. s)))

(derive OutputStream ::adapt-with-streamresult)
(derive Writer ::adapt-with-streamresult)
(derive File ::adapt-with-streamresult)

(defmulti to-result
  "Convert any sort of thing that might function as a sink for XML into a
  vector, the first entry of which is the Result object for transforms. The
  vector may have type metadata for dispatch in the from-result multimethod,
  where any additional entries in the vector may be used to produce the
  result."
  (fn [arg] (if (keyword? arg) arg (class arg))))

(defmethod to-result Result [sr]
  ^{:type ::literal-result} [sr])

(defmethod to-result ::adapt-with-streamresult [r]
  ^{:type ::streamresult-adapted} [(StreamResult. r) r])

(defmethod to-result :string [_]
  (let [sw       (StringWriter.)
        [result] (to-result sw)]
    ^{:type ::string-output} [result sw]))

(defmethod to-result :sxml [_]
  (let [[output result] (sax/sax-result)]
    ^{:type ::sxml-output} [result output]))

(defmulti from-result
  "Post-process a result vector (as generated by to-result) after a
  transformation to yield the output value."
  type)
(defmethod from-result ::literal-result [r] (first r))
(defmethod from-result ::streamresult-adapted [sra] (second sra))
(defmethod from-result ::string-output [sw] (.toString ^StringWriter (second sw)))
(defmethod from-result ::sxml-output [r] @(second r))

(defn compile-template
  "Pre-compile an XSL template into a Templates object. The pre-compiled
  template can be used as the stylesheet argument to transform!, which will
  prevent it having to parse and compile the template on each invocation."
  [ss]
  (binding [sax/*default-xmlns* (assoc sax/*default-xmlns* :xsl
                                       "http://www.w3.org/1999/XSL/Transform")]
    (.. (TransformerFactory/newInstance)
      (newTemplates (to-source ss)))))

(defmulti ^{:private true} transformer
  "Create an XSLT Transformer from any sort of thing that might function as a
  source of XML containing the stylesheet."
  class)

(defmethod transformer Transformer [t] t)
(defmethod transformer Templates [^Templates t] (.newTransformer t))
(defmethod transformer Object [o] (transformer (compile-template o)))

(defn transform!
  "Perform XSL Transformations using the given stylesheet and source document
  (either of which can be any type of thing that can function as a source of
  XML data).

  When a result argument is given, the transform is targeted to it and the
  function returns that object. You may pass :string as the result to have the
  outpu in string format, or :sxml to get the result in SXML format. When no
  result argument is given, it defaults to :sxml."
  ([stylesheet source] (transform! stylesheet source :sxml))
  ([stylesheet source result]
   (let [result (to-result result)]
     (.transform ^Transformer (transformer stylesheet)
                 (to-source source)
                 (first result))
     (from-result result))))

(defn copy!
  "Like transform!, but accepts no stylesheet and performs no transformation,
  simply copies the source to the result. You may pass :string as the result to
  have the output returned in string format (the default when no result
  argument is given), or :sxml to get the result in SXML format (in which case
  you probably want to use sxml-jaxp.sax/read-sxml which is likely to be more
  efficient)."
  ([source] (copy! source :string))
  ([source result] (transform! (identity-transformer) source result)))

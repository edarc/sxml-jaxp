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

(defprotocol TransformSource
  (make-jaxp-source [obj]
     "Adapt obj into an instance of javax.xml.transform.Source."))

(extend-protocol TransformSource
  clojure.lang.IPersistentVector
    (make-jaxp-source [sx] (sax/sax-source sx))
  clojure.lang.LazySeq
    (make-jaxp-source [sx] (sax/sax-source sx))
  Source
    (make-jaxp-source [s] s)
  InputStream
    (make-jaxp-source [is] (StreamSource. is))
  Reader
    (make-jaxp-source [r] (StreamSource. r))
  File
    (make-jaxp-source [f] (StreamSource. f))
  String
    (make-jaxp-source [s] (make-jaxp-source (StringReader. s))))

(defprotocol TransformResult
  (adapt-jaxp-result [obj]
     "Adapt obj into [res state], where res is an instance of
     javax.xml.transform.Result, and state is another object implementing
     TransformResult. The state object will be passed to unadapt-jaxp-result to
     produce the return value of the transform.")
  (unadapt-jaxp-result [state]
     "Extract the resulting value of the transform from the state object
     returned by adapt-jaxp-result."))

(defrecord StreamResultAdapter [streamresult wrapped-obj]
  TransformResult
  (adapt-jaxp-result [sra] [streamresult sra])
  (unadapt-jaxp-result [sra] wrapped-obj))

(defrecord StringAdapter [result stringwriter]
  TransformResult
  (adapt-jaxp-result [sa] [result sa])
  (unadapt-jaxp-result [sa] (.toString ^StringWriter stringwriter)))

(defrecord SXMLAdapter [saxresult output-atom]
  TransformResult
  (adapt-jaxp-result [sa] [saxresult sa])
  (unadapt-jaxp-result [sa] @output-atom))

(defn- wrap-stream-result [obj]
  (let [streamresult (StreamResult. obj)
        state (StreamResultAdapter. streamresult obj)]
    [streamresult state]))

(extend-protocol TransformResult
  ; These types are wrapped with StreamResult, so the state is held in a
  ; StreamResultAdapter. Unadapt is correctly implemented as identity, but it
  ; isn't used.
  OutputStream
    (adapt-jaxp-result [os] (wrap-stream-result os))
    (unadapt-jaxp-result [os] os)
  Writer
    (adapt-jaxp-result [w] (wrap-stream-result w))
    (unadapt-jaxp-result [w] w)
  File
    (adapt-jaxp-result [f] (wrap-stream-result f))
    (unadapt-jaxp-result [f] f)
  ; Result objects are their own state, which unwraps with identity.
  Result
    (adapt-jaxp-result [r] [r r])
    (unadapt-jaxp-result [r] r))

(defn to-result [obj]
  (condp = obj
    :sxml
    (let [[result output-atom] (sax/sax-result)]
      [result (SXMLAdapter. result output-atom)])
    :string
    (let [stringwriter (StringWriter.)
          [result _] (adapt-jaxp-result stringwriter)]
      [result (StringAdapter. result stringwriter)])
    (adapt-jaxp-result obj)))

(defn from-result [obj]
  (unadapt-jaxp-result obj))

(defn compile-template
  "Pre-compile an XSL template into a Templates object. The pre-compiled
  template can be used as the stylesheet argument to transform!, which will
  prevent it having to parse and compile the template on each invocation."
  [ss]
  (.. (TransformerFactory/newInstance)
      (newTemplates (make-jaxp-source ss))))

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
  ([stylesheet source] (transform! stylesheet source :sxml nil))
  ([stylesheet source result] (transform! stylesheet source result nil))
  ([stylesheet source result output-properties]
   (let [[jaxp-result state] (to-result result)
         xfm (transformer stylesheet)]
     (when output-properties
       (doseq [[prop value] output-properties]
         (.setOutputProperty xfm prop value)))
     (.transform ^Transformer xfm
                 (make-jaxp-source source)
                 jaxp-result)
     (from-result state))))

(defn copy!
  "Like transform!, but accepts no stylesheet and performs no transformation,
  simply copies the source to the result. You may pass :string as the result to
  have the output returned in string format (the default when no result
  argument is given), or :sxml to get the result in SXML format (in which case
  you probably want to use sxml-jaxp.sax/read-sxml which is likely to be more
  efficient)."
  ([source] (copy! source :string))
  ([source result] (transform! (identity-transformer) source result)))

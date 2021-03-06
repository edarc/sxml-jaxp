(ns sxml-jaxp.test.transform
  (:use [sxml-jaxp.core :only [normalize]] :reload)
  (:use [sxml-jaxp.sax :only [sax-source sax-result *default-xmlns*]] :reload)
  (:use [sxml-jaxp.transform] :reload)
  (:require [sxml-jaxp.transform.xslt :as xsl] :reload)
  (:use [clojure.test])
  (:import
    (javax.xml.transform.sax SAXSource)
    (java.io InputStream OutputStream Reader Writer File StringReader
             StringWriter ByteArrayInputStream ByteArrayOutputStream)))

(deftest transform "basic transformations"
  (is (= (transform! (identity-stylesheet) [:root {:foo "bar"} :a :b :c])
         (normalize [:root {:foo "bar"} :a :b :c]))
      "identity stylesheet")
  (is (= (transform! (xsl/stylesheet "1.0"
                       (xsl/match-template "/root"
                         [:foo (xsl/value-of "@foo")]))
                     [:root {:foo "bar"} :a :b :c])
         [:foo {} "bar"])
      "trivial stylesheet"))

(deftest polymorphic-source "polymorphic transformation source"
  (are [s] (= (transform! (identity-stylesheet) s)
              (normalize [:root :a :b :c]))
       [:root :a :b :c]
       (sax-source [:root :a :b :c])
       "<root><a/><b/><c/></root>"
       (ByteArrayInputStream. (.getBytes "<root><a/><b/><c/></root>"))
       (StringReader. "<root><a/><b/><c/></root>")))

(deftest polymorphic-template "polymorphic stylesheet input"
  (let [sxml-template (xsl/stylesheet "1.0"
                        (xsl/match-template "/root"
                          [:foo (xsl/value-of "@foo")]))
        string-template
        "<xsl:stylesheet xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
             version='1.0'>
           <xsl:template match='/root'>
             <foo><xsl:value-of select='@foo' /></foo>
           </xsl:template>
         </xsl:stylesheet>"]
    (binding [*default-xmlns* {:xsl "http://www.w3.org/1999/XSL/Transform"}]
      (are [t] (= (transform! t [:root {:foo "bar"}])
                  [:foo {} "bar"])
           sxml-template
           (compile-template sxml-template)
           (sax-source sxml-template)
           (compile-template (sax-source sxml-template))
           string-template
           (compile-template string-template)
           (ByteArrayInputStream. (.getBytes string-template))
           (compile-template (ByteArrayInputStream. (.getBytes string-template)))
           (StringReader. string-template)
           (compile-template (StringReader. string-template))))))

(defn exclude-?xml [s] (re-find #"<[^?].*[^?]>" s))

(deftest polymorphic-result "polymorphic transformation result"
  (let [t (fn [r] (transform! (identity-stylesheet) [:root :a :b :c] r))]
    (is (= (t :sxml) (normalize [:root :a :b :c])))
    (is (= (exclude-?xml (t :string))
           "<root><a/><b/><c/></root>"))
    (is (= (exclude-?xml (.toString (t (StringWriter.))))
           "<root><a/><b/><c/></root>"))
    (is (= (exclude-?xml (.toString (t (ByteArrayOutputStream.))))
           "<root><a/><b/><c/></root>"))
    (is (= (let [[result output-atom] (sax-result)]
             (t result)
             @output-atom)
           (normalize [:root :a :b :c])))))

(deftest sxml-output "copying to various sinks"
  (let [p (fn [r] (copy! [:root :a :b :c] r))
        expect-string "<root><a/><b/><c/></root>"]
    (is (= (exclude-?xml (p :string))
           expect-string))
    (is (= (exclude-?xml (.toString (p (StringWriter.))))
           expect-string))
    (is (= (exclude-?xml (.toString (p (ByteArrayOutputStream.))))
           expect-string))))

(deftest xml-namespaces "XML namespace output"
  (is (= (exclude-?xml (copy! [:myns/root :a :b :c]))
         "<myns:root><a/><b/><c/></myns:root>"))
  (is (= (exclude-?xml (copy! [:myns/root {:otherns/bar "bar"} :a]))
         "<myns:root otherns:bar=\"bar\"><a/></myns:root>")))

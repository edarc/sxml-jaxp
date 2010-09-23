(ns sxml-jaxp.test.sax
  (:use [sxml-jaxp.core :only [normalize simplify]] :reload)
  (:use [sxml-jaxp.sax] :reload)
  (:use [clojure.test])
  (:import
    (java.io StringReader ByteArrayInputStream)))

(deftest event-generation "generation of SAX events from SXML"
  (let [sax-event-seq (comp sax-event-seq normalize)]
    (is (= (sax-event-seq :t)
           [[:start-element :t {}]
            [:end-element :t]])
        "empty root element")
    (is (= (sax-event-seq [:root :inner])
           [[:start-element :root {}]
            [:start-element :inner {}]
            [:end-element :inner]
            [:end-element :root]])
        "single nested element")
    (is (= (sax-event-seq [:root :bob [:alice :eve] :mallory])
           [[:start-element :root {}]
            [:start-element :bob {}]
            [:end-element :bob]
            [:start-element :alice {}]
            [:start-element :eve {}]
            [:end-element :eve]
            [:end-element :alice]
            [:start-element :mallory {}]
            [:end-element :mallory]
            [:end-element :root]])
        "multiple nested elements")
    (is (= (sax-event-seq [:root {:foo "bar"}])
           [[:start-element :root {:foo "bar"}]
            [:end-element :root]])
        "empty element with attributes")
    (is (= (sax-event-seq [:root [:bob {:foo "bar"}]])
           [[:start-element :root {}]
            [:start-element :bob {:foo "bar"}]
            [:end-element :bob]
            [:end-element :root]])
        "nested element with attributes")
    (is (= (sax-event-seq [:root "foo"])
           [[:start-element :root {}]
            [:text-node "foo"]
            [:end-element :root]])
        "nested text node")
    (is (= (sax-event-seq [:root {:xmlns:foo "foo"}])
           [[:start-prefix :foo "foo"]
            [:prefix-map {:foo "foo"}]
            [:start-element :root {:xmlns:foo "foo"}]
            [:end-element :root]
            [:prefix-map {}]
            [:end-prefix :foo]])
        "namespace declaration")
    (is (= (sax-event-seq [:root {:xmlns:foo "foo"} [:bar {:xmlns:foo "masked"}]])
           [[:start-prefix :foo "foo"]
            [:prefix-map {:foo "foo"}]
            [:start-element :root {:xmlns:foo "foo"}]
            [:start-prefix :foo "masked"]
            [:prefix-map {:foo "masked"}]
            [:start-element :bar {:xmlns:foo "masked"}]
            [:end-element :bar]
            [:prefix-map {:foo "foo"}]
            [:end-prefix :foo]
            [:end-element :root]
            [:prefix-map {}]
            [:end-prefix :foo]])
        "masked namespace declaration")))

(deftest shift-reduce "SXML shift-reduce parser"
  (is (= "foo"
         (-> ()
           (push-text "foo")
           first))
      "non-markup")
  (is (= (normalize :t)
         (-> ()
           (push-element :t {})
           (reduce-element :t)
           first))
      "emtpy root element")
  (is (= (normalize [:root :inner])
         (-> ()
           (push-element :root {})
           (push-element :inner {})
           (reduce-element :inner)
           (reduce-element :root)
           first))
      "single nested element")
  (is (= (normalize [:root :bob [:alice :eve] :mallory])
         (-> ()
           (push-element :root {})
           (push-element :bob {})
           (reduce-element :bob)
           (push-element :alice {})
           (push-element :eve {})
           (reduce-element :eve)
           (reduce-element :alice)
           (push-element :mallory {})
           (reduce-element :mallory)
           (reduce-element :root)
           first))
      "multiple nested elements")
  (is (= (normalize [:root {:foo "bar"}])
         (-> ()
           (push-element :root {:foo "bar"})
           (reduce-element :root)
           first))
      "empty element with attributes")
  (is (= (normalize [:root [:bob {:foo "bar"}]])
         (-> ()
           (push-element :root {})
           (push-element :bob {:foo "bar"})
           (reduce-element :bob)
           (reduce-element :root)
           first))
      "nested element with attributes")
  (is (= (normalize [:root "foo"])
         (-> ()
           (push-element :root {})
           (push-text "foo")
           (reduce-element :root)
           first))
      "nested text node")
  (is (= (normalize [:root {:xmlns:foo "foo"}])
         (-> ()
           (push-prefix :foo "foo")
           (push-element :root {})
           (reduce-element :root)
           first))
      "namespace declaration"))

(defn sax-roundtrip
  [form]
  (let [[result handler] (sax-handler)
        reader           (sax-reader form)]
    (doto reader
      (.setContentHandler handler)
      (.parse nil))
    @result))

(deftest sxml-sax-roundtrip "SXML->SAX->SXML roundtrip is lossless"
  (are [form] (= (normalize form) (sax-roundtrip (normalize form)))
    :t
    [:root :inner]
    [:root :bob [:alice :eve] :mallory]
    [:root {:foo "bar"}]
    [:root [:bob {:foo "bar"}]]
    [:root "foo"]
    [:root {:xmlns:foo "foo"}]
    [:root
     [:bob "hello bob"]
     "and then"
     [:alice {:type "hello"} "hello alice and" :eve]
     :finally
     [:mallory "hello mallory"]]))

(deftest sxml-input "reading SXML from various input"
  (let [string-input "<root attr='val'><a/><b/><c/></root>"]
    (are [s] (= (simplify (read-sxml s))
                [:root {:attr "val"} :a :b :c])
         string-input
         (StringReader. string-input)
         (ByteArrayInputStream. (.getBytes string-input)))))

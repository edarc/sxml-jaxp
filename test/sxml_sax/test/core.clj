(ns sxml-sax.test.core
  (:use [sxml-sax.core] :reload)
  (:use [clojure.test])
  (:import
    (java.io InputStream OutputStream Reader Writer File StringReader
             StringWriter ByteArrayInputStream ByteArrayOutputStream)))

(deftest normalization "normalization of SXML forms"
  (testing "non-markup"
    (is (= (normalize "foo") "foo")))
  (testing "empty elements"
    (are [in] (= (normalize in) [:t {}])
      :t
      [:t]
      [:t {}]))
  (testing "non-empty elements with no attributes"
    (are [in] (= (normalize in) [:t {} "foo"])
      [:t "foo"]
      [:t {} "foo"])
    (are [in] (= (normalize in) [:t {} "foo" "bar"])
      [:t "foo" "bar"]
      [:t {} "foo" "bar"]))
  (testing "irreducible element forms"
    (are [form] (= (normalize form) form)
      [:t {:foo "bar"}]
      [:t {:foo "bar"} "foo"]
      [:t {:foo "bar"} "foo" "bar"]))
  (testing "recursive normalization of elements"
    (are [in] (= (normalize in) [:t1 {} [:t2 {}]])
      [:t1 :t2]
      [:t1 {} :t2]
      [:t1 [:t2]]
      [:t1 {} [:t2]]
      [:t1 [:t2 {}]]
      [:t1 {} [:t2 {}]])
    (are [in] (= (normalize in) [:t1 {} [:t2 {}] [:t3 {}]])
      [:t1 :t2 :t3]
      [:t1 {} :t2 :t3])))

(deftest simplification "simplification of SXML forms"
  (testing "non-markup"
    (is (= (simplify "foo") "foo")))
  (testing "empty elements"
    (are [in] (= (simplify in) :t)
      :t
      [:t]
      [:t {}]))
  (testing "non-empty elements with no attributes"
    (are [in] (= (simplify in) [:t "foo"])
      [:t "foo"]
      [:t {} "foo"])
    (are [in] (= (simplify in) [:t "foo" "bar"])
      [:t "foo" "bar"]
      [:t {} "foo" "bar"]))
  (testing "irreducible element forms"
    (are [form] (= (simplify form) form)
      [:t {:foo "bar"}]
      [:t {:foo "bar"} "foo"]
      [:t {:foo "bar"} "foo" "bar"]))
  (testing "recursive simplification of elements"
    (are [in] (= (simplify in) [:t1 :t2])
      [:t1 :t2]
      [:t1 {} :t2]
      [:t1 [:t2]]
      [:t1 {} [:t2]]
      [:t1 [:t2 {}]]
      [:t1 {} [:t2 {}]])
    (are [in] (= (simplify in) [:t1 :t2 :t3])
      [:t1 :t2 :t3]
      [:t1 {} :t2 [:t3]]
      [:t1 {} :t2 [:t3 {}]])))

(deftest event-generation "generation of SAX events from SXML"
  (is (= (sax-event-seq "foo")
         [[:text-node "foo"]])
      "non-markup")
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
          [:start-element :root {:xmlns:foo "foo"}]
          [:end-element :root]
          [:end-prefix :foo]])
      "namespace declaration"))

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

(deftest sxml-input "reading SXML from various input"
  (let [string-input "<root attr='val'><a/><b/><c/></root>"]
    (are [s] (= (simplify (read-sxml s))
                [:root {:attr "val"} :a :b :c])
         string-input
         (StringReader. string-input)
         (ByteArrayInputStream. (.getBytes string-input)))))

(defn sax-roundtrip
  [form]
  (let [[result handler] (sax-handler)
        reader           (sax-reader form)]
    (doto reader
      (.setContentHandler handler)
      (.parse nil))
    @result))

(deftest sxml-sax-roundtrip "SXML->SAX->SXML roundtrip is lossless"
  (are [form] (= (normalize form) (sax-roundtrip form))
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

(ns sxml-sax.test.core
  (:use [sxml-sax.core] :reload)
  (:use [clojure.test]))

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

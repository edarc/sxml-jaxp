(ns sxml-jaxp.test.core
  (:use [sxml-jaxp.core] :reload)
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
      [:t1 {} :t2 :t3]))
  (testing "top-level normalization"
    (are [in] (= (normalize-1 in) [:t1 {}])
      :t1
      [:t1]
      [:t1 {}])
    (are [in] (= (normalize-1 in) [:t1 {} "foo"])
      [:t1 "foo"]
      [:t1 {} "foo"])))

(deftest access-helpers "element access helpers"
  (are [in] (= (tag in) :t)
    :t
    [:t]
    [:t {}]
    [:t {:foo "bar"} "foo"])
  (are [in] (= (attrs in) {})
    :t
    [:t]
    [:t {}]
    [:t {} "foo"])
  (are [in] (= (attrs in) {:foo "bar"})
    [:t {:foo "bar"}]
    [:t {:foo "bar"} "foo"])
  (are [in] (= (children in) [])
    :t
    [:t]
    [:t {}]
    [:t {:foo "bar"}])
  (are [in] (= (children in) ["foo" "bar"])
    [:t "foo" "bar"]
    [:t {} "foo" "bar"]))

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

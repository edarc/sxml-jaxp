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

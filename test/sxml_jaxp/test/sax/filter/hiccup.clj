(ns sxml-jaxp.test.sax.filter.hiccup
  (:use [sxml-jaxp.transform :only [copy!]])
  (:use [sxml-jaxp.core :only [normalize simplify attrs]])
  (:use [sxml-jaxp.sax.filter :only [filter-with]])
  (:use [sxml-jaxp.sax.filter.hiccup :only [hiccup]] :reload)
  (:use [clojure.test]))

(defn convert
  [form]
  (simplify (copy! (filter-with [hiccup] form) :sxml)))

(defn classes-equiv?
  [a b]
  (= (set (.split a " "))
     (set (.split b " "))))

(defn class-of
  [form]
  (-> form convert attrs :class))

(deftest hiccup-syntax "translation of Hiccup syntax"
  (is (= (convert :div) :div))
  (is (= (convert :div#foo) [:div {:id "foo"}]))
  (is (= (convert :div.foo) [:div {:class "foo"}]))
  (is (= (convert :div#foo.bar) [:div {:id "foo" :class "bar"}]))
  (is (= (convert [:div.foo "bar" :hr#baz])
         [:div {:class "foo"} "bar" [:hr {:id "baz"}]]))
  (is (classes-equiv? (class-of :div.a.b)
                      "a b"))
  (is (classes-equiv? (class-of [:div.a.b {:class "a"}])
                      "a b"))
  (is (classes-equiv? (class-of [:div.a.b {:class "b c"}])
                      "a b c"))
  (is (classes-equiv? (class-of [:div.a.b {:class #{"b" "c"}}])
                      "a b c"))
  (is (= (convert [:div#foo {:id "bar"}])
         [:div {:id "bar"}])))

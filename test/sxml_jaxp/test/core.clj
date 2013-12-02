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


(deftest xml-namespaces "XML namespace declaration helpers"
  (testing "ns-decl?"
    (are [kw res] (= (ns-decl? kw) res)
         :some-attr false
         :namespaced/attr false
         :xmlns true
         :xmlns/myns true))

  (testing "prefix <-> ns-decl"
    (are [pfx nsd] (= (prefix->ns-decl pfx) nsd)
         nil :xmlns
         "" :xmlns
         :myns :xmlns/myns
         :yourns :xmlns/yourns
         "myns" :xmlns/myns)
    (are [nsd pfx] (= (ns-decl->prefix nsd) pfx)
         :xmlns nil
         :xmlns/myns :myns
         :xmlns/yourns :yourns)
    (is (thrown? IllegalArgumentException
                 #"not a namespace declaration"
                 (ns-decl->prefix :not-a-namespace)))
    (is (thrown? IllegalArgumentException
                 #"not a namespace declaration"
                 (ns-decl->prefix "not-a-namespace"))))

  (testing "ns-decls access helper"
    (are [elem nsmap] (= (ns-decls elem) nsmap)
         :foo {}
         [:foo "bar"] {}
         [:foo {} "bar"] {}
         [:foo {:xmlns "default-uri"
                :xmlns/myns "myns-uri"}] {nil "default-uri"
                                          :myns "myns-uri"}
         [:foo {:xmlns/myns "myns-uri"
                :not-a-ns "normal attribute"}] {:myns "myns-uri"}))

  (testing "alter-ns-decls modification helper"
    (are [elem] (= (ns-decls (alter-ns-decls
                               elem (constantly {nil "new-default-uri"})))
                   {nil "new-default-uri"})
         :foo
         [:foo]
         [:foo {}]
         [:foo {:xmlns "old-default-uri"} "child"]
         [:foo {:xmlns "old-default-uri"
                :xmlns/doomed "doomed-uri"
                :not-a-ns "normal attribute"} "child"])

    (are [in f nsds+normal-value] (= ((juxt ns-decls (comp :not-a-ns second))
                                      (alter-ns-decls in f))
                                     nsds+normal-value)
         ; merging
         [:foo {:not-a-ns "normal attribute"
                :xmlns "old-default-uri"
                :xmlns/untouched "untouched-uri"}]
         #(merge % {nil "new-default-uri"})
         [{nil "new-default-uri" :untouched "untouched-uri"} "normal attribute"]

         ; overwriting
         [:foo {:xmlns "old-default-uri"
                :xmlns/doomed "doomed-uri"
                :not-a-ns "normal attribute"} "child"]
         (constantly {nil "new-default-uri"})
         [{nil "new-default-uri"} "normal attribute"]

         ; nonsense function
         [:foo {:xmlns "default-uri"
                :xmlns/myns "myns-uri"
                :not-a-ns "normal attribute"}]
         #(into {} (for [[pfx uri] %]
                     [pfx (.toUpperCase uri)]))
         [{nil "DEFAULT-URI" :myns "MYNS-URI"} "normal attribute"]))

  (testing "update-ns-decls modification helper"
    (is (= ((juxt ns-decls (comp :not-a-ns second))
            (update-ns-decls [:foo {:xmlns/overwritten "old-uri"
                                    :xmlns/untouched "untouched-uri"
                                    :not-a-ns "normal attribute"}]
                             {:new "new-uri" :overwritten "replaced-uri"}))
           [{:new "new-uri"
             :overwritten "replaced-uri"
             :untouched "untouched-uri"}
            "normal attribute"])))

  (testing "replace-ns-decls modification helper"
    (is (= ((juxt ns-decls (comp :not-a-ns second))
            (replace-ns-decls [:foo {:xmlns/overwritten "old-uri"
                                     :xmlns/deleted "deleted-uri"
                                     :not-a-ns "normal attribute"}]
                              {:new "new-uri" :overwritten "replaced-uri"}))
           [{:new "new-uri"
             :overwritten "replaced-uri"}
            "normal attribute"]))))


(deftest access-helpers "element access helpers"
  (testing "basic functionality; no namespaces"
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

  (testing "namespace propagation"
    (is (= (attrs [:t {:normal-attr "foo"
                       :xmlns "default-uri"
                       :xmlns/foo "foo-uri"}])
           {:normal-attr "foo"})
        "`attrs` ignores xmlns declarations")

    (is (= (children [:t {:normal-attr "foo"
                          :xmlns "default-uri"
                          :xmlns/parent "parent-uri"}
                      [:child1 {:child-attr "bar"}
                       "no ns-decls"]
                      [:child2 {:another-attr "baz"
                                :xmlns "redefined-default-uri"
                                :xmlns/child "child-uri"}
                       "new and masked ns-decls"]])
           [[:child1 {:child-attr "bar"
                      :xmlns "default-uri"
                      :xmlns/parent "parent-uri"}
             "no ns-decls"]
            [:child2 {:another-attr "baz"
                      :xmlns "redefined-default-uri"
                      :xmlns/child "child-uri"
                      :xmlns/parent "parent-uri"}
             "new and masked ns-decls"]])
        "`children` propagates the namespace declarations from the parent")))


(deftest tag-modification-helpers "tag modification helpers"
  (testing "alter-tag"
    (are [in f out] (= (simplify (alter-tag in f)) out)
         "not a tag"
         (constantly :irrelevant)
         "not a tag"

         :old
         (constantly :new)
         :new

         [:old "child"]
         (constantly :new)
         [:new "child"]

         [:oldns/tag "child"]
         #(keyword "newns" (name %))
         [:newns/tag "child"]

         :abc
         #(keyword (.toUpperCase (name %)))
         :ABC

         [:old {:some-attribute "normal attribute"
                :xmlns "default-uri"}
          [:child "text"]]
         (constantly :new)
         [:new {:some-attribute "normal attribute"
                :xmlns "default-uri"}
          [:child "text"]]))

  (testing "replace-tag"
    (are [in rep out] (= (simplify (replace-tag in rep)) out)
         "not a tag"
         :irrelevant
         "not a tag"

         :old
         :new
         :new

         [:old "child"]
         :new
         [:new "child"]

         [:old {:some-attribute "normal attribute"
                :xmlns "default-uri"}
          [:child "text"]]
         :new
         [:new {:some-attribute "normal attribute"
                :xmlns "default-uri"}
          [:child "text"]])))


(deftest attribute-modification-helpers "attribute modification helpers"
  (testing "alter-attrs"
    (are [in f out] (= (simplify (alter-attrs in f)) out)
         "not a tag"
         (constantly {:irrelevant "attribute"})
         "not a tag"

         :t
         (constantly {:new "new attribute"})
         [:t {:new "new attribute"}]

         [:t "child"]
         (constantly {:new "new attribute"})
         [:t {:new "new attribute"} "child"]

         [:t {:unaltered "untouched value"
              :deleted "doomed"}
          "child"]
         #(dissoc % :deleted)
         [:t {:unaltered "untouched value"} "child"]

         [:t {:abc "untouched value"
              :def "untouched value"}
          "child"]
         #(into {} (for [[k v] %]
                     [(keyword (.toUpperCase (name k))) v]))
         [:t {:ABC "untouched value"
              :DEF "untouched value"}
          "child"]

         ; xmlns is passed through
         [:t {:overwritten "old value"
              :deleted "doomed"
              :xmlns "default-uri"
              :xmlns/myns "myns-uri"}
          "child"]
         (constantly {:new "new attribute"
                      :overwritten "new value"})
         [:t {:overwritten "new value"
              :new "new attribute"
              :xmlns "default-uri"
              :xmlns/myns "myns-uri"}
          "child"]

         ; xmlns aren't presented to f
         [:t {:abc "untouched value"
              :def "untouched value"
              :xmlns "default-uri"
              :xmlns/myns "myns-uri"}
          "child"]
         #(into {} (for [[k v] %]
                     [(keyword (.toUpperCase (name k))) v]))
         [:t {:ABC "untouched value"
              :DEF "untouched value"
              :xmlns "default-uri"
              :xmlns/myns "myns-uri"}
          "child"]))

  (testing "update-attrs"
    (are [in upd out] (= (simplify (update-attrs in upd)) out)
         "not a tag"
         {:irrelevant "attribute"}
         "not a tag"

         :t
         {}
         :t

         :t
         {:new "new attribute"}
         [:t {:new "new attribute"}]

         [:t "stuff"]
         {:new "new attribute"}
         [:t {:new "new attribute"} "stuff"]

         [:t {:overwritten "old value"
              :unaltered "unmodified attribute"}
          "stuff"]
         {:overwritten "new value"}
         [:t {:overwritten "new value"
              :unaltered "unmodified attribute"}
          "stuff"]

         [:t {:overwritten "old value"
              :unaltered "unmodified attribute"
              :xmlns "default-uri"}
          [:child "text"]]
         {:overwritten "new value"}
         [:t {:overwritten "new value"
              :unaltered "unmodified attribute"
              :xmlns "default-uri"}
          [:child "text"]]))

  (testing "replace-attrs"
    (are [in rep out] (= (simplify (replace-attrs in rep)) out)
         "not a tag"
         {:irrelevant "attribute"}
         "not a tag"

         :t
         {}
         :t

         :t
         {:new "new attribute"}
         [:t {:new "new attribute"}]

         [:t "stuff"]
         {:new "new attribute"}
         [:t {:new "new attribute"} "stuff"]

         [:t {:overwritten "old value"
              :deleted "doomed"}
          "stuff"]
         {:overwritten "new value"}
         [:t {:overwritten "new value"} "stuff"]

         [:t {:overwritten "old value"
              :deleted "doomed"
              :xmlns "default-uri"}
          [:child "text"]]
         {:overwritten "new value"}
         [:t {:overwritten "new value"
              :xmlns "default-uri"}
          [:child "text"]])))


(deftest child-modification-helpers "child element modification helpers"
  (testing "alter-children"
    (are [in f out] (= (simplify (alter-children in f)) out)
         "not a tag"
         (constantly [:irrelevant])
         "not a tag"

         :t
         (constantly [])
         :t

         :t
         (comp list count)
         [:t 0]

         [:t {:unaltered "attribute"} :foo [:bar "irrelevant"]]
         (comp list count)
         [:t {:unaltered "attribute"} 2]

         [:t :foo :bar]
         (partial map name)
         [:t "foo" "bar"]

         [:t :foo :bar]
         (constantly [])
         :t

         [:t :first :second]
         reverse
         [:t :second :first]

         [:t [:first "first text"] [:second "second text" :child-tag]]
         (partial mapcat children)
         [:t "first text" "second text" :child-tag]))

  (testing "replace-children"
    (are [in f out] (= (simplify (replace-children in f)) out)
         "not a tag"
         [:irrelevant]
         "not a tag"

         :t
         []
         :t

         [:t {:unaltered "attribute"} :foo [:deleted "text"]]
         [[:new "other text"]]
         [:t {:unaltered "attribute"} [:new "other text"]]

         [:t :foo :bar]
         []
         :t

         [:t [:first "first text"] [:second "second text" :child-tag]]
         ["first text" "second text"]
         [:t "first text" "second text"]))

  (testing "map-children"
    (are [in f out] (= (simplify (map-children in f)) out)
         "not a tag"
         (constantly [:irrelevant])
         "not a tag"

         :t
         (constantly :irrelevant)
         :t

         [:t {:unaltered "attribute"} :foo [:bar "irrelevant"]]
         tag
         [:t {:unaltered "attribute"} :foo :bar]

         [:t :foo :bar]
         name
         [:t "foo" "bar"]

         [:t
          [:first "first text" :first-ignored]
          [:second "second text" [:second-ignored "doomed"]]]
         (comp first children)
         [:t "first text" "second text"])))

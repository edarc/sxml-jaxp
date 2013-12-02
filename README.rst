=========
sxml-jaxp
=========

**sxml-jaxp** is a library of tools for using SXML-inspired XML representations
with the Java XML infrastructure. It uses JAXP internally, so it does not
depend on any particular XML library, although the author has personally tested
it with the OpenJDK default Xerces/Xalan and Saxon 9.1.

Included are tools for reading XML into SXML forms (using SAX), transforming
SXML using XSL Transformations as well as converting it back to XML (using the
TrAX APIs).

Clojure-flavored SXML
=====================

Clojure-flavored SXML uses vectors, keywords, and maps to denote elements,
tags, and attributes. [1]_

.. code:: clojure

  [:item {}
    [:title {} "Memorandum"]
    [:author {} "tyler.durden@paperstreetsoap.com"]
    [:media/content {:url "http://paperstreetsoap.com/snowflake.jpg",
                     :type "image/jpeg",
                     :height "100",
                     :width "100"}]
    [:description {} "You are not a beautiful and unique snowflake."]
    [:some-other-thing {}]]

Elements take the form ``[tag attrs & children]``, where ``tag`` is a keyword
denoting the element name, ``attrs`` is a possibly-empty map of keywords
(attribute names) to values (strings), and ``children`` is a possibly empty
sequence of child nodes, which may be strings or other elements. This is the
*normalized* form.

There is also a *simplified* form:

.. code:: clojure

  [:item
    [:title "Memorandum"]
    [:author "tyler.durden@paperstreetsoap.com"]
    [:media/content {:url "http://paperstreetsoap.com/snowflake.jpg",
                     :type "image/jpeg",
                     :height "100",
                     :width "100"}]
    [:description "You are not a beautiful and unique snowflake."]
    :some-other-thing]

In this form, tags may be of the form ``[tag attrs & children]``, empty
attribute maps can be elided as ``[tag & children]``, and elements with no
children *or* attributes may elide the vector altogether, leaving a bare
keyword as ``tag``, where ``tag`` is a keyword denoting the element name,
``attrs`` is a *non-empty* map of keywords, and ``children`` is a *non-empty*
sequence of child nodes.

``sxml-jaxp`` provides the function ``normalize`` to convert SXML to normalized
form, and ``simplify`` to convert to simplified form. Functions generally
accept either form (or some mixture), but typically return normalized form.

.. [1] This is in contrast with the original Scheme SXML which uses lists and
   symbols for everything.

Usage
=====

Here are some examples of using ``sxml-jaxp`` for various tasks. We'll assume
the following ``require``'s:

.. code:: clojure

  (require '[sxml-jaxp.core :as core]
           '[sxml-jaxp.sax :as sax]
           '[sxml-jaxp.transform :as xfm]
           '[sxml-jaxp.transform.xslt :as xsl]
           '[clojure.java.io :as io])

Parsing XML to SXML
-------------------

You can use the ``sxml-jaxp.sax/read-sxml`` function to read from some source
of XML data and return it in SXML format:

.. code:: clojure

  user=> (sax/read-sxml "<greet>Hello world!</greet>")
  [:greet {} "Hello world!"]

You can use ``Reader``'s, ``InputStream``'s, and ``File``'s too.

.. code:: clojure

  user=> (sax/read-sxml (java.io.StringReader. "<greet>Hello world!</greet>"))
  [:greet {} "Hello world!"]

SXML manipulation helpers
-------------------------

There are several helper functions to allow you to easily access and modify
SXML elements and forms.

Access functions
................

The functions ``sxml-jaxp.core/tag``, ``attrs``, and ``children`` allow you to
access the components of SXML elements easily.

.. code:: clojure

  user=> (def fancy-hello [:greet {:language "en"} "Hello world!"])
  #'user/fancy-hello
  user=> (core/tag fancy-hello)
  :greet
  user=> (core/attrs fancy-hello)
  {:language "en"}
  user=> (core/children fancy-hello)
  ["Hello world!"]

These are more robust than regular vector access methods, firstly because they
work on SXML that might not be normalized:

.. code:: clojure

  user=> (def simple-hello [:greet "Hello world!"])
  #'user/simple-hello
  user=> (core/attrs simple-hello)
  {}
  user=> (core/children simple-hello)
  ["Hello world!"]

Additionally, they are also XML-namespace-aware. They can help you propagate
namespace prefix declarations automatically when navigating, and keep them out
of your hair when working with attributes:

.. code:: clojure

  user=> (def namespacey-hello
           [:hi/greetings
            {:xmlns/hi "http://w3.org/TR/Hello" :language "en"}
            [:hi/greet "Hello world!"]])
  user=> (core/attrs namespacey-hello)
  {:language "en"}
  user=> (core/children namespacey-hello)
  [[:hi/greet {:xmlns/hi "http://w3.org/TR/Hello"} "Hello world!"]]

There is also one other access function specifically for namespace
declarations, ``ns-decls``:

.. code:: clojure

  user=> (core/ns-decls namespacey-hello)
  {:hi "http://w3.org/TR/Hello"}

Modification functions
......................

The modification functions are in three groups, and all take an element as
their first argument. The first is the *alter* group, which update an element
by applying a function to some portion of it. These are ``alter-tag``,
``alter-attrs``, ``alter-children`` and ``alter-ns-decls``:

.. code:: clojure

  user=> (core/alter-tag fancy-hello (fn [t] (-> t name .toUpperCase keyword)))
  [:GREET {:language "en"} "Hello world!"]
  user=> (core/alter-attrs fancy-hello #(assoc % :language "en-US"))
  [:greet {:language "en-US"} "Hello world!"]
  user=> (core/alter-children namespacey-hello (partial mapcat core/children))
  [:hi/greetings
   {:xmlns/hi "http://w3.org/TR/Hello", :language "en"}
   "Hello world!"]
  user=> (core/alter-ns-decls
           namespacey-hello (fn [nsd] (assoc nsd nil (:hi nsd))))
  [:hi/greetings
   {:xmlns/hi "http://w3.org/TR/Hello",
    :xmlns "http://w3.org/TR/Hello",
    :language "en"}
   [:hi/greet "Hello world!"]]

The second group is the *replace* group, ``replace-tag``, ``replace-attrs``,
``replace-children``, ``replace-ns-decls``. These are shorthand for altering an
element by replacing it with a constant value [2]_:

.. code:: clojure

  user=> (core/replace-tag fancy-hello :salutation)
  [:salutation {:language "en"} "Hello world!"]
  user=> (core/replace-children fancy-hello [[:with-feeling "Hello world!!!"]])
  [:greet {:language "en"} [:with-feeling "Hello world!!!"]]

The third group is the special group, which contains ``update-attrs``,
``update-ns-decls``, and ``map-children``. These are also shorthand, for
altering the attributes or XML namespace declarations by merging a map of new
and updated values, like Clojure's ``merge`` function. [2]_

``map-children`` is similar to ``alter-children`` except that it maps the
function across each child element in turn, while ``alter-children`` calls the
function once and passes the entire sequence of children as the parameter.

.. code:: clojure

  user=> (core/map-children [:parent :one :two :three [:four "4"]]
                            (comp name core/tag))
  [:parent {} "one" "two" "three" "four"]

There are also two tiny combinators for helping construct functions to pass
into ``alter-*`` and ``map-children``, which are named ``on-tags`` and
``on-text``. These work by turning your function into an identity when the type
of element passed is not a tag or a text node:

.. code:: clojure

  user=> (core/map-children [:parent :one :two :three "surprise"]
                            (comp name core/tag))
  ClassCastException java.lang.Character cannot be cast to clojure.lang.Named
  clojure.core/name (core.clj:1505)
  user=> (core/map-children [:parent :one :two :three "surprise"]
                            (core/on-tags (comp name core/tag)))
  [:parent {} "one" "two" "three" "surprise"]
  user=> (core/map-children [:parent :one :two :three "surprise"]
                            (core/on-text #(.toUpperCase %)))
  [:parent {} :one :two :three "SURPRISE"]

.. [2] They are implemented using the ``alter-`` functions with Clojure's
   ``constantly`` and ``merge`` functions.

Outputting to XML
-----------------

The ``sxml-jaxp.transform/copy!`` function can be used to copy SXML into various
kinds of output "sinks". Here, we'll use a ``Writer``. Notice it returns the
thing you passed as the "sink" so you can do more stuff with it:

.. code:: clojure

  user=> (.toString (xfm/copy! fancy-hello (java.io.StringWriter.)))
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><greet language=\"en\">Hello world!</greet>"

``copy!`` also recognizes the special sink ``:string``, which is the default
when you don't provide a sink. [3]_ This causes it to return the source as a
string of XML:

.. code:: clojure

  user=> (xfm/copy! fancy-hello :string)
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><greet language=\"en\">Hello world!</greet>"
  user=> (xfm/copy! fancy-hello)
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><greet language=\"en\">Hello world!</greet>"

XSL Transforms
--------------

Transformations are performed with the ``sxml-jaxp.transform/transform!``
function.  This accepts a stylesheet, a source, and a result. I'll use the XSLT
DSL (defined in ``sxml-jaxp.transform.xslt``) to create XSLT stylesheets.

.. code:: clojure

  user=> (xfm/transform! (xsl/stylesheet "1.0"
                           (xsl/match-template "/once-old"
                             [:new-again (xsl/copy-of "@*|node()")]))
                         [:once-old "Hi!"])
  [:new-again {} "Hi!"]

I didn't provide a target for the result, so it defaulted to the special target
``:sxml`` [3]_. Like ``copy!``, it recognizes the special target ``:string`` as
well, and you can use any other reasonable object as your result target.

Here's a more complex example, getting a seq of the latest article titles on
Ars Technica using their RSS feed:

.. code:: clojure

  user=> (def rss-title-tmpl
           (xfm/compile-template
             (xsl/stylesheet "1.0"
               (xsl/match-template "/rss/channel/item"
                 [:link {:title "{title}"}])
               (xsl/match-template "/rss"
                 [:items (xsl/apply-templates-to "channel/item")]))))
  #'user/rss-title-tmpl
  user=> (with-open [at-rss-in (io/input-stream
                                 "http://feeds.arstechnica.com/arstechnica/everything")]
           (map (comp :title core/attrs)
                (core/children (xfm/transform! rss-title-tmpl at-rss-in))))
  ("Forget Amazon’s two-day shipping, soon you can select drone delivery"
   "Why Comcast and other cable ISPs aren’t selling you gigabit Internet"
   "What’s the difference between college-level and corporate programming?"
   "Comet ISON fizzles… but there’s a sting in the tail"
   "Despacio: The 50,000-watt sound system designed for discerning audiophiles"
   "Anti-GMO crop paper to be forcibly retracted"
   "Gallery: Disorienting audiovisual show prepares you for teleportation"
   "Off Siberia’s Arctic coast, the seafloor belches methane"
   "Ars’ resident racer takes a second look at Forza Motorsport 5"
   "Chairs Technica: Where your favorite Ars writers park their rears"
   "Five complaints Ars readers have about OS X Mavericks"
   "Unhappy Thanksgiving for Prenda Law, ordered to pay $261K to defendants"
   "TV news team falls for Facebook doppelgänger scam"
   "Robot Garden 1.0: Putting Click and Grow to the test"
   "Water-repellant surface so efficient that drops bounce back off"
   "Successful killing by stealthy seahorses comes down to their snouts"
   "Tricksy hobbit-sized black hole pretends to be a giant"
   "Ars Technica System Guide: November 2013"
   "Dealmaster Black Friday blowout continues, with updated deals!"
   "Google removes CyanogenMod Installer from Play Store"
   "How to talk your family out of bad consumer electronics purchases"
   "Once-great SSD manufacturer OCZ filing for bankruptcy"
   "Catch up on last-gen gaming with these Black Friday deals"
   "New Linux worm targets routers, cameras, “Internet of things” devices"
   "Elusive Higgs decay channel spotted; particle looks ever more standard")

Here we've pre-compiled our XSL template using ``compile-template``. This can
be used if you plan on transforming more than one document with a particular
stylesheet. It uses TrAX to compile the template into some object implementing
``Templates``, so that it doesn't have to parse and compile it for every
invocation.

.. [3] ``copy!`` actually recognizes the ``:sxml`` sink also, although I don't
   know why you'd ever need that; generally you'd want to use
   ``sxml-jaxp.sax/read-sxml`` which bypasses TrAX and reads the input directly
   with SAX.

XSLT DSL
........

The namespace ``sxml-jaxp.transform.xslt`` [4]_ defines a DSL for writing XSL
transformation stylesheets in Clojure. This DSL outputs the stylesheets in SXML
format. Here's the template we used in the last example:

.. code:: clojure

  user=> (xsl/stylesheet "1.0"
           (xsl/match-template "/rss/channel/item"
             [:link {:title "{title}"}])
           (xsl/match-template "/rss"
             [:items (xsl/apply-templates-to "channel/item")]))
  [:xsl/stylesheet
   {:xmlns/xsl "http://www.w3.org/1999/XSL/Transform", :version "1.0"}
   [:xsl/template
    {:match "/rss/channel/item"}
    [:link {:title "{title}"}]]
   [:xsl/template
    {:match "/rss"}
    [:items [:xsl/apply-templates {:select "channel/item"}]]]]

It does not abstract XSLT very much, except for defining some instructions to
accept positional parameters when they are otherwise always required as
attributes. For example, ``<xsl:value-of />`` always requires a ``select``
attribute, so ``<xsl:value-of select="foo" />`` is written simply
``(xsl/value-of "foo")``. Additional, optional attributes can be added by
supplying a map after the positional parameter.

There are a handful of exceptions:

* ``<xsl:template />`` is actually exposed as two separate functions,
  ``match-template`` and ``named-template``, where the positional argument is
  the XPath ``match`` expression and the template name, respectively, since it
  is fairly common to specify either one or the other.

* ``<xsl:choose />``, a particularly contorted and wordy XSLT construct, is
  exposed as ``cond*``, which looks like an ordinary Clojure ``cond`` except
  that in the predicate position are boolean XPath expressions (which appear
  in the ``<xsl:when test="" />`` attribute) or ``:else`` (for
  ``<xsl:otherwise />``), and in the consequent position is the contents of
  the ``when`` or ``otherwise`` instructions. You can put multiple elements
  inside the consequent by placing them in a vector, as long as the vector
  does not start with a keyword:

  .. code:: clojure

    user=> (xsl/cond*
             "foo" (xsl/value-of "foo")
             "bar" :bar
             :else [[:foo "bar"] [:baz "baz"]])
    [:xsl/choose
     [:xsl/when {:test "foo"} [:xsl/value-of {:select "foo"}]]
     [:xsl/when {:test "bar"} :bar]
     [:xsl/otherwise [:foo "bar"] [:baz "baz"]]]

* ``<xsl:if />`` is exposed as ``if*``. Beware that it behaves like XSLT
  ``<xsl:if />`` and does not accept an alternate expression like Clojure's
  ``if``; all arguments after the condition expression are part of the
  consequent. (It is more akin to Clojure's ``when``). If you need to express
  an alternate, use ``cond*``.

* ``<xsl:apply-templates />`` is exposed as ``apply-templates`` for the
  wildcard case, and ``apply-templates-to`` for the selective case. The latter
  accepts as it's positional parameter the XPath expression appearing in the
  ``select`` attribute.

.. [4] ``:use``'ing the ``sxml-jaxp.transform.xslt`` namespace should be done
   with caution, as XSLT uses names for several instructions that collide with
   identically-named Clojure core functions. Use ``:only``, ``:exclude``, or
   ``:refer-clojure`` to control these collisions if you absolutely must
   ``:use`` the XSLT DSL namespace.

XML namespaces
==============

As has been mentioned, ``sxml-jaxp`` is XML-namespace-aware. As you've probably
guessed from the preceding sections, namespaces on keywords in SXML are
interpreted as XML namespace prefixes, e.g. ``:xsl/stylesheet``,
``:xi/include``, or ``:fo/page-sequence``.

Namespace prefix declarations are also specified in an analogous way to XML:
using ``xmlns`` attributes:

.. code:: clojure

  [:html {:xmlns "http://www.w3.org/1999/xhtml",
          :xmlns/xi "http://www.w3.org/2001/XInclude"}
   [:head [:title "Namespace example"]]
   [:xi/include {:href "body.xml"}]]

These attributes are recognized as namespace prefix declarations and
communicated to the various Java XML APIs as required.

Whenever an SXML form is traversed by ``sxml-jaxp``'s SAX reader, a map
contained in ``sxml-jaxp.sax/*default-xmlns*`` is used to resolve un-declared
namespace prefixes:

.. code:: clojure

  user=> (use '[sxml-jaxp.sax :only [*default-xmlns*]])
  nil
  user=> (binding [*default-xmlns* {nil "http://www.w3.org/1999/xhtml",
                                    :xi "http://www.w3.org/2001/XInclude"}]
           (xfm/copy! [:html
                       [:head [:title "Namespace example"]]
                       [:xi/include {:href "body.xml"}]]
                      *out*))
  <?xml version="1.0" encoding="UTF-8"?><html xmlns="http://www.w3.org/1999/xhtml"
                                              xmlns:xi="http://www.w3.org/2001/XInclude">
     <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>Namespace example</title>
     </head>
     <xi:include href="body.xml"></xi:include>
  </html>#<OutputStreamWriter java.io.OutputStreamWriter@484ae502>

Note that for convenience, ``sxml-jaxp.transform.xslt/stylesheet``
automatically declares the ``xsl`` prefix.

SAX event filters
=================

The ``sxml-jaxp.sax.filter`` module allows filters to be inserted which operate
on the SAX event seq [5]_ generated when SXML is fed as input into JAXP. This
API is experimental, but an example application of this is the Hiccup filter in
``sxml-jaxp.sax.filter.hiccup``, which allows writing XHTML ``id`` and
``class`` attributes using Hiccup's shortcut syntax. Here we'll use
``xfm/copy!``'s ``:sxml`` target to help make it clearer what's going on:

.. code:: clojure

  user=> (use '[sxml-jaxp.sax.filter]
              '[sxml-jaxp.sax.filter.hiccup])
  nil
  user=> (def hiccup-example
           [:html
            [:div#main
             [:p.example.first "An example"]
             [:p.example "Another example"]]])
  #'user/hiccup-example
  user=> (xfm/copy! (filter-with [hiccup] hiccup-example) :sxml)
  [:html
   {}
   [:div
    {:id "main"}
    [:p {:class "example first"} "An example"]
    [:p {:class "example"} "Another example"]]]

If an ``:id`` key appears in an element's attribute map, it overrides the
Hiccup-specified one. If a ``:class`` key is present in the attribute map, it
may be a HTML-style space-delimited string, or a set of strings. The class
names so specified are unioned with the Hiccup-specified classes.

.. code:: clojure

  user=> (xfm/copy! (filter-with [hiccup]
                      [:div#old {:id "new"}]) :sxml)
  [:div {:id "new"}]
  user=> (xfm/copy! (filter-with [hiccup]
                      [:div.a.b {:class "b c"}]) :sxml)
  [:div {:class "a b c"}]

.. [5] The SAX event seq format was originally added to decouple SXML traversal
   from the dirty work of interoperating with the Java SAX API. Perhaps in the
   future, the SAX event seq format will be available in more parts of the API,
   to make the filter feature more useful and composable.

SXML precompilation
-------------------

**Here be dragons.**

SXML can be "pre-compiled", in a sense, by converting it to SAX event seq
format ahead of time. This allows the SAX interop to get better performance by
pre-computing the traversal of an oft-used SXML form. The
``sxml-jaxp.transform`` APIs all accept this format as input.  The easiest way
to use this is the ``sxml-jaxp.sax/compiled-sxml`` macro, which will
pre-compile a literal SXML form at compile time:

.. code:: clojure

  user=> (let [a "foo" b "bar" c "baz"]
           (xfm/copy! (sax/compiled-sxml [:root a b [:c c]])))
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root>foobar<c>baz</c></root>"

It comes in a vanilla function version as well, ``compile-sxml``.

However, there are several caveats in the current implementation (which may be
fixable but I haven't thought about it enough):

* Expressions may be used in the content of a pre-compiled literal, but in the
  current implementation, they are are fixed as element names when at the head
  of a vector, and as text nodes anywhere else. They cannot affect the element
  structure of the resulting document:

  .. code:: clojure

    user=> (let [elem :go]
             (xfm/copy! (sax/compiled-sxml [:ready :set elem [elem]])))
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><ready><set/>:go<go/></ready>"
    user=> (let [fail [:fail "fail!"]]
             (xfm/copy! (sax/compiled-sxml [:ready :set fail])))
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><ready><set/>[:fail \"fail!\"]</ready>"

* Attributes must be literal maps, but they may contain expressions in the
  key and value positions. This is probably much easier to fix.

Limitations and future work
===========================

* Currently the SXML parser ignores processing instructions, and there is no
  way to express a processing instruction in SXML. Advice and suggestions
  welcome.

* Add support for interoperating with the ``clojure.xml`` data format, and
  investigate if there is anything interesting to add for traversing SXML with
  ``clojure.walk``.

* XPath support would be pretty awesome. This can probably be done by providing
  some help with feeding SXML into a ``Document``, along with a function to run
  XPath expressions against it and SXMLify the result.

* Allow filters to be more composable by separating the SAX parser into two
  stages, such that an event seq is generated first, and the shift-reduce SXML
  generation operates on that. Then stream filters can be inserted between
  them. Currently the SAX handler directly feeds the SXML generator.

License
=======

``sxml-jaxp`` is Copyright (C) 2010-2013 Kyle Schaffrick.

Distributed under the Eclipse Public License, the same as Clojure.

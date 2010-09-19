========
sxml-sax
========

**sxml-sax** is a library of tools for using SXML-inspired XML representations
with the Java XML infrastructure. It uses JAXP internally, so it does not
depend on any particular XML library, although the author has personally tested
it with the OpenJDK default Xerces/Xalan and Saxon 9.1.

Included are tools for reading XML into SXML forms (using SAX), transforming
SXML using XSL Transformations as well as converting it back to XML (using the
TrAX APIs).

Clojure-flavored SXML
=====================

Clojure-flavored SXML uses vectors, keywords, and maps to denote elements,
tags, and attributes. [1]_ ::

  [:item {}
    [:title {} "Memorandum"]
    [:author {} "tyler.durden@paperstreetsoap.com"]
    [:media:content {:url "http://paperstreetsoap.com/snowflake.jpg",
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

There is also a *simplified* form::

  [:item
    [:title "Memorandum"]
    [:author "tyler.durden@paperstreetsoap.com"]
    [:media:content {:url "http://paperstreetsoap.com/snowflake.jpg",
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

``sxml-sax`` provides the function ``normalize`` to convert SXML to normalized
form, and ``simplify`` to convert to simplified form. Functions generally
accept either form (or some mixture), but typically return normalized form.

.. [1] This is in contrast with the original Scheme SXML which uses lists and
   symbols for everything.

Usage
=====

Here are some examples of using ``sxml-sax`` for various tasks. We'll assume
the following ``require``'s::

  (require '[sxml-sax.core :as s]
           '[sxml-sax.xslt :as t]
           '[sxml-sax.xslt.lang :as xsl]
           '[clojure.java.io :as io])

Parsing XML to SXML
-------------------

You can use the ``sxml-sax.core/read-sxml`` function to read from some source
of XML data and return it in SXML format::

  user=> (s/read-sxml "<greet>Hello world!</greet>")
  [:greet {} "Hello world!"]

You can use ``Reader``'s, ``InputStream``'s, and ``File``'s too. ::

  user=> (s/read-sxml (java.io.StringReader. "<greet>Hello world!</greet>"))
  [:greet {} "Hello world!"]

Navigation helpers
------------------

There are some trivial helper functions to allow you to access the components
of SXML elements easily. ::

  user=> (def fancy-hello [:greet {:language "en"} "Hello world!"])
  #'user/fancy-hello
  user=> (s/tag fancy-hello)
  :greet
  user=> (s/attrs fancy-hello)
  {:language "en"}
  user=> (s/children fancy-hello)
  ("Hello world!")

These are marginally more useful than regular vector access methods because
they work on SXML that might not be normalized::

  user=> (def simple-hello [:greet "Hello world!"])
  #'user/simple-hello
  user=> (s/attrs simple-hello)
  {}
  user=> (s/children simple-hello)
  ("Hello world!")

Outputting to XML
-----------------

The ``sxml-sax.xslt/copy!`` function can be used to copy SXML into various
kinds of output "sinks". Here, we'll use a ``Writer``. Notice it returns the
thing you passed as the "sink" so you can do more stuff with it::

  user=> (.toString (t/copy! fancy-hello (java.io.StringWriter.)))
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><greet language=\"en\">Hello world!</greet>"

``copy!`` also recognizes the special sink ``:string``, which is the default
when you don't provide a sink. [2]_ This causes it to return the source as a
string of XML::

  user=> (t/copy! fancy-hello :string)
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><greet language=\"en\">Hello world!</greet>"
  user=> (t/copy! fancy-hello)
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><greet language=\"en\">Hello world!</greet>"

XSL Transforms
--------------

Transformations are performed with the ``sxml-sax.xslt/transform!`` function.
This accepts a stylesheet, a source, and a result. I'll use the XSLT DSL
(defined in ``sxml-sax.xslt.lang``) to create XSLT stylesheets. ::

  user=> (t/transform! (xsl/stylesheet "1.0"
                         (xsl/match-template "/once-old"
                           [:new-again (xsl/copy-of "@*|node()")]))
                       [:once-old "Hi!"])
  [:new-again {} "Hi!"]

I didn't provide a target for the result, so it defaulted to the special target
``:sxml`` [2]_. Like ``copy!``, it recognizes the special target ``:string`` as
well, and you can use any other reasonable object as your result target.

Here's a more complex example, getting a seq of the latest article titles on
Ars Technica using their RSS feed::

  user=> (def rss-title-tmpl
           (t/compile-template
             (xsl/stylesheet "1.0"
               (xsl/match-template "/rss/channel/item"
                 [:link {:title "{title}"}])
               (xsl/match-template "/rss"
                 [:items (xsl/apply-templates-to "channel/item")]))))
  #'user/rss-title-tmpl
  user=> (with-open [at-rss-in (io/input-stream
                                 "http://feeds.arstechnica.com/arstechnica/everything")]
           (map (comp :title s/attrs)
                (s/children (t/transform! rss-title-tmpl at-rss-in))))
  ("Week in Apple: OS X beta anniversary, nano review, HDR photography"
   "Week in tech: first sale fail, DRM fail, adult services fail"
   "Week in gaming: Halo Reach! Civilization! Hunting! Come in! "
   "Week in Microsoft: IE9 beta arrives"
   "Ex-child prostitute sues Village Voice over sex ads"
   "Lawsuit: T-Mobile text blocking is harshing our buzz, man"
   "FaceTime-equipped iPad expected no later than first quarter 2011"
   "Microsoft says patent-infringing Android isn't really free "
   "RCN P2P settlement: ISP can throttle away starting November 1"
   "Verizon LTE in 30 cities by year end, AT&T aims for mid-2011"
   "Move, dodge, kill: Time Crisis Razing Storm on the PS3 gets pirates"
   "Intel confirms HDCP key is real, can now be broken at will"
   "Windows Phone 7 SDK here; YouTube, Netflix demoed; no CDMA yet"
   "For crows, a little tool use goes a long way"
   "Feature: The history of Civilization: 20 years of Wonders"
   "HTC moves beyond the phone, marginalizes Google in the process"
   "Feature: BodyMedia FIT review: data, data, and more data for exercisers"
   "Lawsuit targets advertiser over sneaky HTML5 pseudo-cookies"
   "Galaxy Tab coming to all US carriers; no pricing yet, no 4G"
   "Apple TV definitely running iOS, could be jailbreak target"
   "P2P defendants demand legal fees from Far Cry filmmaker"
   "Harder for kids to buy M-rated video game than see R-rated movie"
   "Open source Facebook replacement Diaspora drops first alpha"
   "Skyhook: Google made OEMs break business deals, infringed patents"
   "Strange summer melt leaves Arctic ice near record low")

Here we've pre-compiled our XSL template using ``compile-template``. This can
be used if you plan on transforming more than one document with a particular
stylesheet. It uses TrAX to compile the template into some object implementing
``Transformer``, so that it doesn't have to parse and compile it for every
invocation.

.. [2] ``copy!`` actually recognizes the ``:sxml`` sink also, although I don't
   know why you'd ever need that; generally you'd want to use
   ``sxml-sax.core/read-sxml`` which bypasses TrAX and reads the input directly
   with SAX.

XSLT DSL
........

The namespace ``sxml-sax.xslt.lang`` [3]_ defines a DSL for writing XSL
transformation stylesheets in Clojure. This DSL outputs the stylesheets in SXML
format. Here's the template we used in the last example::

  user=> (xsl/stylesheet "1.0"
           (xsl/match-template "/rss/channel/item"
             [:link {:title "{title}"}])
           (xsl/match-template "/rss"
             [:items (xsl/apply-templates-to "channel/item")]))
  [:xsl:stylesheet
   {:version "1.0"}
   [:xsl:template
    {:match "/rss/channel/item"}
    [:link {:title "{title}"}]]
   [:xsl:template
    {:match "/rss"}
    [:items [:xsl:apply-templates {:select "channel/item"}]]]]

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
  does not start with a keyword::

    user=> (xsl/cond*
             "foo" (xsl/value-of "foo")
             "bar" :bar
             :else [[:foo "bar"] [:baz "baz"]])
    [:xsl:choose
     [:xsl:when {:test "foo"} [:xsl:value-of {:select "foo"}]]
     [:xsl:when {:test "bar"} :bar]
     [:xsl:otherwise [:foo "bar"] [:baz "baz"]]]

* ``<xsl:if />`` is exposed as ``if*``. Beware that it behaves like XSLT
  ``<xsl:if />`` and does not accept an alternate expression like Clojure's
  ``if``; all arguments after the condition expression are part of the
  consequent. (It is more akin to Clojure's ``when``). If you need to express
  an alternate, use ``cond*``.

* ``<xsl:apply-templates />`` is exposed as ``apply-templates`` for the
  wildcard case, and ``apply-templates-to`` for the selective case. The latter
  accepts as it's positional parameter the XPath expression appearing in the
  ``select`` attribute.

.. [3] ``:use``'ing the ``sxml-sax.xslt.lang`` namespace should be done with
   caution, as XSLT uses names for several instructions that collide with
   identically-named Clojure core functions. Use ``:only``, ``:exclude``, or
   ``:refer-clojure`` to control these collisions if you absolutely must
   ``:use`` the XSLT DSL namespace.

XML namespaces
==============

``sxml-sax`` is XML-namespace-aware. As you've probably guessed from the last
section, you can specify a namespace prefix on a tag name in the same way as
you would in regular XML, e.g. ``:xsl:stylesheet``, ``:xi:include``, or
``:fo:page-sequence``.

Namespace prefix declarations are also specified in an analogous way to XML:
using ``xmlns`` attributes::

  [:html {:xmlns "http://www.w3.org/1999/xhtml",
          :xmlns:xi "http://www.w3.org/2001/XInclude"}
   [:head [:title "Namespace example"]]
   [:xi:include {:href "body.xml"}]]

These attributes are recognized as namespace prefix declarations and
communicated to the various Java XML APIs as required.

Whenever an SXML form is traversed by ``sxml-sax``, a map contained in
``sxml-sax.core/*default-xmlns*`` is used to resolve un-declared namespace
prefixes::

  user=> (binding [s/*default-xmlns* {nil "http://www.w3.org/1999/xhtml",
                                      :xi "http://www.w3.org/2001/XInclude"}]
           (t/copy! [:html
                     [:head [:title "Namespace example"]]
                     [:xi:include {:href "body.xml"}]]
                    *out*))
  <?xml version="1.0" encoding="UTF-8"?><html xmlns="http://www.w3.org/1999/xhtml"
                                              xmlns:xi="http://www.w3.org/2001/XInclude">
     <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>Namespace example</title>
     </head>
     <xi:include href="body.xml"></xi:include>
  </html>#<OutputStreamWriter java.io.OutputStreamWriter@484ae502>

Note that for convenience, ``sxml-sax.xslt`` automatically declares the ``xsl``
prefix whenever it parses a stylesheet that is expressed in SXML.

License
=======

``sxml-sax`` is Copyright (C) 2010 Kyle Schaffrick.

Distributed under the Eclipse Public License, the same as Clojure.

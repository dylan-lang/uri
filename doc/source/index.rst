***
URI
***

The URI library exports interfaces to represent and manipulate Uniform Resource
Identifiers (URIs) as described in `RFC 3986
<https://datatracker.ietf.org/doc/html/rfc3986>`_.

.. toctree::
   :hidden:
   :maxdepth: 2

.. current-library:: uri
.. current-module:: uri

Overview
========

The primary class exported by the URI library is :class:`<uri>`.  URIs are usually
created with :func:`string-to-uri`, and converted back to a string (e.g., for display or
for transmission over the network) via :func:`uri-to-string`.

URIs are immutable objects and all functions for retrieving elements of the URI such as
host and path start with a ``uri-`` prefix: :gf:`uri-host`, :gf:`uri-port`, etc.

Many characters in a URI may be "percent encoded" to remove ambiguity in parsing and to
encode non-ASCII characters when sending over the network.  The functions in this module
named with a ``uri-raw-`` prefix return these percent encoded strings and functions
without "raw" in the name return "decoded" strings.



The URI module
==============

.. constant:: <string?>

   This is a non-exported constant that is equivalent to ``false-or(<string>)``. It is
   used here solely to reduce the verbosity of this documentation since most
   :class:`<uri>` methods use this type.

Errors
------

.. class:: <uri-error>

   :superclasses: :drm:`<simple-error>`

   All errors explicitly signaled by the URI module are instances of this class.

.. class:: <uri-parse-error>

   :superclasses: :class:`<uri-error>`

   All errors explicitly signaled by :func:`string-to-uri` are instances of this class.

URI Class
---------

.. class:: <uri>
   :open:

   :superclasses: :drm:`<object>`

   :keyword scheme: An instance of :const:`<string?>`.
   :keyword user-info: An instance of :const:`<string?>`.
   :keyword raw-user-info: An instance of :const:`<string?>`.
   :keyword host: An instance of :const:`<string?>`.
   :keyword raw-host: An instance of :const:`<string?>`.
   :keyword port: An instance of :const:`<integer?>`.
   :keyword path: An instance of :drm:`<list>`.
   :keyword raw-path: An instance of :const:`<string?>`.
   :keyword query: An instance of ``false-or(<string-table>)``.
   :keyword raw-query: An instance of :const:`<string?>`.
   :keyword fragment: An instance of :const:`<string?>`.
   :keyword raw-fragment: An instance of :const:`<string?>`.

   This class represents a URI that has been decomposed into its constituent parts.
   Instances should generally be created via :func:`string-to-uri`.  When calling
   :drm:`make` directly it is only necessary to supply either the "raw" (percent encoded)
   or "non-raw" (percent decoded) init value; the other value will be computed if
   accessed.

Convert to/from Strings
-----------------------

.. function:: string-to-uri

   :signature: string-to-uri(*string* :: :drm:`<string>`) => (*uri* :: :class:`<uri>`)

   Parse *string*, the string representation of a URI, into its component parts and
   return it as a :class:`<uri>`.

.. function:: split-uri

   :signature: split-uri (*string* :: :drm:`<string>`) => (*scheme* ::
               :const:`<string?>`, *user-info* :: :const:`<string?>`, *host* ::
               :const:`<string?>`, *port* :: :const:`<string?>`, *path* ::
               :const:`<string?>`, *query* :: :const:`<string?>`, *fragment* ::
               :const:`<string?>`)

   Splits *string* into its component parts. Except for *port*, which is an
   :const:`<integer?>`, all values are instances of :const:`<string?>` which have *not*
   been percent decoded.  That is, assuming the original *string* was percent encoded,
   the return values are appropriate for using as the ``raw-*`` init arguments to
   ``make(<uri>)``.

   This function is used by :gf:`string-to-uri` and is intended to be used by subclassers
   of :class:`<uri>` for purposes of instance creation if they need to provide additional
   initialization arguments.

.. function:: uri-to-string

   :signature: uri-to-string (*uri* :: :class:`<uri>`, #key *scheme?* :: :drm:`<boolean>`, *authority?* :: :drm:`<boolean>`) => (*string* :: :drm:`<string>`)

   Returns the percent encoded string representation of *uri*.  *scheme?* and
   *authority?* both default to true.

URI Components
--------------

.. generic-function:: uri-scheme

   :signature: uri-scheme (*uri* :: :class:`<uri>`) => (*scheme* :: :const:`<string?>`)

   If *uri* has a scheme part, this function returns it as a :drm:`<string>`, otherwise
   it returns :drm:`#f`.  If *uri* was created by :func:`string-to-uri` the scheme will
   be all lowercase.  Example:

   .. code:: dylan-repl

      ? string-to-uri("S://u:pw@h:77/p?q#f").uri-scheme
      $0 = "s"

.. generic-function:: uri-user-info

   :signature: uri-user-info (*uri* :: :class:`<uri>`) => (*info* :: :const:`<string?>`)

   Returns the user-info component of *uri* as a percent decoded string, or :drm:`#f`.
   Example:

   .. code:: dylan-repl

      ? string-to-uri("S://u:p%40ss@h:77/p?q#f").uri-user-info
      $0 = "u:p@ss"

.. generic-function:: uri-raw-user-info

   :signature: uri-raw-user-info (*uri* :: :class:`<uri>`) => (*info* :: :const:`<string?>`)

   Returns the user-info component of *uri* as a percent encoded string, or :drm:`#f`.
   Example:

   .. code:: dylan-repl

      ? string-to-uri("S://u:p%40ss@h:77/p?q#f").uri-raw-user-info
      $0 = "u:p%40ss"

.. generic-function:: uri-host

   :signature: uri-host (*uri* :: :class:`<uri>`) => (*host* :: :const:`<string?>`)

   Returns the host component of *uri* as a percent decoded string, or :drm:`#f`.
   Example:

   .. code:: dylan-repl

      ? string-to-uri("S://u:pw@h%71:77/p?q#f").uri-host
      $0 = "hq"

.. generic-function:: uri-raw-host

   :signature: uri-raw-host (*uri* :: :class:`<uri>`) => (*host* :: :const:`<string?>`)

   Returns the host component of *uri* as a percent encoded string, or :drm:`#f`.
   Example:

   .. code:: dylan-repl

      ? string-to-uri("S://u:pw@h%71:77/p?q#f").uri-raw-host
      $0 = "h%71"

.. generic-function:: uri-port

   :signature: uri-port (*uri* :: :class:`<uri>`) => (*port* :: :const:`<integer?>`)

   Returns the port component of *uri* as an integer, or :drm:`#f`. Example:

   .. code:: dylan-repl

      ? string-to-uri("S://u:pw@h:77/p?q#f").uri-port
      $0 = 77

.. generic-function:: uri-authority

   :signature: uri-authority (*uri* :: :class:`<uri>`, #key *percent-encoded?* ::
               :drm:`<boolean>`) => (*authority* :: :const:`<string?>`)

   Returns the port component of *uri* as a string, or :drm:`#f`. If *percent-encoded?*
   is true the returned string is percent encoded. The default is false. Example:

   If *uri* has an authority part, this function returns it as a :drm:`<string>`,
   otherwise it returns :drm:`#f`. Example:

   .. code:: dylan-repl

      ? string-to-uri("s://u:pw@h:77/p?q#f").uri-authority
      $0 = "u:pw@h:77"

   Note that per `RFC 3986
   <https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.3>`_,

     "URI producers and normalizers should omit the port component and its ':' delimiter
     if port is empty or if its value would be the same as that of the scheme's default."

   Accordingly, for example, ``uri-authority(string-to-uri("https://foo.org:443"))``
   returns ``"foo.org"``, not ``"foo.org:443"``.

.. generic-function:: uri-path
   :open:

   :signature: uri-path (*uri* :: :class:`<uri>`) => (*path* :: :drm:`<object>`)

   Returns the percent decoded path component of *uri*, or :drm:`#f`.

.. method:: uri-path
   :specializer: <uri>

   Returns the percent decoded path component of *uri* as a list of percent decoded path
   segments.  The list is created by ``split(raw-path, '/')``, therefore it will contain
   an empty string as the first element if the path starts with "/".  If there is no
   path, ``#()`` is returned.

   Example:

   .. code:: dylan-repl

      ? string-to-uri("S://u:pw@h:77/p?q#f").uri-path
      $0 = #("", "p")

.. generic-function:: uri-raw-path

   :signature: uri-raw-path (*uri* :: :class:`<uri>`) => (*path* :: :const:`<string?>`)

   Returns the path component of *uri* as a percent encoded string, or :drm:`#f`.
   Example:

   .. code:: dylan-repl

      ? string-to-uri("S://u:pw@h:77/p?q#f").uri-raw-path
      $0 = "/p"

.. generic-function:: uri-query
   :open:

   :signature: uri-query (*uri* :: :class:`<uri>`) => (*qvalues* :: :drm:`<object>`)

   Returns the query component of *uri*.

.. method:: uri-query
   :specializer: <uri>

   :signature: uri-query (*uri* :: :class:`<uri>`) => (*qvalues* :: ``false-or(<string-table>``)

   Returns the query part of a URI as a table mapping query keys to their corresponding
   (percent decoded) values.  For example, for the query string ``"a=1&b=%40&c&c=&c=v"``
   the returned table looks like this:

   +---------+---------------------+
   |   Key   | Value               |
   +=========+=====================+
   | ``"a"`` | ``"1"``             |
   +---------+---------------------+
   | ``"b"`` | ``"@"``             |
   +---------+---------------------+
   | ``"c"`` | ``#("", "v", #t)``  |
   +---------+---------------------+

   Notice,

   1. The value associated with "b" was percent decoded.
   2. "c" appeared multiple times so its value is a list.
   3. The value associated with "c=" is the empty string.
   4. The value associated with "c" (with no "=") is :drm:`#t`.
   5. The order of the "c" list is not guaranteed; if you depend on it your code may
      break in a future release.

   .. code:: dylan-repl

      ? string-to-uri("S://u:pw@h:77/p?q=1#f").uri-query
      $0 = {<string-table> size: 1}
      ? uri-query-value($0, "q")
      $1 = "1"

   It is recommended to use :gf:`uri-query-value` instead of accessing the
   :gf:`uri-query` table directly.

.. generic-function:: uri-raw-query

   :signature: uri-raw-query (*uri* :: :class:`<uri>`) => (*query* :: :const:`<string?>`)

   .. code:: dylan-repl

      ? string-to-uri("S://u:pw@h:77/p?q%26=2#f").uri-raw-query
      $0 = "q%26=2"

.. generic-function:: uri-query-value
   :open:

   :signature: uri-query-value (*uri* :: :class:`<uri>`, *key* :: :drm:`<string>`, #key *all?* :: :drm:`<boolean>`) => (*value* :: :drm:`<object>`)

.. method:: uri-query-value
   :specializer: <uri>

   :signature: uri-query-value (*uri* :: :class:`<uri>`, *key* :: :drm:`<string>`, #key *all?* :: :drm:`<boolean>`) => (*value* :: :drm:`<object>`)

   This method provides an easier and more future-proof way to access individual query
   values, rather than directly accessing the :class:`<string-table>` returned by
   :gf:`uri-query`.

   By default :gf:`uri-query-value` returns a single string or :drm:`#t`, i.e., if *key*
   maps to a list of values (see :gf:`uri-query`) one of the values is chosen
   arbitrarily.  With ``all?: #t`` the entire list is returned.

.. generic-function:: uri-fragment

   :signature: uri-fragment (*uri* :: :class:`<uri>`) => (*fragment* :: :const:`<string?>`)

   .. code:: dylan-repl

      ? string-to-uri("S://u:pw@h:77/p?q#f%40").uri-fragment
      $0 = "f@"

.. generic-function:: uri-raw-fragment

   :signature: uri-raw-fragment (*uri* :: :class:`<uri>`) => (*fragment* :: :const:`<string?>`)

   .. code:: dylan-repl

      ? string-to-uri("S://u:pw@h:77/p?q#f%40").uri-raw-fragment
      $0 = "f%40"

.. function:: uri-relative?

   :signature: uri-relative? (*uri* :: :class:`<uri>`) => (*relative?* :: :drm:`<boolean>`)

   Essentially a URI is relative if it does not have a scheme.  See `the URI RFC
   <https://datatracker.ietf.org/doc/html/rfc3986#section-4.1>`_ for details.

Merging URIs
------------

.. generic-function:: merge-uris
   :open:

   :signature: merge-uris (*base* :: :class:`<uri>`, *reference* :: :class:`<uri>`, #key *strict?* :: :drm:`<boolean>`) => (*merged* :: :class:`<uri>`)

Normalization and Comparison
----------------------------

`URI normalization and comparison
<https://datatracker.ietf.org/doc/html/rfc3986#section-6>`_ are not yet implemented.

Percent Encoding
----------------

.. generic-function:: percent-encode

   :signature: percent-encode (*string* :: :drm:`<byte-string>`, *charset* :: :drm:`<table>`) => (*encoded-string* :: :drm:`<byte-string>`)

   Returns a :drm:`<string>` in which any character in *string* that isn't part of
   *charset* is percent encoded.  *charset* is typically one of the character sets
   documented below, depending on what component of a URI is being percent encoded.

.. constant:: $charset-user-info

   The character set comprised of all characters that are valid in the user-info
   component of a URI.

.. constant:: $charset-host

   The character set comprised of all characters that are valid in the host component of
   a URI.

.. constant:: $charset-path

   The character set comprised of all characters that are valid in the path component of
   a URI.

.. constant:: $charset-query

   The character set comprised of all characters that are valid in the query component of
   a URI.

.. constant:: $charset-fragment

   The character set comprised of all characters that are valid in the fragment component
   of a URI.

.. generic-function:: percent-decode

   :signature: percent-decode (*string* :: :drm:`<byte-string>`, #key *strict?* :: :drm:`<boolean>`) => (*unencoded* :: :drm:`<byte-string>`)

   Returns a :drm:`<string>` in which any percent escape sequence (e.g., "%40") is
   replaced by the character with the corresponding hex value.  For example,

   .. code:: dylan-repl

      ? percent-decode("a%40b")
      $0 = "a@b"

   If *strict?* is true, an error will be signaled for invalid percent encodings such as
   "100%" or "a % b".  The default is false, meaning to simply include a literal "%" in
   the result.

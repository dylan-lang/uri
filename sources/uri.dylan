Module: uri-internal
Synopsis: RFC 3986: Uniform Resource Identifier (URI): Generic Syntax
Copyright:    Original Code is Copyright (c) 2018 Dylan Hackers
              All rights reserved.
License:      See License.txt in this distribution for details.

// RFC 3986: https://datatracker.ietf.org/doc/html/rfc3986

// Brevity
define constant <string?> = false-or(<string>);
define constant <integer?> = false-or(<integer>);


define open class <uri> (<object>)
  // For slots that have %foo and %raw-foo varieties, the raw slot is percent encoded and
  // the other is not.  If one of them is supplied at init time the other will be
  // computed from it.

  // Always lowercased if the URI was created by string-to-uri.
  constant slot %scheme :: <string?> = #f, init-keyword: scheme:;

  slot %raw-user-info :: <string?> = #f, init-keyword: raw-user-info:;
  slot %user-info     :: <string?> = #f, init-keyword: user-info:;

  // Host, always lowercased if the URI was created by string-to-uri.
  slot %raw-host :: <string?> = #f, init-keyword: raw-host:;
  slot %host     :: <string?> = #f, init-keyword: host:;

  constant slot %port :: <integer?> = #f, init-keyword: port:;

  slot %raw-path :: <string?> = #f, init-keyword: raw-path:;
  // The path is stored as a list of percent-decoded path segments, as created by
  // `split(path, '/')`.  As a consequence, #() can be used to indicate that this slot
  // hasn't yet been set from `%raw-path`.
  slot %path :: <list> = #(), init-keyword: path:;

  // The original URI query string (no leading '?').
  slot %raw-query :: <string?> = #f, init-keyword: raw-query:;
  // Map of query percent-decoded keys to percent-decoded values, computed lazily in
  // `uri-query`.  Values can be one of three types: (1) string, (2) singleton(#t), (3)
  // list with no guaranteed order. Examples:
  //
  //   Query string    "k"s value
  //   "k=v"           "v"
  //   "k="            ""
  //   "k"             #t  (so that #f can indicate k doesn't exist at all)
  //   "k=a&k=b"       #("a", "b") or #("b", "a")
  slot %query :: false-or(<string-table>) = #f, init-keyword: query:;

  slot %raw-fragment :: <string?> = #f, init-keyword: raw-fragment:;
  slot %fragment     :: <string?> = #f, init-keyword: fragment:;
end class;


// --------------------------------------------------------------------------------
// Exported generic functions

// uri-path, uri-query, and uri-query-value are the only open generics because they're
// the only ones with interesting enough implementations for which I (cgay) can imagine
// someone wanting to provide alternatives. We'll see.

define sealed generic uri-scheme        (uri :: <uri>) => (scheme :: <string?>);
define sealed generic uri-user-info     (uri :: <uri>) => (info :: <string?>);
define sealed generic uri-raw-user-info (uri :: <uri>) => (info :: <string?>);
define sealed generic uri-host          (uri :: <uri>) => (host :: <string?>);
define sealed generic uri-raw-host      (uri :: <uri>) => (host :: <string?>);
define sealed generic uri-port          (uri :: <uri>) => (port :: <integer?>);
define open   generic uri-path          (uri :: <uri>) => (path :: <object>);
define sealed generic uri-raw-path      (uri :: <uri>) => (path :: <string?>);
define open   generic uri-query         (uri :: <uri>) => (query :: <object>);
define sealed generic uri-raw-query     (uri :: <uri>) => (query :: <string?>);
define sealed generic uri-fragment      (uri :: <uri>) => (frag :: <string?>);
define sealed generic uri-raw-fragment  (uri :: <uri>) => (frag :: <string?>);

// Get the value associated with a given key in the URI query.  A key may appear in a URI
// query more than once.  To retrieve all values for a key (as a list) use `all?: #t`.
// With `all?: #f` (the default) only one (arbitrary) value is returned.
define open generic uri-query-value
    (uri :: <uri>, key :: <string>, #key all?) => (value :: <object>);

define sealed generic uri-authority
    (uri :: <uri>, #key percent-encoded?) => (authority :: <string?>);

define sealed generic has-authority?
    (uri :: <uri>) => (_ :: <boolean>);


define sealed generic percent-encode
    (string :: <byte-string>, charset :: <table>) => (encoded :: <byte-string>);
define sealed generic percent-decode
    (string :: <byte-string>, #key strict?) => (unencoded :: <byte-string>);


define open generic merge-uris
    (base :: <uri>, reference :: <uri>, #key strict?) => (target :: <uri>);


// --------------------------------------------------------------------------------
// Implementation

define inline method uri-scheme (u :: <uri>) => (scheme :: <string?>)
  u.%scheme
end method;

define inline method uri-raw-user-info (u :: <uri>) => (info :: <string?>)
  if (~u.%raw-user-info & u.%user-info)
    u.%raw-user-info := percent-encode(u.%user-info, $charset-user-info);
  end;
  u.%raw-user-info
end method;

define inline method uri-user-info (u :: <uri>) => (info :: <string?>)
  if (~u.%user-info & u.%raw-user-info)
    u.%user-info := percent-decode(u.%raw-user-info);
  end;
  u.%user-info
end method;

define inline method  uri-raw-host (u :: <uri>) => (host :: <string?>)
  if (~u.%raw-host & u.%host)
    u.%raw-host := percent-encode(u.%host, $charset-host);
  end;
  u.%raw-host
end method;

define inline method  uri-host (u :: <uri>) => (host :: <string?>)
  if (~u.%host & u.%raw-host)
    u.%host := percent-decode(u.%raw-host);
  end;
  u.%host
end method;

define inline method uri-port (u :: <uri>) => (port :: <integer?>)
  u.%port
end method;

define inline method uri-raw-path (u :: <uri>) => (path :: <string?>)
  if (~u.%raw-path & ~empty?(u.%path))
    u.%raw-path := percent-encode(join(u.%path, "/"), $charset-path);
  end;
  u.%raw-path
end method;

// The default implementation returns the segments, but subclassers may decide to return
// a percent decoded string, a <path> object, etc.
define method uri-path (u :: <uri>) => (path :: <list>)
  // Note that split("", '/') => #("") and split("/", '/') => #("", "") so empty? means
  // %path hasn't been computed yet.
  if (empty?(u.%path) & u.%raw-path)
    let path = map(percent-decode, split(u.%raw-path, '/'));
    if (u.%scheme)
      // A scheme means URI is absolute, by definition, and we remove dot segments.
      path := remove-dot-segments(path);
    end;
    u.%path := path
  end;
  u.%path
end method;

define inline method uri-raw-query (u :: <uri>) => (query :: <string?>)
  u.%raw-query
    | if (u.%query)
        // query: was provided but raw-query: wasn't.  No order is guaranteed here.
        u.%raw-query := query-values-to-string(u.%query);
      end
end method;

define function query-values-to-string (qvalues :: <table>) => (query :: <string>)
  let keyvals = #();
  for (v keyed-by k :: <string> in qvalues)
    select (v by instance?)
      singleton(#t) => push(k, keyvals);
      <string>      => push(concatenate(k, "=", v), keyvals);
      <list>        => for (v in v)
                         push(if (v == #t) k else concatenate(k, "=", v) end,
                              keyvals);
                       end;
    end select;
  end for;
  join(keyvals, "&")
end function;

define inline method  uri-query (u :: <uri>) => (qvalues :: false-or(<string-table>))
  u.%query | (u.%query := u.%raw-query & split-query(u.%raw-query))
end method;

define method uri-query-value
    (u :: <uri>, key :: <string>, #key all? :: <boolean>)
 => (value :: <object>)
  let qvalues = u.uri-query;
  if (qvalues)
    let value = element(qvalues, key, default: #f);
    if (instance?(value, <list>))
      if (all?)
        value
      else
        value.head
      end
    else
      value
    end
  end
end method;

define inline method  uri-raw-fragment (u :: <uri>) => (fragment :: <string?>)
  if (~u.%raw-fragment & u.%fragment)
    u.%raw-fragment := percent-encode(u.%fragment, $charset-fragment);
  end;
  u.%raw-fragment
end method;

define inline method  uri-fragment (u :: <uri>) => (fragment :: <string?>)
  if (~u.%fragment & u.%raw-fragment)
    u.%fragment := percent-decode(u.%raw-fragment);
  end;
  u.%fragment
end method;

// FIXME -- Implement the following restrictions in the initialize method
//          for the <uri> class...
//   The scheme and path components are required, though the path may be
//   empty (no characters).  When authority is present, the path must
//   either be empty or begin with a slash ("/") character.  When
//   authority is not present, the path cannot begin with two slash
//   characters ("//").  When authority is present it must have a host
//   component.
//   Need an error class for these.  e.g., <invalid-uri-error>

define class <uri-error> (<simple-error>) end;
define class <uri-parse-error> (<uri-error>) end;

define inline function uri-error
    (format-string :: <string>, #rest format-args)
  signal(make(<uri-error>,
              format-string: format-string,
              format-arguments: format-args));
end function;

define inline function uri-parse-error
    (format-string :: <string>, #rest format-args)
  signal(make(<uri-parse-error>,
              format-string: format-string,
              format-arguments: format-args));
end function;

define inline method has-authority?
    (uri :: <uri>) => (_ :: <boolean>)
  (uri.%host | uri.%raw-host) & #t // host is a required part of the authority
end method;

// RFC 3986, Section 3.2
//       authority   = [ userinfo "@" ] host [ ":" port ]
define method uri-authority
    (uri :: <uri>, #key percent-encoded? :: <boolean>) => (authority :: <string?>)
  if (has-authority?(uri))
    let uinfo = if (percent-encoded?) uri.uri-raw-user-info else uri.uri-user-info end;
    let host = if (percent-encoded?) uri.uri-raw-host else uri.uri-host end;
    // URI producers and normalizers should omit the port component and its ":" delimiter
    // if port is empty or if its value would be the same as that of the scheme's
    // default.  https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.3
    let port = if (uri.%port
                     & ~(uri.%scheme
                           & (uri.%port == default-port-for-scheme(uri.%scheme))))
                 uri.%port
               end;
    concatenate(uinfo | "",
                if (uinfo) "@" else "" end,
                host,
                if (port) ":" else "" end,
                if (port) integer-to-string(port) else "" end)
  end
end method;

define function default-port-for-scheme (scheme :: <string>) => (port :: <integer?>)
  select (scheme by string-equal?)
    // This list was taken from
    // https://gist.github.com/mahmoud/2fe281a8daaff26cfe9c15d2c5bf5c8b and I removed the
    // null entries.
    "acap"     => 674;
    "afp"      => 548;
    "dict"     => 2628;
    "dns"      => 53;
    "ftp"      => 21;
    "git"      => 9418;
    "gopher"   => 70;
    "http"     => 80;
    "https"    => 443;
    "imap"     => 143;
    "ipp"      => 631;
    "ipps"     => 631;
    "irc"      => 194;
    "ircs"     => 6697;
    "ldap"     => 389;
    "ldaps"    => 636;
    "mms"      => 1755;
    "msrp"     => 2855;
    "mtqp"     => 1038;
    "nfs"      => 111;
    "nntp"     => 119;
    "nntps"    => 563;
    "pop"      => 110;
    "prospero" => 1525;
    "redis"    => 6379;
    "rsync"    => 873;
    "rtsp"     => 554;
    "rtsps"    => 322;
    "rtspu"    => 5005;
    "sftp"     => 22;
    "smb"      => 445;
    "snmp"     => 161;
    "ssh"      => 22;
    "svn"      => 3690;
    "telnet"   => 23;
    "ventrilo" => 3784;
    "vnc"      => 5900;
    "wais"     => 210;
    "ws"       => 80;
    "wss"      => 443;
    otherwise => #f;
  end
end function;


define function %charset (#rest contents) => (cs :: <table>)
  let charset = make(<table>);
  for (item in contents)
    select (item by instance?)
      <string> =>
        for (char in item)
          charset[char] := as(<integer>, char);
        end;
      <table> =>
        for (v keyed-by char in item)
          charset[char] := v
        end;
    end select;
  end for;
  charset
end function;

// Collected ABNF for URI: https://datatracker.ietf.org/doc/html/rfc3986#appendix-A

define constant $charset-alphanumeric :: <table>
  = %charset("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
define constant $charset-sub-delims :: <table> = %charset("\'!$&()*+,;=");
define constant $charset-unreserved :: <table> = %charset($charset-alphanumeric, "-._~");
define constant $charset-segment    :: <table> = %charset($charset-unreserved,
                                                          $charset-sub-delims, ":@");
define constant $charset-scheme     :: <table> = %charset($charset-alphanumeric, "+-.");
define constant $charset-user-info  :: <table> = %charset($charset-unreserved,
                                                          $charset-sub-delims, ":");
define constant $charset-host       :: <table> = %charset($charset-unreserved,
                                                          $charset-sub-delims, "%");
define constant $charset-path       :: <table> = %charset($charset-alphanumeric,
                                                          "-._~%/", $charset-sub-delims);
define constant $charset-query      :: <table> = %charset($charset-segment, "/?");
define constant $charset-fragment   :: <table> = $charset-query;

define function validate
    (uri :: <string>, part-name :: <string>, result :: <string>, charset :: <table>,
     #key empty-ok? = #f)
  if (~empty-ok? & empty?(result))
    uri-parse-error("invalid URI %s - %s must not be empty", uri, part-name);
  end;
  for (ch in result)
    if (~element(charset, ch, default: #f))
      uri-parse-error("invalid URI %s - %s may not contain character %c",
                      uri, part-name, ch);
    end;
  end;
end function;

// Not clear if all this ad-hoc parsing code is worth avoiding a dependency on regex, but
// here we are.  Regex: https://datatracker.ietf.org/doc/html/rfc3986#appendix-B

define function parse-scheme
    (uri :: <string>)
 => (scheme :: <string?>, next-index :: <integer>)
  let scheme-end = find-any(uri, curry(\==, ':'));
  if (scheme-end)
    let scheme = lowercase!(copy-sequence(uri, end: scheme-end));
    validate(uri, "scheme", scheme, $charset-scheme, empty-ok?: #f);
    if (~alphabetic?(scheme[0]))
      uri-parse-error("invalid URI %s - scheme must start with an alphabetic character",
                      uri);
    end;
    values(scheme, scheme-end + 1)
  else
    values(#f, 0)
  end
end function;

define function parse-authority
    (uri :: <string>, start :: <integer>)
 => (user-info :: <string?>, host :: <string?>, port :: <integer?>,
     next-index :: <integer>)
  // `start` is the character following "//".
  let stop = uri.size;
  let user :: <string?> = #f;
  let host :: <string?> =  #f;
  let port :: <integer?> = #f;
  let auth-end = find-any(uri, rcurry(member?, "/?#"), start: start) | stop;
  if (start == auth-end)
    values(#f, #f, #f, start)
  else
    let user-end = find-any(uri, curry(\==, '@'), start: start, end: auth-end);
    if (user-end & user-end < auth-end)
      user := copy-sequence(uri, start: start, end: user-end);
    end;
    let host-start = if (user-end) user-end + 1 else start end;
    let port-start = find-any(uri, curry(\==, ':'),
                              start: host-start, end: auth-end, from-end?: #t);
    // There's a lot more we could do to validate the host, but we leave it up to the
    // protocol level, e.g., when an HTTP client tries to convert it to an IP address.
    if (uri[host-start] == '[')
      let close = find-any(uri, curry(\==, ']'),
                           start: host-start, end: auth-end, from-end?: #t);
      if ((port-start | 0) < (close | 0))
        port-start := #f;
      end;
      if (~close | (port-start & uri[port-start - 1] ~== ']'))
        uri-parse-error("invalid URI - bad IPv6+ host:port in %s", uri);
      end;
    end;
    let host = lowercase!(copy-sequence(uri, start: host-start,
                                        end: port-start | auth-end));
    if (empty?(host))
      uri-parse-error("invalid URI host - if authority present host must not"
                        " be empty: %s", uri);
    end;
    if (port-start)
      let bpos :: <integer> = port-start + 1;
      if (bpos < auth-end)
        let port-string = copy-sequence(uri, start: bpos, end: auth-end);
        block ()
          port := string-to-integer(port-string);
        exception (<error>)
          uri-parse-error("invalid URI port at index %d in %s", bpos, uri);
        end;
      else
        uri-parse-error("invalid URI port at index %d in %s", port-start, uri);
      end if;
    end if;
    // TODO(cgay): validate(uri, "authority", ...)
    values(user, host, port, auth-end | stop)
  end if;
end function;

define function parse-path
    (uri :: <string>, start :: <integer>)
 => (path :: <string?>, next-index :: <integer>)
  let path-end
    = find-any(uri, method (c) c == '?' | c == '#' end, start: start)
        | uri.size;
  if (start < path-end)
    let path = copy-sequence(uri, start: start, end: path-end);
    validate(uri, "path", path, $charset-path);
    values(path, path-end)
  else
    values(#f, start)
  end if
end function;

define function parse-query
    (uri :: <string>, start :: <integer>)
 => (query :: <string?>, next-index :: <integer>)
  if (uri[start] == '?')
    let start = start + 1;
    let stop = uri.size;
    if (start < stop)
      let query-end = find-any(uri, curry(\==, '#'), start: start) | stop;
      values(copy-sequence(uri, start: start, end: query-end), query-end)
    else
      values(#f, start)
    end
  else
    values(#f, start)
  end
end function;

define function parse-fragment
    (uri :: <string>, start :: <integer>)
 => (fragment :: <string?>)
  let stop = uri.size;
  if (start < stop & uri[start] == '#')
    copy-sequence(uri, start: start + 1, end: stop)
  end
end function;

define function string-to-uri
    (string :: <string>) => (uri :: <uri>)
  let (scheme, user, host, port, path, query, fragment) = split-uri(string);
  make(<uri>,
       scheme: scheme,
       raw-user-info: user,
       raw-host: host,
       port: port,
       raw-path: path,
       raw-query: query,
       raw-fragment: fragment)
end function;

define function split-uri
    (string :: <string>)
 => (scheme :: <string?>, user :: <string?>, host :: <string?>, port :: <integer?>,
     path :: <string?>, query :: <string?>, fragment :: <string?>)
  let stop = string.size;
  let authority? = starts-with?(string, "//");
  let (scheme :: <string?>, scheme-end :: <integer>)
    = if (authority?)
        values(#f, 0)
      else
        parse-scheme(string)
      end;
  authority?
    := string-equal?(string, "//", start1: scheme-end, end1: min(stop, scheme-end + 2));
  let (user, host, port, auth-end)
    = if (authority?)
        parse-authority(string, scheme-end + 2)
      else
        values(#f, #f, #f, scheme-end)
      end;
  if (scheme & ~port)
    port := default-port-for-scheme(scheme);
  end;
  let (path, path-end) = parse-path(string, auth-end);
  // https://datatracker.ietf.org/doc/html/rfc3986#section-3.3
  if (authority? & path & path[0] ~== '/')
    uri-parse-error("invalid URI: %s - path must be empty or start with '/'"
                      " when authority present", string);
  end;
  if (authority? & path & starts-with?(path, "//"))
    uri-parse-error("invalid URI: %s - path may not start with '//'"
                    " when authority present", string);
  end;
  let (query, query-end)
    = if (path-end < stop)
        parse-query(string, path-end)
      else
        values(#f, path-end)
      end;
  let fragment = parse-fragment(string, query-end);
  values(scheme, user, host, port, path, query, fragment)
end function;

// "A URI-reference is either a URI or a relative reference.  If the URI-reference's
// prefix does not match the syntax of a scheme followed by its colon separator, then the
// URI-reference is a relative reference."
// https://datatracker.ietf.org/doc/html/rfc3986#section-4.1
define function uri-relative?
    (uri :: <uri>) => (relative? :: <boolean>)
  ~uri.%scheme
end function;

// Even though the RFC states that:
// "3.4.  Query
//   The query component contains non-hierarchical data that, along with
//   data in the path component (Section 3.3), serves to identify a
//   resource within the scope of the URI's scheme and naming authority
//   (if any)."
// https://datatracker.ietf.org/doc/html/rfc3986#section-3.4
// We split the query into key-values, where "&foo=&" is different from "&foo&".
// The former sets the qvalue to "" and the latter sets it to #t.
define method split-query (query :: <string>) => (values :: <string-table>)
  let keyvals = split(query, '&');
  let qvalues = make(<string-table>, size: keyvals.size);
  for (kv in keyvals)
    let (qname, qvalue)
      = apply(values, split(kv, '=', remove-if-empty?: #f, count: 2));
    qname := percent-decode(qname);
    qvalue := if (qvalue)
                percent-decode(qvalue) // "?k=&..."
              else
                #t              // "?k&..."
              end;
    let existing = element(qvalues, qname, default: $unfound);
    qvalues[qname] := if (existing == $unfound)
                        qvalue
                      elseif (instance?(existing, <list>))
                        pair(qvalue, existing)
                      else
                        list(qvalue, existing)
                      end;
  end for;
  qvalues
end method;

// Do we want an option to produce a non-encoded URI?
define function uri-to-string
    (uri :: <uri>, #key scheme? = #t, authority? = #t)
 => (string :: <string>)
  authority? := authority? & uri.has-authority?;
  let parts :: <stretchy-vector> = make(<stretchy-vector>);
  if (scheme? & uri.%scheme)
    add!(parts, uri.%scheme);
    add!(parts, ":")
  end;
  if (authority?)
    add!(parts, "//");
    add!(parts, uri-authority(uri, percent-encoded?: #t));
  end;
  let p = uri.uri-raw-path;
  if (p)
    if (authority? & ~starts-with?(p, "/"))
      add!(parts, "/");
    end;
    add!(parts, p);
  end;
  let q = uri.uri-raw-query;
  if (q)
    add!(parts, "?");
    add!(parts, q);
  end;
  if (uri.uri-raw-fragment)
    add!(parts, "#");
    add!(parts, uri.uri-raw-fragment);
  end;
  apply(concatenate, parts)
end function;

// --------------------------------------------------------------------------------
// Percent encoding / decoding

// TODO(cgay):
// https://cs.opensource.google/go/go/+/refs/tags/go1.26.4:src/net/url/gen_encoding_table.go;l=148
// is a pretty useful resource for percent encoding.

define constant $hex-digits :: <byte-string> = "0123456789ABCDEF";
define constant $zero-code    :: <integer> = as(<integer>, '0');
define constant $nine-code    :: <integer> = as(<integer>, '9');
define constant $upper-a-code :: <integer> = as(<integer>, 'A');
define constant $upper-f-code :: <integer> = as(<integer>, 'F');
define constant $lower-a-code :: <integer> = as(<integer>, 'a');

define inline function hex-value (char :: <character>) => (hex-value :: <integer>)
  // char is a hex digit [0-9a-fA-F]
  let code :: <integer> = as(<integer>, char);
  select (as(<integer>, char) by \<=)
    $nine-code    => code - $zero-code;
    $upper-f-code => code - $upper-a-code + 10;
    otherwise     => code - $lower-a-code + 10;
  end
end function;

// Return a string in which any character in `string` that isn't a member of `charset` is
// percent encoded.  Returns `string` if no percent encoding needed, otherwise makes a
// copy.
define method percent-encode
    (string :: <byte-string>, charset :: <table>) => (encoded :: <byte-string>)
  let indexes = #();
  let n = 0;
  for (c in string, i from 0)
    if (~element(charset, c, default: #f))
      n := n + 1;
      indexes := pair(i, indexes);
    end;
  end;
  if (n == 0)
    string
  else
    let encoded = make(<byte-string>, size: string.size + n * 2);
    let len = string.size;
    iterate loop (i-string = 0, i-encoded = 0, indexes = reverse!(indexes))
      if (i-string < len)
        let index = indexes.head;
        if (i-string == index)
          let code = as(<integer>, string[i-string]);
          encoded[i-encoded] := '%';
          encoded[i-encoded + 1] := $hex-digits[ash(code, -4)];
          encoded[i-encoded + 2] := $hex-digits[logand(code, #x0F)];
          loop(i-string + 1, i-encoded + 3, indexes.tail)
        else
          encoded[i-encoded] := string[i-string];
          loop(i-string + 1, i-encoded + 1, indexes)
        end
      end
    end iterate;
    encoded
  end if
end method;

define method percent-decode
    (string :: <byte-string>, #key strict? :: <boolean>) => (unencoded :: <byte-string>)
  if (~member?('%', string))
    string
  else
    with-output-to-string (stream)
      let prev1 = #f;           // two chars behind ch
      let prev2 = #f;           // one char behind ch
      for (ch in string,
           i from 0)
        if (prev1 == '%')
          // Are prev1, prev2, and ch % HEX HEX ?
          if (hexadecimal-digit?(prev2) & hexadecimal-digit?(ch))
            write-element(stream, as(<byte-character>,
                                     ash(hex-value(prev2), 4) + hex-value(ch)));
          elseif (strict?)
            uri-parse-error("invalid URI percent escape at character %= in %s",
                            i, string);
          else
            write-element(stream, prev1);
            write-element(stream, prev2);
            write-element(stream, ch);
          end;
          prev1 := #f; prev2 := #f;
        else
          prev1 & write-element(stream, prev1);
          prev1 := prev2;
          prev2 := ch;
        end;
      end for;
      if (strict? & (prev1 == '%' | prev2 == '%'))
        uri-parse-error("invalid URI percent escape at end of %=", string);
      else
        prev1 & write-element(stream, prev1);
        prev2 & write-element(stream, prev2);
      end;
    end with-output-to-string
  end if
end method;

// --------------------------------------------------------------------------------
// Transform References (merge URIs)

// 5.2.2.  Transform References
// "Merges" a base and reference URI
// A leading '/' in the path indicates that it's absolute.
// Logic taken directly from the pseudeocode
define method merge-uris
    (base :: <uri>, reference :: <uri>, #key strict? :: <boolean> = #t)
 => (target :: <uri>)
  // TODO(cgay): (optional) normalize (6.2.2) base first

  // https://datatracker.ietf.org/doc/html/rfc3986#section-5.2.1
  base.%scheme
    | uri-error("cannot merge against base URI without a scheme: %s",
                uri-to-string(base));
  let ref :: <uri> = reference; // brevity
  let (scheme, user, host, port, path, query) = #f;
  let ref-scheme = ref.uri-scheme;
  if (~strict? & ref.uri-scheme = base.uri-scheme)
    ref-scheme := #f            // effectively undefine scheme(R)
  end;
  // This is a direct translation of
  // https://datatracker.ietf.org/doc/html/rfc3986#section-5.2.2
  // Note the use of the public APIs here to ensure that a value is available if
  // at least one of the raw/non-raw slots is non-false.
  if (ref-scheme)
    // If the reference URI has a scheme then it is fully-qualified and is used as-is,
    // except for dropping the fragment and removing dot segments from the path.
    // https://datatracker.ietf.org/doc/html/rfc3986#section-4.3
    scheme := ref.uri-scheme;
    user := ref.uri-raw-user-info;
    host := ref.uri-raw-host;
    port := ref.uri-port;
    path := ref.uri-raw-path;
    query := ref.uri-raw-query;
  else
    if (ref.has-authority?)
      user := ref.uri-raw-user-info;
      host := ref.uri-raw-host;
      port := ref.uri-port;
      path := ref.uri-raw-path;
      query := ref.uri-raw-query;
    else
      if (~ref.uri-raw-path)
        path := base.uri-raw-path;
        query := ref.uri-raw-query | base.uri-raw-query;
      else
        path := remove-dot-segments(if (starts-with?(ref.uri-raw-path, "/"))
                                      ref.uri-raw-path
                                    else
                                      merge-raw-paths(base, ref)
                                    end);
        query := ref.uri-raw-query;
      end;
      user := base.uri-raw-user-info;
      host := base.uri-raw-host;
      port := base.uri-port;
    end;
    scheme := base.uri-scheme;
  end;
  make(<uri>,
       scheme: scheme,
       raw-user-info: user,
       raw-host: host,
       port: port,
       raw-path: path,
       raw-query: query,
       raw-fragment: ref.uri-raw-fragment) // always from ref
end method merge-uris;

define function merge-raw-paths
    (base :: <uri>, ref :: <uri>) => (path :: <byte-string>)
  // https://datatracker.ietf.org/doc/html/rfc3986#section-5.2.3
  if (base.has-authority? & ~base.%raw-path)
    // Use ref path but make it absolute.  (Seems this can result in "//" at the
    // beginning of the path, but it is what the RFC says to do so....)
    concatenate("/", ref.%raw-path | "")
  else
    let slash = base.%path & find-any(base.%raw-path, curry(\==, '/'), from-end?: #t);
    if (~slash)
      ref.%raw-path
    else
      let prefix = copy-sequence(base.%raw-path, end: slash + 1);
      concatenate(prefix, ref.%raw-path | "")
    end
  end
end function;

// https://datatracker.ietf.org/doc/html/rfc3986#section-5.2.4
define method remove-dot-segments
    (path :: <string>) => (new-path :: <string>)
  if (~member?('.', path))
    path
  else
    join(remove-dot-segments(split(path, '/')), "/")
  end
end method;

define method remove-dot-segments
    (segments :: <list>) => (new-path :: <list>)
  if (~member?("..", segments, test: \=) & ~member?(".", segments, test: \=))
    segments
  else
    as(<list>, remove-dot-segments(as(<deque>, segments)))
  end
end method;

define method remove-dot-segments
    (segments :: <deque>) => (new-path :: <deque>)
  let result = make(<deque>);
  let len = size(segments);
  for (seg in segments, i from 0)
    let last? = (i = len - 1);
    select (seg by \=)
      "." =>
        if (last?)
          push-last(result, "");  // /a/b/. => /a/b/
        end;
      ".." =>
        if (last?)
          last(result) := "";   // /a/b/.. => /a/
        elseif (result.size > 0 & ~empty?(last(result)))
          pop-last(result);
        end if;
      otherwise =>
        push-last(result, seg);
    end select;
  end for;
  result
end method;

/*

TODO(cgay): verify that these examples are in the tests and delete this comment.

// example / usage / testing

begin
  let bar = "/foo?users=admin&users=1&users=2&members=3&members=4&comment=&add=Add";
  let foo = parse-url(bar);
  format-out("%=, %=\n", foo.%query["users"], foo.%query["members"]);
  format-out("%s\n%s\n", bar, foo);

  let foo = parse-url("http://baz.blub/pat%2fh/test?fo%20o=ba%2f%20r");
  format-out("%s, %=,%s\n", foo.%query, foo.%path, foo);

  format-out("%s\n", split-query("foo=bar+blub&baz"));

  let uri = parse-uri("http://foo:bar@baz.blub:23/path/test/../page?fo%20=ba+r&q1=q2&q3=&q4#extra");
  let url = parse-url("http://foo:bar@baz.blub:23/path/test/../page?fo%20o=b+r&q1=q2&q3=&q4#extra");
  format-out("%=\n", uri.%query);
  format-out("%=\n", url.%query);

  format-out("%=\n", percent-decode("foo%20bar"));
  format-out("%=\n", percent-decode("%2"));
  format-out("%=\n", percent-decode("%"));
  format-out("%=\n", percent-decode("%rg"));

  let uri = parse-uri("http://foo:bar@baz.blub:23/path/test/../page?foo=bar&q1=q2#extra");
  format-out("%s\n", build-uri(uri));
  uri := make(<uri>, scheme: "http", user: "foo@bar:blub");
  format-out("%s\n", build-uri(uri));
  uri := make(<uri>, scheme: "http", host: "foobar", path: "/p1/p2/p3", query: "k1=v1&k2=v2");
  last(uri.%path) := "foo/bar+baz";
  format-out("%s\n", build-uri(uri));
  let url = make(<url>, scheme: "http", host: "foobar", path: "/p1/p2/p3", query: "k1=v1&k2=v2");
  last(url.%path) := "foo/bar+baz";
  format-out("%s\n", build-uri(url));

  let uri1 = parse-uri("http://foo.bar/test");
  format-out("uri1: %=\n", uri);
  format-out("uri1 (built): %=\n", build-uri(uri1));
  let uri2 = make(<uri>, path: "../foo/../../bar");
  format-out("uri2: %=\n", uri2);
  format-out("uri2 (built): %=\n", build-uri(uri2));
  let uri3 = transform-uris(uri1, uri2);
  format-out("uri3: %=\n", uri3);
  format-out("uri3 (built): %=\n", build-uri(uri3));

  format-out("%s\n", build-uri(transform-uris(parse-uri("http://foo.bar/test"), make(<uri>, path: "../foo/../../bar"))));
  format-out("%s\n", build-uri(transform-uris(parse-uri("http://foo.bar/test"), make(<uri>, path: "/foo/bar"))));

  format-out("as: %s\n", as(<string>, parse-uri("http://foo?a=1&b=2#anchor")));
end;
*/

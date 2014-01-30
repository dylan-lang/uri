module: uri
author: Bastian Mueller
synopsis: RFC 3986: Uniform Resource Identifier (URI): Generic Syntax
Copyright:    Original Code is Copyright (c) 2011 Dylan Hackers
              All rights reserved.
License:      See License.txt in this distribution for details.

define class <uri> (<object>)
  slot uri-scheme :: <string> = "",
    init-keyword: scheme:;
  slot uri-userinfo :: <string> = "",
    init-keyword: userinfo:;
  slot uri-host :: <string> = "",
    init-keyword: host:;
  slot uri-port :: false-or(<integer>) = #f,
    init-keyword: port:;
  // Do you really want this to be a stretchy type?
  slot uri-path :: <sequence> = make(<deque>),
    init-keyword: path:;
  // keys without values are #t
  slot uri-query :: <string-table> = make(<string-table>),
    init-keyword: query:;
  slot uri-fragment :: <string> = "",
    init-keyword: fragment:;
end class <uri>;

// FIXME -- Implement the following restrictions in the initialize method
//          for the <uri> class...
//   The scheme and path components are required, though the path may be
//   empty (no characters).  When authority is present, the path must
//   either be empty or begin with a slash ("/") character.  When
//   authority is not present, the path cannot begin with two slash
//   characters ("//").  When authority is present it must have a host
//   component.
//   Need an error class for these.  e.g., <invalid-uri-error>

define class <url> (<uri>) end;

define method has-authority-part?
    (uri :: <uri>) => (has-it? :: <boolean>)
  ~empty?(uri.uri-userinfo) | ~empty?(uri.uri-host) | (uri.uri-port ~= #f)
end method has-authority-part?;


// RFC 3986, Section 3.2
//       authority   = [ userinfo "@" ] host [ ":" port ]
define method uri-authority
    (uri :: <uri>)
 => (result :: <string>)
  let result = "";
  unless (empty?(uri.uri-userinfo))
    result := concatenate(result, percent-encode($uri-userinfo, uri.uri-userinfo), "@");
  end;
  result := concatenate(result, uri.uri-host | "");
  if (uri.uri-port)
    result := concatenate(result, ":", integer-to-string(uri.uri-port));
  end if;
  result;
end method uri-authority;

define constant $alpha :: <byte-string>
  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz";

define constant $digit :: <byte-string> = "0123456789";

//define constant $uri-scheme :: <byte-string> = concatenate($alpha, $digit, "+-.");
//define constant $uri-gen-delims :: <byte-string> = ":/?#[]@"";
define constant $uri-sub-delims :: <byte-string> = "\'!$&()*+,;=";
//define constant $uri-reserved :: <byte-string> = concatenate($uri-gen-delims, $uri-sub-delims);
define constant $uri-unreserved :: <byte-string> = concatenate($alpha, $digit, "-._~");
define constant $uri-pchar :: <byte-string> = concatenate($uri-unreserved, $uri-sub-delims, ":@");
define constant $uri-userinfo :: <byte-string> = concatenate($uri-unreserved, $uri-sub-delims, ":");
define constant $uri-query :: <byte-string> = concatenate($uri-pchar, "/?");
define constant $uri-query-no-plus :: <byte-string> = remove($uri-query, '+');
//define constant $uri-port = $digit;
define constant $uri-segment = $uri-pchar;
//define constant $uri-fragment = $uri-query;

define constant $plus :: <regex> = compile-regex("[+]");

define inline function parse-scheme
    (uri :: <string>)
 => (scheme :: false-or(<string>), next-index :: <integer>)
  let scheme-end = find-delimiter(uri, ':');
  if (scheme-end)
    values(copy-sequence(uri, start: 0, end: scheme-end), scheme-end + 1)
  else
    values(#f, 0)
  end
end function parse-scheme;

define inline function parse-authority
    (uri :: <string>, start :: <integer>)
 => (userinfo :: false-or(<string>),
     host :: <string>,
     port :: false-or(<integer>),
     next-index :: <integer>)
  let stop = uri.size;
  let userinfo :: false-or(<string>) = #f;
  let host :: <string> =  "";
  let port :: false-or(<integer>) = #f;

  let userinfo-end = find-delimiter(uri, '@', start: start);
  let authority-end = find-delimiter(uri, '/', start: start);
  if (authority-end & userinfo-end & userinfo-end < authority-end)
    // parse userinfo
    userinfo := copy-sequence(uri, start: start, end: userinfo-end);
  end if;
  let host-start = userinfo-end | start;
  let port-start = find-delimiter(uri, ':', start: host-start);
  // parse port
  if (port-start)
    port := string-to-integer(copy-sequence(uri, start: port-start, end: authority-end));
  end if;
  // parse host
  host := copy-sequence(uri, start: host-start, end: port-start | authority-end | stop);

  values(userinfo, host, port, authority-end | stop)
end function parse-authority;

define inline function parse-path
    (uri :: <string>, start :: <integer>)
 => (path :: false-or(<string>), next-index :: <integer>)
  let stop = uri.size;
  let path-end = find-delimiters(uri, "?#", start: start) | stop;
  if (start < path-end)
    values(copy-sequence(uri, start: start, end: path-end), path-end)
  else
    values(#f, start)
  end if
end function parse-path;

define inline function parse-query
    (uri :: <string>, start :: <integer>)
 => (query :: false-or(<string>), next-index :: <integer>)
  let stop = uri.size;
  if (start < stop)
    if (uri[start] == '?')
      let query-end = find-delimiter(uri, '#', start: start + 1) | stop;
      values(copy-sequence(uri, start: start + 1, end: query-end), query-end)
    else
      values(#f, start)
    end if
  else
    values(#f, start)
  end if
end function parse-query;

define inline function parse-fragment
    (uri :: <string>, start :: <integer>)
 => (fragment :: false-or(<string>))
  let stop = uri.size;
  if (start < stop)
    // probably not necessary
    if (uri[start] == '#')
      copy-sequence(uri, start: start + 1, end: stop)
    else
      #f
    end if
  else
    #f
  end if
end function parse-fragment;

define function %parse-uri
    (uri :: <string>)
 => (scheme :: false-or(<string>),
     userinfo :: false-or(<string>),
     host :: false-or(<string>),
     port :: false-or(<integer>),
     path :: false-or(<string>),
     query :: false-or(<string>),
     fragment :: false-or(<string>))
  let stop = uri.size;

  let (scheme :: false-or(<string>), scheme-end :: <integer>)
    = if (starts-with?(uri, "//"))
        values(#f, 0)
      else
        parse-scheme(uri)
      end if;

  if (starts-with?(copy-sequence(uri, start: scheme-end), "//"))
    // parse authority path-abempty
    let (userinfo, host, port, authority-end)
      = parse-authority(uri, scheme-end + 2);
    let (path, path-end) = parse-path(uri, authority-end);
    let (query, query-end) = parse-query(uri, path-end);
    let (fragment, fragment-end) = parse-fragment(uri, query-end);
    values(scheme, userinfo, host, port, path, query, fragment)
  else
    // parse path-absolute, path-noscheme or path-empty
    let (path, path-end) = parse-path(uri, scheme-end);
    let (query, query-end) = parse-query(uri, path-end);
    let fragment = parse-fragment(uri, query-end);
    values(scheme, #f, #f, #f, path, query, fragment)
  end if
end function %parse-uri;

define method parse-uri-as
    (class :: subclass(<uri>), uri :: <string>)
 => (result :: <uri>)
  let (scheme, userinfo, host, port, path, query, fragment)
    = if (empty?(uri))
        values(#f, #f, #f, #f, #f, #f, #f)
      else
        %parse-uri(uri)
      end if;

  if (class == <url> & query)
    query := regex-replace(query, $plus, " ");
  end if;
  if (scheme) scheme := percent-decode(scheme); end;
  if (userinfo) userinfo := percent-decode(userinfo); end;
  if (host) host := percent-decode(host); end;
  if (fragment) fragment := percent-decode(fragment); end;
  let uri = make(class,
                 scheme: scheme | "",
                 userinfo: userinfo | "",
                 host: host | "",
                 port: port,
                 fragment: fragment | "");
  if (path & ~empty?(path))
    uri.uri-path := split-path(path);
  end if;
  if (query)
    uri.uri-query := split-query(query);
  end if;
  if (absolute?(uri))
    uri.uri-path := remove-dot-segments(uri.uri-path);
  end if;
  uri;
end method parse-uri-as;

define constant parse-uri = curry(parse-uri-as, <uri>);

define constant parse-url = curry(parse-uri-as, <url>);

// relative / absolute

define function relative?
    (uri :: <uri>)
 => (result :: <boolean>);
  empty?(uri.uri-scheme)
end;

define constant absolute? = complement(relative?);

// split parts

define method split-path
    (path :: <string>)
 => (parts :: <sequence>)
  map(percent-decode, split(path, '/'))
end;

// Even though the RFC states that:
// "3.4.  Query
//   The query component contains non-hierarchical data that, along with
//   data in the path component (Section 3.3), serves to identify a
//   resource within the scope of the URI's scheme and naming authority
//   (if any)."
// We split the query into key-values, where "&foo=&" is different from &foo&.
// The former sets the qvalue to "" and the latter sets it to #t.
define method split-query
    (query :: <string>)
 => (parts :: <string-table>);
  let parts = split(query, "&");
  let table = make(<string-table>, size: parts.size);
  for (part in parts)
    let (qname, qvalue) = apply(values, split(part, "=",
                                              remove-if-empty?: #f,
                                              count: 2));
    qname := percent-decode(qname);
    if (qvalue)
      qvalue := percent-decode(qvalue);
    else
      qvalue := #t;
    end if;
    if (element(table, qname, default: #f))
      table[qname] := if (instance?(table[qname], <string>))
                        list(table[qname], qvalue);
                      else
                        pair(qvalue, table[qname]);
                      end if;
    else
      table[qname] := qvalue;
    end if;
  end for;
  table;
end method split-query;

define method as
    (class == <string>, uri :: <uri>)
 => (result :: <string>)
  build-uri(uri)
end method as;

// build-uri

// Turn a uri into a string
define open generic build-uri
    (uri :: <uri>, #key include-scheme, include-authority)
 => (result :: <string>);

define method build-uri
    (uri :: <uri>,
     #key include-scheme = #t, include-authority = #t)
 => (result :: <string>)
  let parts :: <stretchy-vector> = make(<stretchy-vector>);
  if (include-scheme & ~empty?(uri.uri-scheme))
    add!(parts, uri.uri-scheme);
    add!(parts, ":")
  end;
  if (include-authority & has-authority-part?(uri))
    add!(parts, "//");
    add!(parts, uri.uri-authority);
  end;
  add!(parts, build-path(uri));
  unless (empty?(uri.uri-query))
    add!(parts, "?");
    add!(parts, build-query(uri));
  end;
  unless (empty?(uri.uri-fragment))
    add!(parts, "#");
    add!(parts, uri.uri-fragment);
  end;
  join(parts, "")
end method build-uri;

// build-path

define method build-path
    (uri :: <uri>)
 => (encoded-path :: <string>)
  if (empty?(uri.uri-path))
    ""
  else
    join(map(curry(percent-encode, $uri-segment),
             uri.uri-path),
         "/")
  end if;
end;

// build-query

define method build-query
    (uri :: <uri>) => (encoded-query :: <string>)
  build-query-internal(uri, $uri-query)
end;

define method build-query
    (url :: <url>) => (encoded-query :: <string>)
  build-query-internal(url, $uri-query-no-plus)
end;

define method build-query-internal
    (uri :: <uri>, chars-not-to-encode :: <sequence>)
 => (encoded-query :: <string>)
  if (empty?(uri.uri-query))
    ""
  else
    let parts = make(<stretchy-vector>);
    for (value keyed-by key in uri.uri-query)
      key := percent-encode(chars-not-to-encode, key);
      add-key-value(parts, key, value);
    end for;
    join(parts, "&")
  end if;
end method build-query-internal;

define method add-key-value
    (parts :: <stretchy-vector>, key :: <string>, value :: <string>)
 => (parts :: <stretchy-vector>);
  add!(parts, concatenate(key, "=", percent-encode($uri-query, value)));
end;

define method add-key-value
    (parts :: <stretchy-vector>, key :: <string>, value == #t)
 => (parts :: <stretchy-vector>)
  add!(parts, key);
end;

define method add-key-value
    (parts :: <stretchy-vector>, key :: <string>, values :: <list>)
 => (parts :: <stretchy-vector>);
  for (value in values)
    add-key-value(parts, key, value);
  end for;
  parts;
end;


// percent-encode

define method percent-encode
    (chars :: <sequence>, unencoded :: <byte-string>)
 => (encoded :: <string>)
  let encoded = "";
  for (char in unencoded)
    encoded := concatenate(encoded, if (member?(char, chars))
                                      list(char)
                                    else
                                      format-to-string("%%%X", as(<byte>, char))
                                    end if);
  end for;
  encoded;
end method percent-encode;

// percent-decode

define method percent-decode
    (encoded :: <byte-string>)
 => (unencoded :: <string>);
  let result = make(limited(<stretchy-vector>, of: <byte-character>));
  let (decode?, ignore?) = values(#f, #f);
  for (char in encoded, position from 0)
    if (ignore?)
      ignore? := #f;
    else
      if (char = '%' & ~decode?)
        decode? := #t;
      else
        if (decode? & size(encoded) > position + 1)
          let encoded-char = make(<byte-string>, size: 2);
          encoded-char[0] := char;
          encoded-char[1] := encoded[position + 1];
          let decoded-char = as(<byte-character>,
                                string-to-integer(encoded-char, base: 16));
          ignore? := #t;
          decode? := #f;
          add!(result, decoded-char);
        else
          add!(result, char);
        end if;
      end if;
    end if;
  end for;
  as(<string>, result)
end method percent-decode;

// remove-dot-segments

define generic remove-dot-segments (path :: <object>) => (result :: <object>);

define method remove-dot-segments (path :: <string>) => (result :: <string>);
  let path = split(path, "/", remove-if-empty?: #f);
  path := remove-dot-segments(path);
  join(path, "/")
end;

define method remove-dot-segments (path :: <sequence>) => (result :: <sequence>);
  let input = make(<deque>);
  do(curry(push-last, input), path);
  let output = make(<deque>);
  for (segment in input, i from 0)
    let last? = (i = size(input) - 1);
    if ((segment = "." | segment = "") & last?)
      push-last(output, "");
    elseif (segment = ".." & last?)
      last(output) := "";
    elseif (segment = "..")
      if (size(output) > 0 & last(output) ~= "")
        pop-last(output);
      end if;
    elseif (segment = ".")
    else
      push-last(output, segment);
    end if;
  end for;
  output;
end;

// 5.2.2.  Transform References
// "Merges" a base and reference URI
// A leading empty component in the path indicates that it's absolute.
// Logic taken directly from the pseudeocode
define method transform-uris
    (base :: <uri>, reference :: <uri>,
     #key as :: subclass(<uri>) = <uri>)
 => (target :: <uri>)
  local method merge (base, reference)
      if (has-authority-part?(base) & empty?(base.uri-path))
        concatenate(#(""), reference.uri-path);
      else
        concatenate(copy-sequence(base.uri-path, end: base.uri-path.size - 1),
                    reference.uri-path)
      end if;
    end;
  let target = make(as);
  if (~empty?(reference.uri-scheme))
    // If the reference uri has a scheme then it is fully-qualified and
    // is used in its entirety, except for the fragment.  (If <uri> were
    // immutable we could just return the original.)
    target.uri-scheme := reference.uri-scheme;
    target.uri-userinfo := reference.uri-userinfo;
    target.uri-host := reference.uri-host;
    target.uri-port := reference.uri-port;
    target.uri-path := remove-dot-segments(reference.uri-path);
    target.uri-query := reference.uri-query;
  else
    if (has-authority-part?(reference))
      target.uri-userinfo := reference.uri-userinfo;
      target.uri-host := reference.uri-host;
      target.uri-port := reference.uri-port;
      target.uri-path := remove-dot-segments(reference.uri-path);
      target.uri-query := reference.uri-query;
    else
      // reference's scheme and authority were both empty...
      if (empty?(reference.uri-path))
        target.uri-path := base.uri-path;
        target.uri-query := if (empty?(reference.uri-query))
                              base.uri-query
                            else
                              reference.uri-query
                            end;
      else
        target.uri-path := if (first(reference.uri-path) = "")
                             remove-dot-segments(reference.uri-path)
                           else
                             remove-dot-segments(merge(base, reference))
                           end;
        target.uri-query := reference.uri-query;
      end if;
      target.uri-userinfo := base.uri-userinfo;
      target.uri-host := base.uri-host;
      target.uri-port := base.uri-port;
    end if;
    target.uri-scheme := base.uri-scheme;
  end if;
  target.uri-fragment := reference.uri-fragment;
  target;
end method transform-uris;

define method print-message (uri :: <uri>, stream :: <stream>) => ();
  format(stream, "%s", build-uri(uri))
end;

/*

// example / usage / testing

begin
  let bar = "/foo?users=admin&users=1&users=2&members=3&members=4&comment=&add=Add";
  let foo = parse-url(bar);
  format-out("%=, %=\n", foo.uri-query["users"], foo.uri-query["members"]);
  format-out("%s\n%s\n", bar, foo);

  let foo = parse-url("http://baz.blub/pat%2fh/test?fo%20o=ba%2f%20r");
  format-out("%s, %=,%s\n", foo.uri-query, foo.uri-path, foo);

  format-out("%s\n", split-query("foo=bar+blub&baz"));

  let uri = parse-uri("http://foo:bar@baz.blub:23/path/test/../page?fo%20=ba+r&q1=q2&q3=&q4#extra");
  let url = parse-url("http://foo:bar@baz.blub:23/path/test/../page?fo%20o=b+r&q1=q2&q3=&q4#extra");
  format-out("%=\n", uri.uri-query);
  format-out("%=\n", url.uri-query);

  format-out("%=\n", percent-decode("foo%20bar"));
  format-out("%=\n", percent-decode("%2"));
  format-out("%=\n", percent-decode("%"));
  format-out("%=\n", percent-decode("%rg"));

  let uri = parse-uri("http://foo:bar@baz.blub:23/path/test/../page?foo=bar&q1=q2#extra");
  format-out("%s\n", build-uri(uri));
  uri := make(<uri>, scheme: "http", userinfo: "foo@bar:blub");
  format-out("%s\n", build-uri(uri));
  uri := make(<uri>, scheme: "http", host: "foobar", path: "/p1/p2/p3", query: "k1=v1&k2=v2");
  last(uri.uri-path) := "foo/bar+baz";
  format-out("%s\n", build-uri(uri));
  let url = make(<url>, scheme: "http", host: "foobar", path: "/p1/p2/p3", query: "k1=v1&k2=v2");
  last(url.uri-path) := "foo/bar+baz";
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


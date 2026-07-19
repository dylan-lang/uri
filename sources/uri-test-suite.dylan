module: uri-test-suite

// TODO(cgay): copy the tests from some language that has a large user base and therefore
// the URI code has been battle tested for edge cases.

define constant $base-uri = string-to-uri("http://a/b/c/d;p?q");

define test test-string-to-uri-basic ()
  assert-equal("http",     $base-uri.uri-scheme);
  assert-equal("a",        $base-uri.uri-authority);
  assert-equal("/b/c/d;p", $base-uri.uri-raw-path);
  assert-equal(#("", "b", "c", "d;p"), $base-uri.uri-path);
  assert-equal("q",        $base-uri.uri-raw-query);
  assert-false($base-uri.uri-fragment);
end test;

define test test-percent-encode ()
end test;

define test test-percent-decode ()
  let uri = string-to-uri("http://a/p?x=a%20test&y=it%27s");
  assert-equal("x=a%20test&y=it%27s", uri.uri-raw-query);
  assert-equal("a test", uri.uri-query["x"]);
  assert-equal("it's", uri.uri-query["y"]);

  assert-signals(<uri-error>, percent-decode("%", strict?: #t));
  assert-signals(<uri-error>, percent-decode("%a", strict?: #t));
  assert-signals(<uri-error>, percent-decode("%ag", strict?: #t));
  assert-equal("%", percent-decode("%"));
  assert-equal("%a", percent-decode("%a"));
  assert-equal("%ag", percent-decode("%ag"));
  assert-equal("%ga", percent-decode("%ga"));
  assert-equal("x@x", percent-decode("x%40x"));
  assert-equal("x/x", percent-decode("x%2fx"));
  assert-equal("x/x", percent-decode("x%2Fx"));
end test;

define function check-uri-reference
    (uri-string :: <string>,
     #key scheme, user, host, port, path, query, fragment, merged :: <string>)
  let uri = string-to-uri(uri-string);
  expect-equal(scheme,   uri.uri-scheme);
  expect-equal(user,     uri.uri-raw-user-info);
  expect-equal(host,     uri.uri-raw-host);
  expect-equal(port,     uri.uri-port);
  expect-equal(path,     uri.uri-raw-path);
  expect-equal(query,    uri.uri-raw-query);
  expect-equal(fragment, uri.uri-raw-fragment);
  let target-uri = merge-uris($base-uri, uri);
  expect-equal(merged, uri-to-string(target-uri),
               "%s merged against base URI %s = %s ?",
               uri-string, uri-to-string($base-uri), merged);
end function;

// Normal Examples from
// https://datatracker.ietf.org/doc/html/rfc3986#section-5.4.1
define test test-normal-uri-reference-examples ()
  check-uri-reference("g:h", scheme: "g", path: "h", merged: "g:h");
  check-uri-reference("g",       path: "g",   merged: "http://a/b/c/g");
  check-uri-reference("./g",     path: "./g", merged: "http://a/b/c/g");
  check-uri-reference("g/",      path: "g/",  merged: "http://a/b/c/g/");
  check-uri-reference("/g",      path: "/g",  merged: "http://a/g");
  check-uri-reference("//g",     host: "g",   merged: "http://g");
  check-uri-reference("?y",      query: "y",  merged: "http://a/b/c/d;p?y");
  check-uri-reference("g?y",     path: "g", query: "y", merged: "http://a/b/c/g?y");
  check-uri-reference("#s",      fragment: "s", merged: "http://a/b/c/d;p?q#s");
  check-uri-reference("g#s",     path: "g", fragment: "s", merged: "http://a/b/c/g#s");
  check-uri-reference("g?y#s",   path: "g", query: "y", fragment: "s",   merged: "http://a/b/c/g?y#s");
  check-uri-reference(";x",      path: ";x", merged: "http://a/b/c/;x");
  check-uri-reference("g;x",     path: "g;x", merged: "http://a/b/c/g;x");
  check-uri-reference("g;x?y#s", path: "g;x", query: "y", fragment: "s", merged: "http://a/b/c/g;x?y#s");
  check-uri-reference(".",       path: ".",       merged: "http://a/b/c/");
  check-uri-reference("./",      path: "./",      merged: "http://a/b/c/");
  check-uri-reference("..",      path: "..",      merged: "http://a/b/");
  check-uri-reference("../",     path: "../",     merged: "http://a/b/");
  check-uri-reference("../g",    path: "../g",    merged: "http://a/b/g");
  check-uri-reference("../..",   path: "../..",   merged: "http://a/");
  check-uri-reference("../../",  path: "../../",  merged: "http://a/");
  check-uri-reference("../../g", path: "../../g", merged: "http://a/g");
  // Note default port not included in merged result string, per RFC.
  check-uri-reference("http://host:80", scheme: "http", host: "host", port: 80, merged: "http://host");
  check-uri-reference("http://host:80/abc", scheme: "http", host: "host", port: 80, path: "/abc", merged: "http://host/abc");
  check-uri-reference("http://host:77/abc", scheme: "http", host: "host", port: 77, path: "/abc", merged: "http://host:77/abc");
end test;

// Abnormal Examples from
// https://datatracker.ietf.org/doc/html/rfc3986#section-5.4.2
define test test-abnormal-uri-reference-examples ()
  check-uri-reference("../../../g",    path: "../../../g",    merged: "http://a/g");
  check-uri-reference("../../../../g", path: "../../../../g", merged: "http://a/g");
  check-uri-reference("/./g",          path: "/./g",          merged: "http://a/g");
  check-uri-reference("/../g",         path: "/../g",         merged: "http://a/g");
  check-uri-reference("g.",            path: "g.",            merged: "http://a/b/c/g.");
  check-uri-reference(".g",            path: ".g",            merged: "http://a/b/c/.g");
  check-uri-reference("g..",           path: "g..",           merged: "http://a/b/c/g..");
  check-uri-reference("..g",           path: "..g",           merged: "http://a/b/c/..g");
  check-uri-reference("./../g",        path: "./../g",        merged: "http://a/b/g");
  check-uri-reference("./g/.",         path: "./g/.",         merged: "http://a/b/c/g/");
  check-uri-reference("g/./h",         path: "g/./h",         merged: "http://a/b/c/g/h");
  check-uri-reference("g/../h",        path: "g/../h",        merged: "http://a/b/c/h");
  check-uri-reference("g;x=1/./y",     path: "g;x=1/./y",     merged: "http://a/b/c/g;x=1/y");
  check-uri-reference("g;x=1/../y",    path: "g;x=1/../y",    merged: "http://a/b/c/y");
end test;

define test test-path-segment-normalization ()
  let base = string-to-uri("ftp:/x/y?q");
  local
    method check-normalize-path (want, path)
      let u = string-to-uri(path);
      let m = merge-uris(base, u);
      let p = join(m.uri-path, "/");
      assert-equal(want, p, "normalize path %=", path);
    end;
  // All absolute paths so just resolving .. segments.
  check-normalize-path("/a/c",   "/a/b/../c");
  check-normalize-path("/a/b/c", "/a/b/./c");
  check-normalize-path("/a/c",   "/a/./b/../c");
  check-normalize-path("/b/c",   "/a/../b/./c");
  check-normalize-path("/a/c",   "/a/b/./../c");
  check-normalize-path("/a/c",   "/a/b/.././c");
  check-normalize-path("/b/c",   "/a/../../../b/c");

  // Relative paths so actual path merging happens.
  check-normalize-path("/x/a/c",    "a/b/../c");
  check-normalize-path("/x/a/b/c",  "a/b/./c");
  check-normalize-path("/x/a/c",    "a/./b/../c");
  check-normalize-path("/x/b/c",    "a/../b/./c");
  check-normalize-path("/x/a/c",    "a/b/./../c");
  check-normalize-path("/x/a/c",    "a/b/.././c");
  check-normalize-path("/b/c",      "a/../../../b/c");
end test;

define test parse-error-test ()
  for (uri in #["http://host:xx/abc"])
    assert-signals(<uri-parse-error>, string-to-uri(uri), uri);
  end;
end test;

define test test-uri-query-value ()
  let u = make(<uri>, raw-query: "a&b=2&b=3&c=4&c&d=5&e=%26");
  assert-equal(#t, uri-query-value(u, "a"));
  // Technically we don't guarantee an order, so this could change.
  assert-equal("3",         uri-query-value(u, "b"));
  assert-equal(#("3", "2"), uri-query-value(u, "b", all?: #t));
  assert-equal(#(#t, "4"),  uri-query-value(u, "c", all?: #t));
  assert-equal("5",         uri-query-value(u, "d"));
  assert-equal("&",         uri-query-value(u, "e"));
end test;

define test test-scheme ()
  assert-signals(<uri-parse-error>, string-to-uri("://foo.com"),
                 "URI may not start with ':'");

  // Check some invalid chars.
  for (char in "%/&,")
    let string = format-to-string("a%cb://c.d", char);
    assert-signals(<uri-parse-error>, string-to-uri(string),
                   "scheme may not contain %=", char);
  end;

  // Check the valid non-ALPHA chars.
  for (char in "+-.")
    let string = format-to-string("a%cb://c.d", char);
    assert-no-errors(string-to-uri(string),
                     "scheme may not contain %=", char);
  end;

  // Leading char must be ALPHA.
  for (char in "+-7")
    let string = format-to-string("%cabc://c.d", char);
    assert-signals(<uri-parse-error>, string-to-uri(string),
                   "scheme must start with A-Z");
  end;

  // Scheme isn't required to be lowercased but it must be compared without regard to
  // case and should be lowercase in constructed URI strings, so we store it in
  // lowercase.
  assert-equal("ftp", uri-scheme(string-to-uri("FTP:/a")),
               "scheme lowercased?");
end test;

define test test-authority ()
  let u = string-to-uri("http://user:p%40ss@host.org");
  assert-equal("user:p%40ss", u.uri-raw-user-info);
  assert-equal("user:p@ss", u.uri-user-info);
  assert-equal("host.org", u.uri-host);
  assert-equal("user:p@ss@host.org", uri-authority(u));
  assert-equal("user:p%40ss@host.org", uri-authority(u, percent-encoded?: #t));

  let u1 = string-to-uri("//%61.org/a/b#4");
  assert-equal("%61.org", u1.uri-raw-host);
  assert-equal("a.org", u1.uri-host);

  let u2 = string-to-uri("https://[aa::bb]:123/foo");
  assert-equal("[aa::bb]", u2.uri-host);
  assert-equal(123, u2.uri-port);

  let u3 = string-to-uri("https://1.2.3.4:123/foo");
  assert-equal("1.2.3.4", u3.uri-host);
  assert-equal(123, u3.uri-port);

  let u4 = string-to-uri("//[aa::bb]/foo");
  assert-equal("[aa::bb]", u4.uri-host);
  assert-false(u4.uri-port);

  let u5 = string-to-uri("https://1.2.3.4/foo");
  assert-equal("1.2.3.4", u5.uri-host);
  assert-equal(443, u5.uri-port);
end test;

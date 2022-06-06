module: uri-test-suite

define macro uri-reference-test-definer
  { define uri-reference-test ?name:token
      ?uri:token
      ( ?scheme:token , ?authority:token ,
        ?path:token , ?query:token ,
        ?fragment:token )
      => ?result:token
   end
  }
   =>
  {
    define test "uri-reference-" ## ?name ()
      let uri = parse-uri(?uri);
      check-equal("scheme", uri.uri-scheme, ?scheme);
      check-equal("authority", uri.uri-authority, ?authority);
      check-equal("path", build-path(uri), ?path);
      check-equal("query", build-query(uri), ?query);
      check-equal("fragment", uri.uri-fragment, ?fragment);
      let target-uri = transform-uris($base-uri, uri);
      check-equal("target-uri", build-uri(target-uri), ?result);
    end
  }
end macro;

define constant $base-uri = parse-uri("http://a/b/c/d;p?q");

define test uri-base-test ()
  check-equal("base-uri scheme", $base-uri.uri-scheme, "http");
  check-equal("base-uri authority", $base-uri.uri-authority, "a");
  check-equal("base-uri path", build-path($base-uri), "/b/c/d;p");
  check-equal("base-uri query", build-query($base-uri), "q");
  check-equal("base-uri fragment", $base-uri.uri-fragment, "");
end;

define test uri-plus-decode-test ()
  let uri = parse-uri("http://a/p?x=a+test");
  check-equal("uri query x has a plus", uri.uri-query["x"], "a+test");
  let url = parse-url("http://a/p?x=a+test");
  check-equal("url query x has a space", url.uri-query["x"], "a test");
end;

define test uri-percent-encode-test ()
  check-equal("percent encode space", percent-encode($uri-pchar, " "), "%20");
end;

define test uri-percent-decode-test ()
  let uri = parse-uri("http://a/p?x=a%20test&y=it%27s");
  check-equal("uri query x", uri.uri-query["x"], "a test");
  check-equal("uri query y", uri.uri-query["y"], "it's");
end;

define uri-reference-test normal-test-1
  "g:h" ("g", "", "h", "", "") => "g:h"
end;

define uri-reference-test normal-test-2
  "g" ("", "", "g", "", "") => "http://a/b/c/g"
end;

define uri-reference-test normal-test-3
  "./g" ("", "", "./g", "", "") => "http://a/b/c/g"
end;

define uri-reference-test normal-test-4
  "g/" ("", "", "g/", "", "") => "http://a/b/c/g/"
end;

define uri-reference-test normal-test-5
  "/g" ("", "", "/g", "", "") => "http://a/g"
end;

define uri-reference-test normal-test-6
  "//g" ("", "g", "", "", "") => "http://g"
end;

define uri-reference-test normal-test-7
  "?y" ("", "", "", "y", "") => "http://a/b/c/d;p?y"
end;

define uri-reference-test normal-test-8
  "g?y" ("", "", "g", "y", "") => "http://a/b/c/g?y"
end;

define uri-reference-test normal-test-9
  "#s" ("", "", "", "", "s") => "http://a/b/c/d;p?q#s"
end;

define uri-reference-test normal-test-10
  "g#s" ("", "", "g", "", "s") => "http://a/b/c/g#s"
end;

define uri-reference-test normal-test-11
  "g?y#s" ("", "", "g", "y", "s") => "http://a/b/c/g?y#s"
end;

define uri-reference-test normal-test-12
  ";x" ("", "", ";x", "", "") => "http://a/b/c/;x"
end;

define uri-reference-test normal-test-13
  "g;x" ("", "", "g;x", "", "") => "http://a/b/c/g;x"
end;

define uri-reference-test normal-test-14
  "g;x?y#s" ("", "", "g;x", "y", "s") => "http://a/b/c/g;x?y#s"
end;

define uri-reference-test normal-test-15
  "" ("", "", "", "", "") => "http://a/b/c/d;p?q"
end;

define uri-reference-test normal-test-16
  "." ("", "", ".", "", "") => "http://a/b/c/"
end;

define uri-reference-test normal-test-17
  "./" ("", "", "./", "", "") => "http://a/b/c/"
end;

define uri-reference-test normal-test-18
  ".." ("", "", "..", "", "") => "http://a/b/"
end;

define uri-reference-test normal-test-19
  "../" ("", "", "../", "", "") => "http://a/b/"
end;

define uri-reference-test normal-test-20
  "../g" ("", "", "../g", "", "") => "http://a/b/g"
end;

define uri-reference-test normal-test-21
  "../.." ("", "", "../..", "", "") => "http://a/"
end;

define uri-reference-test normal-test-22
  "../../" ("", "", "../../", "", "") => "http://a/"
end;

define uri-reference-test normal-test-23
  "../../g" ("", "", "../../g", "", "") => "http://a/g"
end;

define uri-reference-test has-port-test-1
  "http://host:80" ("http", "host:80", "", "", "") => "http://host:80"
end;

define uri-reference-test has-port-test-2
  "http://host:80/abc" ("http", "host:80", "/abc", "", "") => "http://host:80/abc"
end;


define uri-reference-test abnormal-test-1
  "../../../g" ("", "", "../../../g", "", "") => "http://a/g"
end;

define uri-reference-test abnormal-test-2
  "../../../../g" ("", "", "../../../../g", "", "") => "http://a/g"
end;

define uri-reference-test abnormal-test-3
  "/./g" ("", "", "/./g", "", "") => "http://a/g"
end;

define uri-reference-test abnormal-test-4
  "/../g" ("", "", "/../g", "", "") => "http://a/g"
end;

define uri-reference-test abnormal-test-5
  "g." ("", "", "g.", "", "") => "http://a/b/c/g."
end;

define uri-reference-test abnormal-test-6
  ".g" ("", "", ".g", "", "") => "http://a/b/c/.g"
end;

define uri-reference-test abnormal-test-7
  "g.." ("", "", "g..", "", "") => "http://a/b/c/g.."
end;

define uri-reference-test abnormal-test-8
  "..g" ("", "", "..g", "", "") => "http://a/b/c/..g"
end;

define uri-reference-test abnormal-test-9
  "./../g" ("", "", "./../g", "", "") => "http://a/b/g"
end;

define uri-reference-test abnormal-test-10
  "./g/." ("", "", "./g/.", "", "") => "http://a/b/c/g/"
end;

define uri-reference-test abnormal-test-11
  "g/./h" ("", "", "g/./h", "", "") => "http://a/b/c/g/h"
end;

define uri-reference-test abnormal-test-12
  "g/../h" ("", "", "g/../h", "", "") => "http://a/b/c/h"
end;

define uri-reference-test abnormal-test-13
  "g;x=1/./y" ("", "", "g;x=1/./y", "", "") => "http://a/b/c/g;x=1/y"
end;

define uri-reference-test abnormal-test-14
  "g;x=1/../y" ("", "", "g;x=1/../y", "", "") => "http://a/b/c/y"
end;

define test uri-path-segment-normalization-test ()
  check-equal("path", "/a/c", remove-dot-segments("/a/b/../c"));
  check-equal("path", "/a/b/c", remove-dot-segments("/a/b/./c"));
  check-equal("path", "/a/c", remove-dot-segments("/a/./b/../c"));
  check-equal("path", "/b/c", remove-dot-segments("/a/../b/./c"));
  check-equal("path", "/a/c", remove-dot-segments("/a/b/./../c"));
  check-equal("path", "/a/c", remove-dot-segments("/a/b/.././c"));
  check-equal("path", "/b/c", remove-dot-segments("/a/../../../b/c"));

  check-equal("path", "a/c", remove-dot-segments("a/b/../c"));
  check-equal("path", "a/b/c", remove-dot-segments("a/b/./c"));
  check-equal("path", "a/c", remove-dot-segments("a/./b/../c"));
  check-equal("path", "b/c", remove-dot-segments("a/../b/./c"));
  check-equal("path", "a/c", remove-dot-segments("a/b/./../c"));
  check-equal("path", "a/c", remove-dot-segments("a/b/.././c"));
  check-equal("path", "b/c", remove-dot-segments("a/../../../b/c"));
end;

define test parse-error-test ()
  for (uri in #["http://host:xx/abc"])
    assert-signals(<uri-parse-error>, parse-uri(uri), uri);
  end;
end test parse-error-test;

define test split-query-test ()
  let t = make(<string-table>);
  t["a"] := #t;
  t["b"] := #("3", "2");
  t["c"] := #(#t, "4");
  t["d"] := "5";
  assert-equal(split-query("a&b=2&b=3&c=4&c&d=5"), t);
end;

run-test-application()

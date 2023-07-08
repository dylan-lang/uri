module: dylan-user

define library uri
  use common-dylan;
  use collection-extensions;
  use io;
  use system;
  use strings;

  export uri;
end;

define module uri
  use common-dylan,
    exclude: { format-to-string };
  use format;
  use streams,
    import: { <byte>, <byte-character> };
  use strings;

  export
    <uri>, <url>,
    uri-scheme, uri-scheme-setter,
    uri-userinfo, uri-userinfo-setter,
    uri-host, uri-host-setter,
    uri-port, uri-port-setter,
    uri-path, uri-path-setter,
    uri-query, uri-query-setter,
    uri-fragment, uri-fragment-setter,
    uri-authority /* not defined --cgay   uri-authority-setter */;
  export
    parse-uri, parse-url,
    build-uri, transform-uris,
    build-path, build-query;
  export
    remove-dot-segments,
    split-path,
    split-query;
  export
    absolute?,
    relative?;
  export
    percent-encode,
    percent-decode,
    $uri-pchar;
  export
    <uri-parse-error>;
end;

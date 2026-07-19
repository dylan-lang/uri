module: dylan-user

define library uri
  use common-dylan;
  use io;
  use system;
  use strings;

  export uri;
end library;

define module uri
  // Errors
  create
    <uri-error>,
    <uri-parse-error>;

  // Creation and conversion
  create
    merge-uris,
    split-uri,
    string-to-uri,
    uri-to-string;

  // URI class and getters
  create
    <uri>,
    uri-scheme,
    uri-user-info, uri-raw-user-info,
    uri-host, uri-raw-host,
    uri-port,
    uri-authority,
    uri-path, uri-raw-path,
    uri-query, uri-raw-query, uri-query-value,
    uri-fragment, uri-raw-fragment,
    uri-relative?;

  // Percent encoding
  create
    percent-encode,
    percent-decode,
    $charset-user-info,
    $charset-host,
    $charset-path,
    $charset-query,
    $charset-fragment;
end module;

define module uri-internal
  use byte-vector,
    import: { copy-bytes };
  use common-dylan,
    exclude: { format-to-string };
  use streams;
  use strings;
  use uri;
end module;

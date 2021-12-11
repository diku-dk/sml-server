# sml-server [![CI](https://github.com/diku-dk/sml-server/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-server/actions)

This Standard ML package provides the basics for running an HTTP web
server for which requests are handled by Standard ML code.

The functionality is based on the
[Http](https://github.com/diku-dk/sml-http/blob/main/lib/github.com/diku-dk/sml-http/http.sig)
library from the [sml-http](https://github.com/diku-dk/sml-http)
package and utilises the inet-socket functionality of the Standard ML
basis library.

__Notice__: This code is work in progress and the aim is for the
library to implement most of the features of [SMLserver](http://smlserver.org), a web
server Apache module. On the contrary to the original SMLserver
design, the present code makes it possible to run native code for
servicing requests.

## Overview of MLB files

- `lib/github.com/diku-dk/sml-server/server.mlb`:

  - **signature** [`SERVER`](lib/github.com/diku-dk/sml-server/server.sig)

  - **signature** [`SERVER_UTIL`](lib/github.com/diku-dk/sml-server/server-util.sig)

  - **signature** [`SERVER_SERIALIZE`](lib/github.com/diku-dk/sml-server/server-serialize.sig)

  - **signature** [`SERVER_COOKIE`](lib/github.com/diku-dk/sml-server/server-cookie.sig)

  - **signature** [`SERVER_REQ`](lib/github.com/diku-dk/sml-server/server-req.sig)

  - **signature** [`SERVER_RESP`](lib/github.com/diku-dk/sml-server/server-resp.sig)

  - **signature** [`SERVER_CONN`](lib/github.com/diku-dk/sml-server/server-conn.sig)

  - **signature** [`SERVER_INFO`](lib/github.com/diku-dk/sml-server/server-info.sig)

  - **structure** `Server` :> `SERVER`

## Demonstration

An example use of the server functionality is available in
[test1.sml](lib/github.com/diku-dk/sml-server/test/test1.sml). After
the [mlb-file](lib/github.com/diku-dk/sml-server/test/test1.mlb) is
compiled, the executable acts as a web server, which can be started
from the command-line:

```
  $ ./test1.exe
  HTTP/1.1 server started on port 8000
  Use C-c to exit the server loop...
```

Help is available by passing the `--help` command-line option.

## Use of the package

This library is set up to work well with the SML package manager
[smlpkg](https://github.com/diku-dk/smlpkg).  To use the package, in
the root of your project directory, execute the command:

```
$ smlpkg add github.com/diku-dk/sml-server
```

This command will add a _requirement_ (a line) to the `sml.pkg` file in your
project directory (and create the file, if there is no file `sml.pkg`
already).

To download the library into the directory
`lib/github.com/diku-dk/sml-server` (along with other necessary
libraries), execute the command:

```
$ smlpkg sync
```

You can now reference the `mlb`-file using relative paths from within
your project's `mlb`-files.

Notice that you can choose either to treat the downloaded package as
part of your own project sources (vendoring) or you can add the
`sml.pkg` file to your project sources and make the `smlpkg sync`
command part of your build process.

## Try it!

The library works with either [MLton](http://mlton.org) or
[MLKit](http://elsman.com/mlkit/).

After checking out the sources, write

    $ smlpkg sync

Then simply write `make test` in your shell (assuming
[MLKit](http://elsman.com/mlkit/) is installed).

To use MLton as a compiler, write instead:

    $ MLCOMP=mlton make clean test

## Authors

Copyright (c) 2015-2021 Martin Elsman, University of Copenhagen.

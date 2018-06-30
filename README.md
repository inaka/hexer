# hexer

Hex.pm integration in escript format.

## Contact Us

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/elvis/issues/new) in this repo
(or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io).

## Building

To build `hexer` just run `make escript`. This will compile the project
and generate an executable binary in `bin/hexer`.

## Usage

Once the `hexer` binary is available you can run it as any other command tool:

```
$ hexer
Usage: hexer [-h] [-v] [command]

  -h, --help     Show this help information.
  -v, --version  Show the version of this tool.

Commands:

  user.auth      Generate API key by providing username and password.
  user.register  Register new user.
  publish        Publish package in hex.pm.
```

## Registering (`user.register`)

Before you can publish a package to hex.pm you need to have a registered user.
If you already have one then skip this section and go to [Authorization](#authorization).

Running `hexer user.register` will bring up a prompt asking for an email address,
a username and a password. Once you enter all this information if there is any
problem then there will be an error shown. If the user is successfully registered
then you will also get a confirmation message.

## Authorization (`user.auth`)

Once you have a registered user you need to generate the API key that will be
used for authentication to the hex.pm server.

The process is simple, just run `hexer user.auth` and you will be prompted
for username and password. If the credentials are valid a `hexer.config` file
will be created containing the information necessary to connect to the hex.pm
server.

## Publishing a Package (`publish`)

When publishing a package, `hexer` extracts information from the
`application_name.app.src` and the `Makefile`.

All dependencies are extracted from the `Makefile`, but **only** those that are
configured as **hex packages** are included in the published package. This means
that you need to make sure that all libraries your package depends on exist as
a hex package and specified in the `Makefile` as:

```
dep_name = hex 0.0.1
```

All other information is taken from the `application_name.app.src` file. The
project's name is the atom found as the second element in the `application_name.app.src`
tuple. Additional properties are read from the proplist in the file's specification,
these include:

- `vsn` (required): the version number of the package, specified as the `vsn` entry.
- `description`: a description for the project. A string. Required.
- `links`: list of useful or related links. A list of two element tuples, where the
  first element is the link's text and the second the url. Default: `[]`.
- `maintainers`: list of the project's maintainers names. A list of strings.
  Default: `[]`.
- `files`: list of files and folders (strings) that should be included in the package.
  Default: `["src", "c_src", "include", "priv", "rebar.config.script", "rebar.config",
  "rebar.lock", "Makefile", "Emakefile", "erlang.mk", "README*", "readme*", "LICENSE*", "license*"]`.
- `licenses`: list of license names (strings). Required.
- `pkg_name`: set a new package name for avoid name collisions. Default: `ebin/*.app or src/*.app.src name value`.
- `build_tools`: list of available build tools . Default: `["make"]`.

These are the same properties the [rebar3_hex](https://github.com/hexpm/rebar3_hex)
plugin reads from the same file.
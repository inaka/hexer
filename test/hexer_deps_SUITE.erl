-module(hexer_deps_SUITE).

-include_lib("inaka_mixer/include/mixer.hrl").
-mixin([{ hexer_test_utils
        , [ init_per_suite/1
          , end_per_suite/1
          ]
        }
       ]).

-export([ all/0
        ]).

-export([ resolve_basic/1
        , resolve_empty/1
        , resolve_multiple_lines/1
        , resolving_no_hex_deps/1
        ]).

-spec all() -> [atom()].
all() -> hexer_test_utils:all(?MODULE).

-spec resolve_basic(hexer_test_utils:config()) -> {comment, string()}.
resolve_basic(_Config) ->

  ct:comment("Simple Makefile works with hex dependencies"),
  create_makefile(
    "PROJECT = test \n"
    "DEPS = validerl\n"
    "dep_validerl = hex 1.0.0\n"
    ),
  ResultBasic = hexer_deps:resolve("."),
  Dependecy = validerl,
  [ {Dependecy,
    [ {<<"app">>, Dependecy}
    , {<<"requirement">>, "1.0.0"}
    , {<<"optional">>, false}
    ]}
  ] = ResultBasic,

  ct:comment("Shell/Test Deps should be ignored"),
  create_makefile(
    "DEPS = validerl \n"
    "TEST_DEPS = dep3 dep5\n"
    "SHELL_DEPS = dep6 dep7\n"
    "\n"
    "dep_validerl = hex 1.0.0\n"
    "dep_dep2 = hex 0.2.2\n"
    "dep_dep3 = hex 3.4.5\n"
    "dep_dep5 = git https://github.com/user/dep5 e666999a\n"
    "dep_dep6 = git https://github.com/user/dep5 a999666e\n"
    ),

  ResultBasic = hexer_deps:resolve("."),

  {comment, ""}.


-spec resolving_no_hex_deps(hexer_test_utils:config()) -> {comment, string()}.
resolving_no_hex_deps(_Config) ->

  ct:comment("Simple Makefile doesn't works with NO hex dependencies"),
  ErrorDependency = "dep_dep1 = git https://github.com/user/dep5 e666999a",
  create_makefile(
    "DEPS = dep1 \n"
     ++ ErrorDependency ++ "\n"
    ),
  ok = try _ = hexer_deps:resolve("."), error
       catch _:{hexer_deps, no_hex_dependency} -> ok
       end,

  ct:comment("Complex Makefile doesn't works with NO hex dependencies"),
  ErrorDep1 = "dep_dep1 = git https://github.com/user/dep5 e666999a",
  ErrorDep2 = "dep_dep6 = git https://github.com/user/dep5 a999666e",
  create_makefile(
    "DEPS = dep1 dep6\n"
    "TEST_DEPS  = dep3 dep5\n"
    "SHELL_DEPS = dep7\n"
    "\n"
    "dep_dep1 = hex 0.1.2\n"
    "dep_dep2 = hex 0.2.2\n"
    "dep_dep3 = hex 3.4.5\n"
    ++ ErrorDep1 ++ "\n"
    ++ ErrorDep2 ++ "\n"
    ),
  ok = try _ = hexer_deps:resolve("."), error
       catch _:{hexer_deps, no_hex_dependency} -> ok
       end,
  {comment, ""}.

-spec resolve_empty(hexer_test_utils:config()) -> {comment, string()}.
resolve_empty(_Config) ->

  ct:comment("Empty Makefile should produce no deps"),
  create_makefile(""),
  [] = hexer_deps:resolve("."),

  ct:comment("No hex deps produce no deps"),
  create_makefile(
    "PROJECT = test \n"
    "SHELL_DEPS = dep_dep1 dep_dep2\n"
    "dep_dep1 = cp /a/path\n"
    "dep_dep2 = git https://github.com/user/dep2 b00b1e5\n"
    ),

  [] = hexer_deps:resolve("."),

  ct:comment("Shell/Test Deps should be ignored"),
  create_makefile(
    "PROJECT = test \n"
    "TEST_DEPS = dep3 dep5\n"
    "SHELL_DEPS = dep9 dep100\n"
    "\n"
    "dep_dep2 = git https://github.com/user/dep2 b00b1e5\n"
    "dep_dep3 = hex 3.4.5\n"
    "dep_dep5 = git https://github.com/user/dep5 e666999a\n"
    "dep_dep6 = hex 6.7.8\n"
    ),

  [] = hexer_deps:resolve("."),

  {comment, ""}.

-spec resolve_multiple_lines(hexer_test_utils:config()) -> {comment, string()}.
resolve_multiple_lines(_Config) ->

  ct:comment("Simple Makefile works"),
  create_makefile(
    "PROJECT = test \n"
    "DEPS = trails  \n"
    "\n"
    "dep_trails = hex 0.1.1\n"
    "dep_dep2 = hex 2.4.5\n"
    "dep_dep3 = hex 3.4.5\n"
    ),
   ResultMultiple = hexer_deps:resolve("."),
   [ {trails,
       [ {<<"app">>, trails}
       , {<<"requirement">>, "0.1.1"}
       , {<<"optional">>, false}
       ]}
   ] = ResultMultiple,

  ct:comment("Shell/Test Deps should be ignored"),
  create_makefile(
    "PROJECT = test \n"
    "DEPS = tirerl  barrel_jiffy  \n"
    "TEST_DEPS = dep4 dep5\n"
    "SHELL_DEPS = dep6 dep7\n"
    "\n"
    "dep_barrel_jiffy = hex   0.14.5\n"
    "dep_tirerl = hex 0.1.9\n"
    "dep_dep4 = hex 2.3.4\n"
    "dep_dep5 = git https://github.com/user/dep5 e666999a\n"
    "dep_dep6 = hex 5.6.7\n"
    ),
  [ {tirerl  ,
      [ { <<"app">>, tirerl }
        , {<<"requirement">>, "0.1.9"}
        , {<<"optional">>, false}
      ]}
  , {barrel_jiffy,
      [ { <<"app">>, jiffy}
        , {<<"requirement">>, "0.14.5"}
        , {<<"optional">>, false}
      ]}] = hexer_deps:resolve("."),

  {comment, ""}.

create_makefile(Contents) ->
  ok = file:write_file("Makefile", [Contents, "include ../../erlang.mk\n"]).

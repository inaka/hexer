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
    "DEPS = dep1 \n"
    "\n"
    "dep_dep1 = hex 0.1.1\n"
    ),

  [{dep1, "0.1.1"}] = hexer_deps:resolve("."),

  ct:comment("Shell/Test Deps should be ignored"),
  create_makefile(
    "DEPS = dep1 \n"
    "TEST_DEPS = dep3 dep5\n"
    "SHELL_DEPS = dep6 dep7\n"
    "\n"
    "dep_dep1 = hex 0.1.2\n"
    "dep_dep2 = hex 0.2.2\n"
    "dep_dep3 = hex 3.4.5\n"
    "dep_dep5 = git https://github.com/user/dep5 e666999a\n"
    "dep_dep6 = git https://github.com/user/dep5 a999666e\n"
    ),

  [{dep1, "0.1.2"}] = hexer_deps:resolve("."),

  {comment, ""}.


-spec resolving_no_hex_deps(hexer_test_utils:config()) -> {comment, string()}.
resolving_no_hex_deps(_Config) ->

  ct:comment("Simple Makefile doesn't works with NO hex dependencies"),
  ErrorDependency = "dep_dep1 = git https://github.com/user/dep5 e666999a",
  create_makefile(
    "DEPS = dep1 \n"
     ++ ErrorDependency ++ "\n"
    ),
  "dep_dep1 = " ++ TrimedDep = ErrorDependency,
  [{no_hex_dependency, TrimedDep}] = hexer_deps:resolve("."),

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
   "dep_dep1 = " ++ TrimedDep1 = ErrorDep1,
   "dep_dep6 = " ++ TrimedDep2 = ErrorDep2,
  [ {no_hex_dependency, TrimedDep1}
  , {no_hex_dependency, TrimedDep2}
  ] = hexer_deps:resolve("."),

  {comment, ""}.
-spec resolve_empty(hexer_test_utils:config()) -> {comment, string()}.
resolve_empty(_Config) ->

  ct:comment("Empty Makefile should produce no deps"),
  create_makefile(""),
  [] = hexer_deps:resolve("."),

  ct:comment("No hex deps produce no deps"),
  create_makefile(
    "SHELL_DEPS = dep_dep1 dep_dep2\n"
    "dep_dep1 = cp /a/path\n"
    "dep_dep2 = git https://github.com/user/dep2 b00b1e5\n"
    ),

  [] = hexer_deps:resolve("."),

  ct:comment("Shell/Test Deps should be ignored"),
  create_makefile(
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
    "DEPS = dep1 dep2\n"
    "DEPS += dep3\n"
    "\n"
    "dep_dep1 = hex 0.1.2\n"
    "dep_dep2 = hex 2.4.5\n"
    "dep_dep3 = hex 3.4.5\n"
    ),

  [{dep1, "0.1.2"}, {dep2, "2.4.5"}, {dep3, "3.4.5"}] = hexer_deps:resolve("."),

  ct:comment("Shell/Test Deps should be ignored"),
  create_makefile(
    "DEPS = dep1 dep2\n"
    "TEST_DEPS = dep4 dep5\n"
    "DEPS += dep3\n"
    "SHELL_DEPS = dep6 dep7\n"
    "\n"
    "dep_dep1 = hex 0.1.1\n"
    "dep_dep2 = hex 2.2.2\n"
    "dep_dep3 = hex 3.3.3\n"
    "dep_dep4 = hex 2.3.4\n"
    "dep_dep5 = git https://github.com/user/dep5 e666999a\n"
    "dep_dep6 = hex 5.6.7\n"
    ),

  [{dep1, "0.1.1"}, {dep2, "2.2.2"}, {dep3, "3.3.3"}] = hexer_deps:resolve("."),

  {comment, ""}.

create_makefile(Contents) ->
  ok = file:write_file("Makefile", ["include ../../erlang.mk\n", Contents]).

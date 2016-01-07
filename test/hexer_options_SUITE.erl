-module(hexer_options_SUITE).

-include_lib("inaka_mixer/include/mixer.hrl").
-mixin([{ hexer_test_utils
        , [ init_per_suite/1
          , end_per_suite/1
          ]
        }
       ]).

-export([all/0]).

-export([ help/1
        , version/1
        ]).

-spec all() -> [atom()].
all() -> hexer_test_utils:all(?MODULE).

-spec help(hexer_test_utils:config()) -> {comment, string()}.
help(_Config) ->
  HelpRegex = "Usage: .*",

  ct:comment("Show help"),
  HelpFun = fun() -> ok = hexer_options:help() end,
  hexer_test_utils:check_output(HelpRegex, HelpFun),

  {comment, ""}.

-spec version(hexer_test_utils:config()) -> {comment, string()}.
version(_Config) ->
  VersionRegex = "Version .*",

  ct:comment("Show version information"),
  VersionFun = fun() -> ok = hexer_options:version() end,
  hexer_test_utils:check_output(VersionRegex, VersionFun),

  {comment, ""}.

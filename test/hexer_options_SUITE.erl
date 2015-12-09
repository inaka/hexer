-module(hexer_options_SUITE).

-include_lib("mixer/include/mixer.hrl").
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
  ok = hexer_options:help(),
  {comment, ""}.

-spec version(hexer_test_utils:config()) -> {comment, string()}.
version(_Config) ->
  ok = hexer_options:version(),
  {comment, ""}.

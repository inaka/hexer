-module(hexer_options).

-export([option_spec_list/0]).

-export([ help/0
        , version/0
        ]).

-spec option_spec_list() -> [getopt:option_spec()].
option_spec_list() ->
  [{help,     $h, "help",     undefined, "Show this help information."},
   {version,  $v, "version",  undefined, "Show the version of this tool."}
  ].

-spec help() -> ok.
help() ->
  OptSpecList = option_spec_list(),
  getopt:usage(OptSpecList, "hexer", standard_io).

-spec version() -> ok.
version() ->
  {ok, AppConfig} = application:get_all_key(hexer),
  Vsn = proplists:get_value(vsn, AppConfig),
  Version = "Hexer - A standalone command-line interface to hex.pm~nVersion ~s",
  hexer_utils:print(Version, [Vsn]).

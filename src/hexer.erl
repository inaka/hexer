-module(hexer).

-export([main/1]).

-spec main([string()]) -> ok.
main(Args) ->
  {ok, _} = application:ensure_all_started(?MODULE),
  OptSpecList = option_spec_list(),
  case getopt:parse(OptSpecList, Args) of
    {ok, {[], []}} ->
      help();
    {ok, {Options, Commands}} ->
      try
        AtomCommands = [list_to_existing_atom(Cmd) || Cmd <- Commands],
        process(hexer_options, Options),
        process(hexer_commands, AtomCommands),
        ok
      catch
        _:Reason -> hexer_utils:error(Reason)
      end;
    {error, Error} ->
      hexer_utils:error(Error),
      help()
  end,
  ok.

-spec option_spec_list() -> [getopt:option_spec()].
option_spec_list() ->
  [{help,     $h, "help",     undefined, "Show this help information."},
   {version,  $v, "version",  undefined, "Show the version of this tool."}
  ].

-spec help() -> ok.
help() ->
  OptSpecList = option_spec_list(),
  getopt:usage(OptSpecList, ?MODULE, standard_io).

-type option() :: {atom(), any()} | atom().
-type command() :: atom().

-spec process(module(), [option() | command()] | option() | command()) -> ok.
process(Module, Items) when is_list(Items) ->
  [process(Module, Item) || Item <- Items];
process(Module, {Name, Arg}) ->
  Module:Name(Arg);
process(Module, Name) ->
  Module:Name().

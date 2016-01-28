%%% @doc escript for hexer
-module(hexer).

-export([main/1]).

%% @doc entry point for escript
-spec main([string()]) -> ok.
main(Args) ->
  {ok, _} = application:ensure_all_started(hexer),
  OptSpecList = hexer_options:option_spec_list(),
  try
    case getopt:parse(OptSpecList, Args) of
      {ok, {[], []}} ->
        hexer_options:help();
      {ok, {Options, Commands}} ->
        AtomCommands = [list_to_atom(Cmd) || Cmd <- Commands],
        process(hexer_options, Options),
        process(hexer_commands, AtomCommands),
        ok;
      {error, Error} -> throw(Error)
    end
  catch
    _:Reason ->
      hexer_utils:error(Reason),
      hexer_options:help()
  end.

-type option() :: {atom(), any()} | atom().
-type command() :: atom().

-spec process(module(), [option() | command()] | option() | command()) -> ok.
process(Module, Items) when is_list(Items) ->
  [process(Module, Item) || Item <- Items];
process(Module, Name) ->
  Module:Name().

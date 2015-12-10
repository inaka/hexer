-module(hexer_config).

-export([update/2]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec update(atom(), any()) -> ok.
update(Key, Value) ->
  Config = read(),
  NewConfig = lists:ukeymerge(1, [{Key, Value}], lists:keysort(1, Config)),
  write(NewConfig).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

-spec read() -> [proplists:property()].
read() ->
  case file:consult(config_path()) of
    {ok, Config} -> Config;
    {error, enoent} -> []
  end.

-spec write([proplists:property()]) -> ok.
write(Config) ->
  ConfigEncoded = [[io_lib:print(KV), ".\n"] || KV <- Config],
  ok = file:write_file(config_path(), iolist_to_binary(ConfigEncoded)).

-spec config_path() -> string().
config_path() -> "hexer.config".

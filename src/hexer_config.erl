%%% @doc hexer.config manager
-module(hexer_config).

-export([ update/2
        , api_key/0
        ]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------
%% @doc Updates an existing API key
-spec update(atom(), any()) -> ok.
update(Key, Value) ->
  Config = read(),
  NewConfig = lists:ukeymerge(1, [{Key, Value}], lists:keysort(1, Config)),
  write(NewConfig).

%% @doc Generates a new API key
-spec api_key() -> {ok, string()}.
api_key() ->
  case lists:keyfind(key, 1, read()) of
    {key, Key} -> {ok, Key};
    _ -> throw(no_api_key)
  end.

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

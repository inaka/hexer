-module(hexer_server).

-export([new_api_key/3]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec new_api_key(binary(), binary(), binary()) ->
  {ok, binary()} | {error, any()}.
new_api_key(Name, Username, Password) ->
  Path = "keys",
  Headers = #{basic_auth => {Username, Password}},
  Body = jsxn:encode(#{<<"name">> => Name}),
  {ok, Conn} = open_connection(),
  case shotgun:post(Conn, Path, Headers, Body, #{}) of
    {ok, #{status_code := StatusCode, body:= Body}}
      when 200 =< StatusCode, StatusCode < 300 ->
      #{<<"secret">> := Secret} = jsxn:decode(Body),
      {ok, Secret};
    {ok, #{status_code := StatusCode, body:= Body}} ->
      {error, StatusCode};
    {error, Error} ->
      {error, Error}
  end.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

-spec open_connection() -> {ok, shotgun:connection()} | {error, any()}.
open_connection() ->
  {Url, Port} = connection_info(),
  shotgun:open(Url, Port, https).

connection_info() ->
  {api_url(), api_port()}.

-spec api_url() -> string().
api_url() -> "https://hex.pm".

-spec api_port() -> non_neg_integer().
api_port() -> 443.

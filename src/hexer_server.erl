-module(hexer_server).

-export([new_api_key/3]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec new_api_key(binary(), string(), string()) ->
  {ok, binary()} | {error, any()}.
new_api_key(Name, Username, Password) ->
  Path = "/api/keys",
  Headers = #{basic_auth         => {Username, Password},
              <<"Accept">>       => <<"application/vnd.hex+erlang">>,
              <<"Content-Type">> => <<"application/vnd.hex+erlang">>},
  Body = term_to_binary(#{<<"name">> => Name}),
  {ok, Conn} = open_connection(),

  case shotgun:post(Conn, Path, Headers, Body, #{}) of
    {ok, #{status_code := StatusCode, body:= ResBody}}
      when 200 =< StatusCode, StatusCode < 300 ->
      #{<<"secret">> := Secret} = binary_to_term(ResBody),
      {ok, Secret};
    {ok, #{status_code := StatusCode, body := ResBody}} ->
      {error, {StatusCode, binary_to_term(ResBody)}};
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
api_url() -> "hex.pm".

-spec api_port() -> non_neg_integer().
api_port() -> 443.

-module(hexer_server).

-export([ new_api_key/3
        , create_user/3
        ]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec new_api_key(binary(), string(), string()) ->
  {ok, binary()} | {error, any()}.
new_api_key(Name, Username, Password) ->
  Body = term_to_binary(#{<<"name">> => Name}),

  case post(Username, Password, "/api/keys", Body) of
    {ok, #{status_code := StatusCode, body:= ResBody}}
      when 200 =< StatusCode, StatusCode < 300 ->
      #{<<"secret">> := Secret} = binary_to_term(ResBody),
      {ok, Secret};
    {ok, #{status_code := StatusCode, body := ResBody}} ->
      {error, {StatusCode, binary_to_term(ResBody)}};
    {error, Error} ->
      {error, Error}
  end.

-spec create_user(string(), string(), string()) -> ok | {error, any()}.
create_user(Username, Email, Password) ->
  Body = term_to_binary(#{ <<"username">> => list_to_binary(Username)
                         , <<"email">>    => list_to_binary(Email)
                         , <<"password">> => list_to_binary(Password)
                         }
                       ),

  case post(Username, Password, "/api/users", Body) of
    {ok, #{status_code := StatusCode}}
      when 200 =< StatusCode, StatusCode < 300 ->
      ok;
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

-spec post(string(), string(), string(), binary()) ->
  {ok, map()} | {error, any()}.
post(Username, Password, Path, Body) ->
  {ok, Conn} = open_connection(),

  Headers = #{ basic_auth         => {Username, Password}
             , <<"Accept">>       => <<"application/vnd.hex+erlang">>
             , <<"Content-Type">> => <<"application/vnd.hex+erlang">>
             },

  shotgun:post(Conn, Path, Headers, Body, #{}).

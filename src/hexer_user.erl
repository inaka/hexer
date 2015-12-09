-module(hexer_user).

-export([auth/0]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec auth() -> ok.
auth() ->
  Username = list_to_binary(hexer_utils:prompt("Username:", string)),
  Password = list_to_binary(hexer_utils:prompt("Password:", password)),
  generate_key(Username, Password).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

-spec generate_key(binary(), binary()) -> ok.
generate_key(Username, Password) ->
  hexer_utils:print("Generating API key..."),
  {ok, Name} = inet:gethostname(),
  case hexer_server:new_api_key(list_to_binary(Name), Username, Password) of
    {ok, Secret} ->
      hexer_config:update(username, Username),
      hexer_config:update(secret, Secret);
    {error, Error} ->
      hexer_utils:print("Generation of API key failed (~p)", [Error]),
      {error, Error}
  end.

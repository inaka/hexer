-module(hexer_user).

-export([auth/0]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec auth() -> ok | {error, any()}.
auth() ->
  Username = hexer_utils:prompt("Username: ", "~s"),
  Password = hexer_utils:prompt("Password: ", password),
  generate_key(Username, Password).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

-spec generate_key(string(), string()) -> ok | {error, any()}.
generate_key(Username, Password) ->
  hexer_utils:print("Generating API key..."),
  {ok, Name} = inet:gethostname(),
  case hexer_server:new_api_key(list_to_binary(Name), Username, Password) of
    {ok, Secret} ->
      hexer_config:update(username, Username),
      hexer_config:update(secret, binary_to_list(Secret));
    {error, Error} ->
      hexer_utils:error("Generation of API key failed (~p)", [Error]),
      {error, Error}
  end.

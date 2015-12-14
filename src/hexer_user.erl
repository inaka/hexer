-module(hexer_user).

-export([ auth/0
        , register/0
        ]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec auth() -> ok | {error, any()}.
auth() ->
  Username = hexer_utils:prompt("Username: ", "~s"),
  Password = hexer_utils:prompt("Password: ", password),
  generate_key(Username, Password).

-spec 'register'() -> ok | {error, any()}.
register() ->
  Username = hexer_utils:prompt("Username: ", "~s"),
  Email    = hexer_utils:prompt("Email: ", "~s"),
  case hexer_utils:prompt("Password: ", password) of
    "" -> throw(empty_password);
    Password ->
      case hexer_utils:prompt("Password (confirm): ", password) of
        Password -> register_user(Username, Email, Password);
        _ -> throw(password_mismatch)
      end
  end.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

-spec generate_key(string(), string()) -> ok | {error, any()}.
generate_key(Username, Password) ->
  hexer_utils:print("Generating API key..."),
  {ok, Name} = inet:gethostname(),
  case hexer_server:new_api_key(list_to_binary(Name), Username, Password) of
    {ok, Key} ->
      hexer_config:update(username, Username),
      hexer_config:update(key, binary_to_list(Key));
    {error, Error} ->
      hexer_utils:error("Generation of API key failed (~p)", [Error]),
      {error, Error}
  end.

-spec register_user(string(), string(), string()) -> ok | {error, any()}.
register_user(Username, Email, Password) ->
  hexer_utils:print("Registering..."),
  case hexer_server:create_user(Username, Email, Password) of
    ok ->
      ok = generate_key(Username, Password),
      hexer_utils:print("You are required to confirm your email to access "
                        "your account, a confirmation email has been sent "
                        "to ~s", [Email]);
    {error, Error} ->
      hexer_utils:error( "Registration of user ~s failed (~p)"
                       , [Username, Error]
                       ),
      {error, Error}
  end.

-module(hexer_utils).

-compile({no_auto_import, [error/1]}).

-export([ prompt/2
        , print/1
        , print/2
        , error/1
        ]).

-type format() :: string | integer | password.

-spec prompt(string(), format()) -> any().
prompt(Message, password) ->
  case io:setopts([binary, {echo, false}]) of
    ok ->
      {ok, Password} = io:fread(Message, string),
      Password;
    _ ->
      erlang:error(unsupported_shell)
    end;
prompt(Message, Format) ->
  {ok, Result} = io:fread(Message, Format),
  Result.

-spec print(string()) -> ok.
print(Message) -> print(Message, []).

-spec print(string(), [any()]) -> ok.
print(Message, Args) -> io:format(Message, Args).

-spec error(string()) -> ok.
error(Message) -> error_logger:error_msg(Message).

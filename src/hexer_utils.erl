-module(hexer_utils).

-compile({no_auto_import, [error/1, error/2]}).

-export([ prompt/2
        , print/1
        , print/2
        , error/1
        , error/2
        ]).

-define(OP_PUTC, 0).

-spec prompt(string(), string() | password) -> string().
prompt(Message, password) ->
  case io:setopts([binary, {echo, false}]) of
    ok ->
      {ok, [Password]} = io:fread(Message, "~s"),
      ok = io:setopts([binary, {echo, true}]),
      print(""),
      Password;
    _ ->
      MessageBin = list_to_binary(Message),
      error_logger:tty(false),
      Port = open_port({spawn, "tty_sl -e"}, [binary, eof]),
      port_command(Port, <<?OP_PUTC, MessageBin/binary>>),
      receive
        {Port, {data, PasswordLine}} ->
          [Password | _] = binary:split(PasswordLine, <<"\n">>),
          port_command(Port, <<?OP_PUTC, $\n>>),
          port_close(Port),
          error_logger:tty(true),
          binary_to_list(Password)
      end
  end;
prompt(Message, Format) ->
  {ok, [Result]} = io:fread(Message, Format),
  Result.

-spec print(string()) -> ok.
print(Message) -> print(Message, []).

-spec print(string(), [any()]) -> ok.
print(Message, Args) -> io:format(Message ++ "~n", Args).

-spec error(any()) -> ok.
error(Error) -> error("~p", [Error]).

-spec error(string(), [any()]) -> ok.
error(Message, Args) ->
  io:format("Error: " ++ Message ++ "~n", Args).

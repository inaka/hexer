-module(hexer_utils).

-compile({no_auto_import, [error/1, error/2]}).

-export([ prompt/2
        , print/1
        , print/2
        , error/1
        , error/2
        ]).

-export([ find_single_file/1
        , find_all/2
        , create_tar/4
        ]).

-define(OP_PUTC, 0).

%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------

-spec prompt(string(), string() | password | boolean) ->
  string() | boolean().
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
prompt(Message, boolean) ->
  Answer = io:get_line(Message ++ " [Y/n]: "),
  AnswerStripped = string:strip(Answer, both, $\n),
  case string:to_lower(AnswerStripped) of
    "yes" -> true;
    "y"   -> true;
    ""    -> true;
    "no"  -> false;
    "n"   -> false;
    _     ->
      error("Invalid answer"),
      prompt(Message, boolean)
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

-spec create_tar(atom(), string(), map(), [any()]) ->
  {ok, binary()} | {error, any()}.
create_tar(Name, Version, Meta, Files) ->
  ContentsPath = io_lib:format("~s-~s-contents.tar.gz", [Name, Version]),
  Path = io_lib:format("~s-~s.tar", [Name, Version]),
  ok = erl_tar:create(ContentsPath, Files, [compressed]),

  {ok, Contents} = file:read_file(ContentsPath),
  MetaString = encode_term(Meta),

  Blob = <<MetaString/binary, Contents/binary>>,
  <<X:256/big-unsigned-integer>> = crypto:hash(sha256, Blob),
  Checksum = string:to_upper(lists:flatten(io_lib:format("~64.16.0b", [X]))),

  MetaFiles = [
               {"CHECKSUM", list_to_binary(Checksum)},
               {"metadata.config", MetaString},
               {"contents.tar.gz", Contents}
              ],

  ok = erl_tar:create(Path, MetaFiles),
  Tar = file:read_file(Path),
  ok = file:delete(ContentsPath),
  ok = file:delete(Path),
  Tar.

-spec find_single_file([string()]) -> {ok, string()} | notfound.
find_single_file([]) ->
  notfound;
find_single_file([Pattern | Patterns]) ->
  case filelib:wildcard(Pattern) of
    [] -> find_single_file(Patterns);
    [File | _] -> {ok, File}
  end.

-spec find_all([string()], string()) -> [{string(), string()}].
find_all(Paths, Dir) ->
  Dir1 = case Dir of "." -> ""; _ -> Dir end,
  AbsDir = filename:absname(Dir1),
  Files  = lists:flatmap(fun dir_files1/1,
                         [filename:join(Dir, P) || P <- Paths]
                        ),
  [{F1 -- (AbsDir ++ "/"), F1} || F1 <- filter_regular(Files)].

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec encode_term(map()) -> binary().
encode_term(Meta) ->
  Fun = fun(MetaPair) ->
            Opts = [{encoding, utf8}],
            String = io_lib_pretty:print(binarify(MetaPair), Opts),
            unicode:characters_to_binary([String, ".\n"])
        end,
  Data = lists:map(Fun, maps:to_list(Meta)),
  iolist_to_binary(Data).

-spec binarify(any()) -> boolean() | binary() | [any()] | {any(), any()}.
binarify(Term) when is_boolean(Term) ->
    Term;
binarify(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
binarify([]) ->
    [];
binarify(Term) when is_list(Term) ->
    case io_lib:printable_list(Term) of
        true ->
            list_to_binary(Term);
        false ->
            [binarify(X) || X <- Term]
    end;
binarify({Key, Value}) ->
    {binarify(Key), binarify(Value)};
binarify(Term) ->
    Term.

-spec dir_files1(string()) -> [string()].
dir_files1(Dir) ->
  lists:flatmap(fun(Y) -> dir_files(Y) end, filelib:wildcard(Dir)).

-spec filter_regular([string()]) -> [string()].
filter_regular(Files) ->
  lists:filter(fun filelib:is_regular/1,
               [filename:absname(F) || F <- Files]
              ).

-spec dir_files(string()) -> [string()].
dir_files(Path) ->
  case filelib:is_dir(Path) of
    true ->
      filelib:wildcard(filename:join(Path, "**"));
    false ->
      [Path]
  end.

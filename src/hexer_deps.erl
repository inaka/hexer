-module(hexer_deps).

-export([resolve/1, new_requeriment_meta/2]).

-type dep() :: { Name :: atom(), Version :: string()}.
-type requirement() :: { DepName :: atom(), Data :: [{binary(), any()}]}.

-export_type([requirement/0]).

-spec resolve(string()) -> [requirement()].
resolve(AppDir) ->
  HexerMk = filename:join([AppDir, "hexer.mk"]),
  try
    create_hexer_mk(HexerMk),
    Output = hexer_utils:cmd("cd " ++ AppDir ++ "; make -f hexer.mk list-deps"),
    _ = hexer_utils:cmd("make deps"),
    DepStrings =
      [ string:tokens(DepLine, [$=])
      || DepLine <- string:tokens(Output, [$|, $\n])],
    Deps = [create_dep_vsn_tuple(Dep, Fetch) || [Dep, Fetch] <- DepStrings],
    ok = validate_hex_dependencies(Deps),
    DepDirList = get_deps_dirs(AppDir),
    [new_requeriment_meta(DepTuple, DepDirList) || DepTuple <- Deps]
  after
    _ = file:delete(HexerMk)
  end.

create_hexer_mk(HexerMk) ->
  Contents =
    "include Makefile\n"
    "\n"
    "THE_DEPS = "
      "$(foreach dep,"
        "$(filter-out $(IGNORE_DEPS),$(DEPS)),"
        "$(dep) = $(dep_$(dep))\\|)\n"
    "\n"
    "list-deps:\n"
    "\t@echo $(THE_DEPS)\n",

  ok = file:write_file(HexerMk, Contents).

-spec create_dep_vsn_tuple(string(), string()) ->
  dep() | {no_hex_dependency, string()}.
create_dep_vsn_tuple(Dep, Fetch) ->
  case string:tokens(Fetch, [$\s, $\t]) of
    ["hex", Version | _] -> {to_dep_atom(Dep), Version};
    NonHexVersion        -> {no_hex_dependency, string:join(NonHexVersion, " ")}
  end.

to_dep_atom(Dep) ->
  list_to_atom([Char || Char <- Dep, Char /= $\s]).

-spec validate_hex_dependencies([tuple()]) -> ok | {error, any()}.
validate_hex_dependencies(Deps) ->
  case [{no_hex_dependency, Dep} || {no_hex_dependency, Dep} <- Deps] of
    []    -> ok;
    NoHexDeps ->
      FormatedDeps = hexer_utils:format_deps(NoHexDeps),
      Description  = "Dependencies not published in hex.pm:~n  ~s",
      hexer_utils:error(Description, [FormatedDeps]),
      throw({hexer_deps, no_hex_dependency})
  end.

-spec new_requeriment_meta(dep(), list()) -> requirement().
new_requeriment_meta({DepName, Version}, DepDirList) ->
  FunMatch = fun(X) -> filename:basename(X) == atom_to_list(DepName) end,
  [NoHexDeps] = lists:filter(FunMatch , DepDirList),
  Path = [NoHexDeps, "/src/*app.src"],
  AppSrcPathRaw = hexer_utils:cmd(["ls ", Path]),
  AppSrcPath = string:tokens(AppSrcPathRaw, [$|, $\n]),
  { DepName,
    %It's probably that DepName =/= AppName in app.src
    [ {<<"app">> , create_requeriment_meta(AppSrcPath)}
    , {<<"requirement">>, Version}
    , {<<"optional">>, false}
    ]}.

create_requeriment_meta(AppSrcPath) ->
  {ok, [MetaData]} = file:consult(AppSrcPath),
  { application, ApplicationName , _} = MetaData,
  ApplicationName.

get_deps_dirs(AppDir) ->
  PathMk = filename:join([AppDir, "allDepsDir.mk"]),
  try
    create_path_mk(PathMk),
    Commands = ["cd ", AppDir, "; make -f allDepsDir.mk list-all-deps-dirs"],
    Output = hexer_utils:cmd(Commands),
    string:tokens(Output, [$\n, $\s, $\t])
  after
    _ = file:delete(PathMk)
  end.

create_path_mk(PathMk) ->
  Contents =
    "include Makefile\n"
    "\n"
    "list-all-deps-dirs:\n"
    "\t@echo $(ALL_DEPS_DIRS)\n",
  ok = file:write_file(PathMk, Contents).



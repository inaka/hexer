-module(hexer_deps).

-export([resolve/1]).

-type dep() :: { Name :: atom(), Version :: string()}.
-type requirement() :: { DepName :: binary(), Data :: [{binary(), any()}]}.

-export_type([requirement/0]).

-spec resolve(string()) -> [requirement()].
resolve(AppDir) ->
  HexerMk = filename:join([AppDir, "hexer.mk"]),
  try
    create_hexer_mk(HexerMk),
    Output = hexer_utils:cmd("cd " ++ AppDir ++ "; make -f hexer.mk list-deps"),
    _ = hexer_utils:cmd("cd " ++ AppDir ++ "; make deps"),
    DepStrings =
      [ string:tokens(DepLine, [$=])
      || DepLine <- string:tokens(Output, [$|, $\n])],
    Deps = [create_dep_vsn_tuple(Dep, Fetch) || [Dep, Fetch] <- DepStrings],
    ok = validate_hex_dependencies(Deps),
    [new_requeriment_meta(DepTuple) || DepTuple <- Deps]
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
      throw({hexer_package, no_hex_dependency})
  end.

-spec new_requeriment_meta(dep()) -> requirement().
new_requeriment_meta({DepName, Version}) ->
  { DepName, 
    %It's probably that DepName =/= AppName in app.src
    [ {<<"app">> , create_requeriment_meta(DepName)} 
    , {<<"requirement">>, Version}
    , {<<"optional">>, false}
    ]}.

create_requeriment_meta(PackageName) ->
 MetaData = file:consult(get_src_path(PackageName)),
 { application, ApplicationName , _} = MetaData,
 ApplicationName.

get_src_path(Application) ->
  "deps/" ++ Application ++ "/src/" ++ Application ++ "app.src".

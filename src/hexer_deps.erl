-module(hexer_deps).

-export([resolve/1]).

-type dep() :: { Name :: atom(), Version :: string()}.

-export_type([dep/0]).

-spec resolve(string()) -> [dep()].
resolve(AppDir) ->
  HexerMk = filename:join([AppDir, "hexer.mk"]),
  try
    create_hexer_mk(HexerMk),
    Output = os:cmd("cd " ++ AppDir ++ "; make -f hexer.mk list-deps"),
    DepStrings =
      [ string:tokens(DepLine, [$=])
      || DepLine <- string:tokens(Output, [$|, $\n])],
    [create_dep_vsn_tuple(Dep, Fetch) || [Dep, Fetch] <- DepStrings]
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

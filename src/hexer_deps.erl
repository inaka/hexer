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
    [ {to_dep_atom(Dep), hex_version(Fetch)}
    || [Dep, Fetch] <- DepStrings, is_hex_dep(Fetch)]
  after
    _ = file:delete(HexerMk)
  end.

create_hexer_mk(HexerMk) ->
  Contents =
    "include Makefile\n"
    "\n"
    "THE_DEPS = "
      "$(foreach dep,"
        "$(filter-out $(IGNORE_DEPS),$(BUILD_DEPS) $(DEPS)),"
        "$(dep) = $(dep_$(dep))\\|)\n"
    "\n"
    "list-deps:\n"
    "\t@echo $(THE_DEPS)\n",
  ok = file:write_file(HexerMk, Contents).

is_hex_dep(Fetch) ->
  case string:tokens(Fetch, [$\s, $\t]) of
    ["hex" | _] -> true;
    _ -> false
  end.

hex_version(Fetch) ->
  ["hex", Version | _] = string:tokens(Fetch, [$\s, $\t]),
  Version.

to_dep_atom(Dep) ->
  list_to_atom([Char || Char <- Dep, Char /= $\s]).

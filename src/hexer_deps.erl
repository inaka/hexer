-module(hexer_deps).

-export([resolve/1]).

-type dep() :: { Name :: atom(), Version :: string()}.

-export_type([dep/0]).

-spec resolve(string()) -> [dep()].
resolve(AppDir) ->
  {ok, Makefile} = file:read_file(filename:join([AppDir, "Makefile"])),
  Regex = "dep_(\\w+) *?= *?hex +([\\w\\d\\.]+)",
  RegexOpts = [{capture, all_but_first, list}, global, multiline],
  case re:run(Makefile, Regex, RegexOpts) of
    {match, Deps} ->
      [{list_to_atom(Name), Version} || [Name, Version] <- Deps];
    nomatch -> []
  end.

-module(hexer_package).

-export([publish/0]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec publish() -> ok | {error, any()}.
publish() ->
  hexer_utils:print("Publishing..."),
  %% We need the following information to build the package:
  %% - App Directory
  %% - Name
  %% - Version
  %% - AppDetails from *.app.src
  %% - Deps

  %% We assume the app's dir is the current directory.
  AppDir = ".",
  #{ name    := Name
   , version := Version
   , details := Details
   } = hexer_utils:load_app_info(),
  Deps = hexer_deps:resolve(AppDir),
  ok = validate_app_details(Details),
  ok = validate_hex_dependencies(Deps),
  publish(AppDir, Name, Version, Deps, Details).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-spec validate_hex_dependencies([tuple()]) -> ok | {error, any()}.
validate_hex_dependencies(Deps) ->
  case [{no_hex_dependency, Dep} || {no_hex_dependency, Dep} <- Deps] of
    []    -> ok;
    NoHexDeps ->
      FormatedDeps = format_deps(NoHexDeps),
      Description  = "Error: No Hex Dependencys in Makefile:~n  ~s",
      hexer_utils:error(Description, [FormatedDeps]),
      throw({hexer_package, no_hex_dependency})
  end.

-spec validate_app_details(map()) -> ok | {error, any()}.
validate_app_details(Details) ->
  case maps:is_key(contributors, Details) of
    true ->
       throw({hexer_package, has_contributors});
    false ->
      ok
  end.

-spec publish( string()
             , atom()
             , string()
             , [hexer_deps:dep()]
             , hexer_utils:details()
             ) -> ok | {error, any()}.
publish(AppDir, Name, Version, Deps, Details) ->
  Description = list_to_binary(maps:get(description, Details, "")),
  FilePaths   = maps:get(files, Details, default_files()),
  Files       = hexer_utils:find_all(FilePaths, AppDir),
  Filenames   = [list_to_binary(F) || {F, _} <- Files],
  Maintainers = maps:get(maintainers, Details, []),
  Licenses    = maps:get(licenses, Details, []),
  Links       = maps:get(links, Details, []),
  PackageName = maps:get(pkg_name, Details, Name),
  BuildTools  = maps:get(build_tools, Details, []),

  Optional = #{ app          => Name
              , requirements => Deps
              , maintainers  => Maintainers
              , precompiled  => false
              , parameters   => []
              , description  => Description
              , files        => Filenames
              , licenses     => Licenses
              , links        => Links
              , build_tools  => BuildTools
              },
  EmptyValue = fun(_, "") -> false; (_, _) -> true end,
  OptionalFiltered = maps:filter(EmptyValue, Optional),
  Mandatory = #{name => PackageName, version => Version},
  Meta = maps:merge(Mandatory, OptionalFiltered),

  {ok, APIKey} = hexer_config:api_key(),
  hexer_utils:print("Publishing ~s ~s", [PackageName, Version]),
  hexer_utils:print("  Dependencies:~n  ~s", [format_deps(Deps)]),
  hexer_utils:print("  Included files:", []),
  lists:foreach(
    fun(Filename) ->
      hexer_utils:print("    ~s", [Filename])
    end, Filenames),
  hexer_utils:print( "Before publishing, please read Hex CoC: "
                     "https://hex.pm/docs/codeofconduct"
                   , []
                   ),
  case hexer_utils:prompt("Proceed?", boolean) of
    true ->
      upload_package(APIKey, Name, Version, Meta, Files);
    _ ->
      hexer_utils:print("Goodbye...")
  end.

-spec upload_package(
  string(), atom(), string(), map(), [{string(), string()}]) ->
  ok | {error, any()}.
upload_package(APIKey, Name, Version, Meta, Files) ->
  {ok, Tar} = hexer_utils:create_tar(Name, Version, Meta, Files),
  case hexer_server:publish_package(APIKey, atom_to_list(Name), Tar) of
    ok -> hexer_utils:print("Published ~s ~s", [Name, Version]);
    {error, Error} -> throw(Error)
  end.

-spec default_files() -> [string()].
default_files() ->
  [ "src", "c_src", "include", "priv"
  , "rebar.config.script", "rebar.config", "rebar.lock"
  , "Makefile", "Emakefile", "erlang.mk"
  , "README*", "readme*"
  , "LICENSE*", "license*"
  ].

-spec format_deps([hexer_deps:dep()]) -> string().
format_deps(Deps) ->
  DepsStr = [atom_to_list(Name) ++ " " ++ Version || {Name, Version} <- Deps],
  string:join(DepsStr, "\n  ").

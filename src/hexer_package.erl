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
   } = load_app_info(),
  Deps = hexer_deps:resolve(AppDir),
  case validate_app_details(Details) of
    ok -> publish(AppDir, Name, Version, Deps, Details);
    {error, Error} -> throw(Error)
  end.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

-type details() :: #{ description  => string()
                    , applications => [atom()]
                    , maintainers  => [string()]
                    , licenses     => [string()]
                    , files        => [string()] %% Files to be included
                    , links        => [{Label :: string(), Label :: string()}]
                    }.

-type app_info() :: #{ name    => atom()
                     , version => string()
                     , details => details()
                     }.

-spec load_app_info() -> app_info().
load_app_info() ->
  case hexer_utils:find_single_file(["ebin/*.app", "src/*.app.src"]) of
    {ok, Path} ->
      {ok, [AppSrc]} = file:consult(Path),
      {application, Name, Details} = AppSrc,
      DetailsMap = maps:from_list(Details),
      #{ name    => Name
       , version => maps:get(vsn, DetailsMap)
       , details => maps:remove(vsn, DetailsMap)
       };
    notfound ->
      throw(app_file_not_found)
  end.

-spec validate_app_details(map()) -> ok | {error, any()}.
validate_app_details(Details) ->
  case maps:is_key(contributors, Details) of
    true ->
      {error, {hexer_package, has_contributors}};
    false ->
      ok
  end.

-spec publish(string(), atom(), string(), [hexer_deps:dep()], details()) ->
  ok | {error, any()}.
publish(AppDir, Name, Version, Deps, Details) ->
  Description = list_to_binary(maps:get(description, Details, "")),
  FilePaths   = maps:get(files, Details, default_files()),
  Files       = hexer_utils:find_all(FilePaths, AppDir),
  Filenames   = [F || {_, F} <- Files],

  Maintainers = maps:get(maintainers, Details, []),
  Licenses    = maps:get(licenses, Details, []),
  Links       = maps:get(links, Details, []),
  PackageName = maps:get(pkg_name, Details, Name),

  Optional = #{ app          => PackageName
              , requirements => Deps
              , maintainers  => Maintainers
              , precompiled  => false
              , parameters   => []
              , description  => Description
              , files        => Files
              , licenses     => Licenses
              , links        => Links
              , build_tools  => [<<"make">>]
              },
  EmptyValue = fun(_, "") -> false; (_, _) -> true end,
  OptionalFiltered = maps:filter(EmptyValue, Optional),
  Mandatory = #{name => Name, version => Version},
  Meta = maps:merge(Mandatory, OptionalFiltered),

  {ok, APIKey} = hexer_config:api_key(),
  hexer_utils:print("Publishing ~s ~s", [Name, Version]),
  hexer_utils:print("  Dependencies:~n    ~s", [format_deps(Deps)]),
  hexer_utils:print("  Included files:~n    ~s"
                   , [string:join(Filenames, "\n    ")]
                   ),
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

-spec upload_package(string(), atom(), string(), map(), [any()]) ->
  ok | {error, any()}.
upload_package(APIKey, Name, Version, Meta, Files) ->
  {ok, Tar} = hexer_utils:create_tar(Name, Version, Meta, Files),
  case hexer_server:publish(APIKey, atom_to_list(Name), Tar) of
    ok -> hexer_utils:print("Published ~s ~s", [Name, Version]);
    {error, Error} -> throw(Error)
  end.

-spec default_files() -> [string()].
default_files() ->
  [ "src", "c_src", "include", "priv"
  , "rebar.config.script", "rebar.config", "rebar.lock"
  , "Makefile", "erlang.mk"
  , "README*", "readme*"
  , "LICENSE*", "license*"
  ].

-spec format_deps([hexer_deps:dep()]) -> string().
format_deps(Deps) ->
  DepsStr = [atom_to_list(Name) ++ " " ++ Version || {Name, Version} <- Deps],
  string:join(DepsStr, "\n    ").

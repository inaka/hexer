-module(hexer_docs).

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
   } = load_app_info(),
  publish(AppDir, Name, Version).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-type details() :: #{ description  => string()
                    , applications => [atom()]
                    , maintainers  => [string()]
                    , licenses     => [string()]
                    , files        => [binary()] %% Files to be included
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
      VSN = maps:get(vsn, DetailsMap),
      Version = transform_vsn_git_to_tag(VSN),
      #{ name    => Name
       , version => Version
       };
    notfound ->
      throw(app_file_not_found)
  end.

-spec transform_vsn_git_to_tag(any()) -> any().
transform_vsn_git_to_tag(git) ->
  GetGitTag = hexer_utils:cmd("git describe --abbrev=0 --tags"),
  case GetGitTag of
    "fatal: " ++ Reason ->
      throw({hexer_docs, {bad_github_tag, Reason}});
    TagOK ->
      TagOK
  end;
transform_vsn_git_to_tag(Value) -> Value.

-spec publish(string(), atom(), string()) ->
  ok | {error, any()}.
publish(AppDir, Name, Version) ->
  Files       = hexer_utils:find_all(["doc"], AppDir),
  Filenames   = [list_to_binary(F) || {F, _} <- Files],
  {ok, APIKey} = hexer_config:api_key(),
  hexer_utils:print("Publishing Docs ~s", [Version]),
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
      ok = upload_doc(APIKey, Name, Version, Files);
    _ ->
      hexer_utils:print("Goodbye...")
  end.

-spec upload_doc(
  string(), atom(), string(), [{string(), string()}]) ->
  ok | {error, any()}.
upload_doc(_APIKey, _Name, _Version, []) ->
  throw({hexer_docs, no_files_to_upload});
upload_doc(APIKey, Name, Version, Files) ->
  Tarball = atom_to_list(Name) ++ "-" ++ Version ++ "-docs.tar.gz",
  Filenames = [ F || {F, _} <- Files],
  ok = erl_tar:create(Tarball, Filenames, [compressed]),
  {ok, Tar} = file:read_file(Tarball),
  ok = file:delete(Tarball),
  case hexer_server:publish_docs(APIKey, atom_to_list(Name), Version, Tar) of
    ok -> hexer_utils:print("Published Docs ~s ~s", [Name, Version]);
    {error, Error} -> throw(Error)
  end.

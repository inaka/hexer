%%% @doc Doc publishing utilities
-module(hexer_docs).

-export([publish/0]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------
%% @doc publishes the app documentation.
%%      doc/ folder should already exist.
-spec publish() -> ok | {error, any()}.
publish() ->
  hexer_utils:print("Publishing..."),
  %% We need the following information to upload the docs:
  %% - Name
  %% - Version

  %% We assume the app's dir is the current directory.
  AppDir = ".",
  #{ name    := Name
   , version := Version
   } = hexer_utils:load_app_info(),
  publish(AppDir, Name, Version).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-spec publish(string(), atom(), string()) ->
  ok | {error, any()}.
publish(AppDir, Name, Version) ->
  Files       = hexer_utils:find_all(["doc"], AppDir),
  Filenames   = [list_to_binary(F) || {F, _} <- Files],
  {ok, APIKey} = hexer_config:api_key(),
  hexer_utils:print("Publishing Docs for ~p ~s", [Name, Version]),
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
  FilePaths = [{filename:basename(File), AbsPath} || {File, AbsPath} <- Files],
  ok = erl_tar:create(Tarball, FilePaths, [compressed]),
  {ok, Tar} = file:read_file(Tarball),
  ok = file:delete(Tarball),
  case hexer_server:publish_docs(APIKey, atom_to_list(Name), Version, Tar) of
    ok -> hexer_utils:print("Published Docs ~s ~s", [Name, Version]);
    {error, Error} -> throw(Error)
  end.


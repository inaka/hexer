-module(hexer_docs_SUITE).

-include_lib("inaka_mixer/include/mixer.hrl").
-mixin([{ hexer_test_utils
        , [ init_per_suite/1
          , end_per_suite/1
          ]
        }
       ]).

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ non_api_key_found/1
        , bad_github_tag/1
        , no_files_to_upload/1
        , publish_succeeds/1
        , server_error/1
        , shotgun_error/1
        , not_published/1
        ]).

-import( hexer_test_utils
       , [ app_directory/0
         , run_in_dir/2
         , app_src_path/0
         , create_app_src/1
         ]).

-spec all() -> [atom()].
all() -> hexer_test_utils:all(?MODULE).

init_per_testcase(_, Config) ->
  meck:new(hexer_utils, [passthrough]),
  meck:new(shotgun, [passthrough]),
  _ = file:make_dir("src"),
  Config.

end_per_testcase(_, Config) ->
  meck:unload(hexer_utils),
  meck:unload(shotgun),
  _ = file:del_dir("src"),
  run_in_dir(app_directory(), fun hexer_test_utils:delete_api_key/0),
  Config.

-spec non_api_key_found(hexer_test_utils:config()) -> {comment, string()}.
non_api_key_found(_Config) ->

  ct:comment("Create dummy files"),
  AppSrcBin = <<"{application, my_app, [{vsn, \"0.0.1\"}]}.">>,
  create_app_src(AppSrcBin),
  ok = file:write_file("Makefile", <<>>),

  ct:comment("Error: No API key"),
  ok = try ok = hexer_docs:publish(), error
       catch _:no_api_key -> ok
       end,
  {comment, ""}.

-spec bad_github_tag(hexer_test_utils:config()) -> {comment, string()}.
bad_github_tag(_Config) ->
  ct:comment("Error: Bad Github tag"),
  AppSrcBin3 = <<"{ application, my_app "
                 "  , [ {vsn, git} ]"
                 "}.">>,
  create_app_src(AppSrcBin3),
  ErrorTag = "No Tag!!",
  OsFalseFun = fun(_) ->  "fatal: " ++ ErrorTag end,
  meck:expect(hexer_utils, cmd, OsFalseFun),
  ok = try ok = hexer_docs:publish(), error
       catch _:{hexer_utils, {bad_github_tag, ErrorTag}} -> ok
       end,
  {comment, ""}.

-spec not_published(hexer_test_utils:config()) -> {comment, string()}.
not_published(_Config) ->
  ok = run_in_dir(app_directory(), fun hexer_test_utils:generate_api_key/0),
  PromptMockFalseFun = fun(_, _) -> false end,
  meck:expect(hexer_utils, prompt, PromptMockFalseFun),
  hexer_test_utils:mock_shotgun_ok(),
  ct:comment("Don't proceed publishing"),
  ok = run_in_dir(app_directory(), fun hexer_docs:publish/0),
  timeout = hexer_test_utils:wait_receive(publish, 500),
  {comment, ""}.

-spec no_files_to_upload(hexer_test_utils:config()) -> {comment, string()}.
no_files_to_upload(_Config) ->
  ct:comment("Error: app_file_not_found error"),
   ok = hexer_test_utils:generate_api_key(),
  run_in_dir(app_directory(), fun delete_docs/0),
  PromptMockTrueFun = fun(_, _) -> true end,
  meck:expect(hexer_utils, prompt, PromptMockTrueFun),
  hexer_test_utils:mock_shotgun_ok(),
  ok = try ok = hexer_docs:publish(), error
       catch _:{hexer_docs, no_files_to_upload} -> ok
       end,
  {comment, ""}.

-spec shotgun_error(hexer_test_utils:config()) -> {comment, string()}.
shotgun_error(_Config) ->
  ct:comment("Error: shotgun error"),
  ok = run_in_dir(app_directory(), fun hexer_test_utils:generate_api_key/0),
  run_in_dir(app_directory(), fun create_docs/0),
  PromptMockTrueFun = fun(_, _) -> true end,
  meck:expect(hexer_utils, prompt, PromptMockTrueFun),
  PostErrorFun = fun(_, _, _, _, _) -> {error, unexpected} end,
  meck:expect(shotgun, post, PostErrorFun),
  ok = try ok = run_in_dir(app_directory(), fun hexer_docs:publish/0), error
       catch _:unexpected -> ok
       end,
  {comment, ""}.

-spec server_error(hexer_test_utils:config()) -> {comment, string()}.
server_error(_Config) ->
  ok = run_in_dir(app_directory(), fun hexer_test_utils:generate_api_key/0),
  run_in_dir(app_directory(), fun create_docs/0),
  PromptMockTrueFun = fun(_, _) -> true end,
  meck:expect(hexer_utils, prompt, PromptMockTrueFun),
  ct:comment("Error: server error when publishing"),
  ResponseError = #{status_code => 500, body => term_to_binary({})},
  Post500Fun = fun(_, _, _, _, _) -> {ok, ResponseError} end,
  meck:expect(shotgun, post, Post500Fun),
  ok = try ok = run_in_dir(app_directory(), fun hexer_docs:publish/0), error
       catch _:{500, {}} -> ok
       end,
  {comment, ""}.

-spec publish_succeeds(hexer_test_utils:config()) -> {comment, string()}.
publish_succeeds(_Config) ->
  ok = run_in_dir(app_directory(), fun hexer_test_utils:generate_api_key/0),
  PromptMockTrueFun = fun(_, _) -> true end,
  meck:expect(hexer_utils, prompt, PromptMockTrueFun),
  hexer_test_utils:mock_shotgun_ok(),
  ct:comment("Publishing succeeds"),
  ok = run_in_dir(app_directory(), fun hexer_docs:publish/0),
  ok = hexer_test_utils:wait_receive(publish, 500),

  {comment, ""}.

create_docs() ->
  Output = hexer_utils:cmd("make edoc"),
  ct:pal("~s~n", [Output]).

delete_docs() ->
  Output = hexer_utils:cmd("rm -rf docs/"),
  ct:pal("~s~n", [Output]).


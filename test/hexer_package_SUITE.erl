-module(hexer_package_SUITE).

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

-export([ publish/1
        ]).

-spec all() -> [atom()].
all() -> hexer_test_utils:all(?MODULE).

init_per_testcase(_, Config) ->
  meck:new(hexer_utils, [passthrough]),
  meck:new(shotgun, [passthrough]),
  Config.

end_per_testcase(_, Config) ->
  meck:unload(hexer_utils),
  meck:unload(shotgun),
  Config.

-spec publish(hexer_test_utils:config()) -> {comment, string()}.
publish(_Config) ->
  Self = self(),
  AppDir = "../../",
  ok = run_in_dir(AppDir, fun generate_api_key/0),

  PromptTrueFun = fun(_, _) -> true end,
  meck:expect(hexer_utils, prompt, PromptTrueFun),

  Response = #{status_code => 200},
  PostFun = fun(_, _, _, _, _) -> Self ! publish, {ok, Response} end,
  OpenFun = fun(_, _, _) -> {ok, self()} end,
  CloseFun = fun(_) -> ok end,
  meck:expect(shotgun, open, OpenFun),
  meck:expect(shotgun, post, PostFun),
  meck:expect(shotgun, close, CloseFun),

  ct:comment("Publishing succeeds"),
  ok = run_in_dir(AppDir, fun hexer_package:publish/0),
  ok = hexer_test_utils:wait_receive(publish, 500),

  ct:comment("Don't proceed publishing"),
  PromptFalseFun = fun(_, _) -> false end,
  meck:expect(hexer_utils, prompt, PromptFalseFun),
  ok = run_in_dir(AppDir, fun hexer_package:publish/0),
  timeout = hexer_test_utils:wait_receive(publish, 500),

  ct:comment("Error: App file not found"),
  ok = try ok = hexer_package:publish(), error
       catch _:app_file_not_found -> ok
       end,

  ct:comment("Create dummy files"),
  ok = file:make_dir("src"),
  AppSrcBin = <<"{application, hexer, [{vsn, \"0.0.1\"}]}.">>,
  HexerAppSrcFileName = "src/hexer.app.src",
  ok = file:write_file(HexerAppSrcFileName, AppSrcBin),
  ok = file:write_file("Makefile", <<>>),

  ct:comment("Error: No API key"),
  ok = try ok = hexer_package:publish(), error
       catch _:no_api_key -> ok
       end,

  ct:comment("Error: has contributors"),
  AppSrcBin2 = <<"{ application, hexer "
                 "  , [ {vsn, \"0.0.1\"} "
                 "    , {contributors, []}"
                 "    ]"
                 "}.">>,
  ok = file:write_file(HexerAppSrcFileName, AppSrcBin2),
  ok = try ok = hexer_package:publish(), error
       catch _:{hexer_package, has_contributors} -> ok
       end,
  ok = file:delete(HexerAppSrcFileName),

  ct:comment("Error: Bag Github tag"),
  AppSrcBin3 = <<"{ application, my_app "
                 "  , [ {vsn, git} ]"
                 "}.">>,
  ok = file:write_file("src/my_app.app.src", AppSrcBin3),
  OsFalseFun = fun(_) ->  "fatal: " ++ (ErrorTag = "No Tag!!") end,
  meck:expect(hexer_utils, cmd, OsFalseFun),
  ok = try ok = hexer_package:publish(), error
       catch _:{hexer_package, {bad_github_tag, ErrorTag}} -> ok
       end,


  meck:expect(hexer_utils, prompt, PromptTrueFun),

  ct:comment("Error: server error when publishing"),
  ResponseError = #{status_code => 500, body => term_to_binary({})},
  Post500Fun = fun(_, _, _, _, _) -> {ok, ResponseError} end,
  meck:expect(shotgun, post, Post500Fun),
  ok = try ok = run_in_dir(AppDir, fun hexer_package:publish/0), error
       catch _:{500, {}} -> ok
       end,

  ct:comment("Error: shotgun error"),
  PostErrorFun = fun(_, _, _, _, _) -> {error, unexpected} end,
  meck:expect(shotgun, post, PostErrorFun),
  ok = try ok = run_in_dir(AppDir, fun hexer_package:publish/0), error
       catch _:unexpected -> ok
       end,

  {comment, ""}.

-spec run_in_dir(string(), function()) -> any().
run_in_dir(Dir, Fun) ->
  {ok, Cwd} = file:get_cwd(),
  ok = file:set_cwd(Dir),
  try
    Fun()
  after
    ok = file:set_cwd(Cwd)
  end.

-spec generate_api_key() -> ok.
generate_api_key() ->
  PromptFun = fun(_, _) -> "value" end,
  meck:expect(hexer_utils, prompt, PromptFun),

  Response = #{ status_code => 200
              , body => term_to_binary(#{<<"secret">> => <<"1">>})
              },
  PostFun = fun(_, _, _, _, _) -> {ok, Response} end,
  OpenFun = fun(_, _, _) -> {ok, self()} end,
  CloseFun = fun(_) -> ok end,
  meck:expect(shotgun, open, OpenFun),
  meck:expect(shotgun, post, PostFun),
  meck:expect(shotgun, close, CloseFun),

  ct:comment("Generating API key succeeds"),
  ok = hexer_user:auth(),
  true = filelib:is_regular("hexer.config"),
  ok.

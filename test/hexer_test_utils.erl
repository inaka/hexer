-module(hexer_test_utils).

-export([ all/1
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ check_output/2
        , wait_receive/2
        , generate_api_key/0
        , mock_shotgun_ok/0
        , run_in_dir/2
        , app_directory/0
        , app_src_path/0
        , delete_api_key/0
        , create_app_src/1
        ]).

-type config() :: proplists:proplist().
-export_type([config/0]).

-spec all(atom()) -> [atom()].
all(Module) ->
  ExcludedFuns = [module_info, init_per_suite, end_per_suite],
  [F || {F, 1} <- Module:module_info(exports)] -- ExcludedFuns.

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(hexer),
  Config.

end_per_suite(Config) ->
  ok = application:stop(hexer),
  Config.

-spec check_output(string(), function()) -> boolean().
check_output(Regex, Fun) ->
  ct:capture_start(),
  Fun(),
  ct:capture_stop(),

  Lines    = ct:capture_get(),
  {ok, MP} = re:compile(Regex),
  MatchFun = fun(Line) -> re:run(Line, MP) =/= nomatch end,

  lists:any(MatchFun, Lines).

-spec wait_receive(any(), timeout()) -> ok | timeout.
wait_receive(Value, Timeout) ->
  receive Value -> ok
  after   Timeout -> timeout
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

-spec mock_shotgun_ok() -> ok.
mock_shotgun_ok() ->
  Response = #{status_code => 200},
  PostShotgunMkFun = fun(_, _, _, _, _) -> self() ! publish, {ok, Response} end,
  OpenShotgunMkFun = fun(_, _, _) -> {ok, self()} end,
  CloseShotgunMkFun = fun(_) -> ok end,
  meck:expect(shotgun, post, PostShotgunMkFun),
  meck:expect(shotgun, open, OpenShotgunMkFun),
  meck:expect(shotgun, close, CloseShotgunMkFun).

-spec run_in_dir(string(), function()) -> any().
run_in_dir(Dir, Fun) ->
  {ok, Cwd} = file:get_cwd(),
  ok = file:set_cwd(Dir),
  try
    Fun()
  after
    ok = file:set_cwd(Cwd)
  end.

app_directory() -> "../../".

app_src_path() -> "src/my_app.app.src".

delete_api_key() ->
 _ = hexer_utils:cmd("rm hexer.config").

create_app_src(AppSrcBodyBinary) ->
  _ = hexer_utils:cmd("rm " ++ app_src_path()),
  ok = file:write_file(app_src_path(), AppSrcBodyBinary).
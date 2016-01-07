-module(hexer_test_utils).

-export([ all/1
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ check_output/2
        , wait_receive/2
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

-module(hexer_test_utils).

-export([ all/1
        , init_per_suite/1
        , end_per_suite/1]).


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

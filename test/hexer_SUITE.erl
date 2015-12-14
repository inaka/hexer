-module(hexer_SUITE).

-include_lib("mixer/include/mixer.hrl").
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

-export([ main/1
        ]).

-spec all() -> [atom()].
all() -> hexer_test_utils:all(?MODULE).

init_per_testcase(_, Config) ->
  meck:new(hexer_user, [passthrough]),
  meck:new(hexer_package, [passthrough]),
  Config.

end_per_testcase(_, Config) ->
  meck:unload(hexer_user),
  meck:unload(hexer_package),
  Config.

-spec main(hexer_test_utils:config()) -> {comment, string()}.
main(_Config) ->
  HelpRegex = "Usage: .*",

  ct:comment("No args should show help"),
  NoArgsFun = fun() -> ok = hexer:main([]) end,
  hexer_test_utils:check_output(HelpRegex, NoArgsFun),

  ct:comment("hexer should call the specified commands"),
  Self = self(),
  AuthFun = fun() -> Self ! auth end,
  meck:expect(hexer_user, auth, AuthFun),
  ok = hexer:main(["user.auth"]),
  ok = hexer_test_utils:wait_receive(auth, 500),

  RegisterFun = fun() -> Self ! register end,
  meck:expect(hexer_user, register, RegisterFun),
  ok = hexer:main(["user.register"]),
  ok = hexer_test_utils:wait_receive(register, 500),

  PublishFun = fun() -> Self ! publish end,
  meck:expect(hexer_package, publish, PublishFun),
  ok = hexer:main(["publish"]),
  ok = hexer_test_utils:wait_receive(publish, 500),


  ct:comment("Unexisting command or option should show error & help"),
  OptionArgFun = fun() -> ok = hexer:main(["--something"]) end,
  hexer_test_utils:check_output(HelpRegex, OptionArgFun),

  CommandArgFun = fun() -> ok = hexer:main(["something"]) end,
  hexer_test_utils:check_output(HelpRegex, CommandArgFun),

  {comment, ""}.
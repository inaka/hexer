-module(hexer_users_SUITE).

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

-export([ auth/1
        , register/1
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

-spec auth(hexer_test_utils:config()) -> {comment, string()}.
auth(_Config) ->
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

  ResponseError = #{ status_code => 401
                   , body => term_to_binary(#{<<"status">> => 401})
                   },
  PostErrorFun = fun(_, _, _, _, _) -> {ok, ResponseError} end,
  meck:expect(shotgun, post, PostErrorFun),

  ct:comment("Generating API key fails when status is not 2XX"),
  {error, {401, _}} = hexer_user:auth(),

  PostError2Fun = fun(_, _, _, _, _) -> {error, unexpected} end,
  meck:expect(shotgun, post, PostError2Fun),

  ct:comment("Generating API key fails when error on POST"),
  {error, unexpected} = hexer_user:auth(),

  {comment, ""}.

-spec register(hexer_test_utils:config()) -> {comment, string()}.
register(_Config) ->
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

  ct:comment("Registering a new user succeeds"),
  ok = hexer_user:register(),
  true = filelib:is_regular("hexer.config"),

  ResponseError = #{ status_code => 401
                   , body => term_to_binary(#{<<"status">> => 401})
                   },
  PostErrorFun = fun(_, _, _, _, _) -> {ok, ResponseError} end,
  meck:expect(shotgun, post, PostErrorFun),

  ct:comment("Registering a new user fails when status is not 2XX"),
  {error, {401, _}} = hexer_user:register(),

  PostError2Fun = fun(_, _, _, _, _) -> {error, unexpected} end,
  meck:expect(shotgun, post, PostError2Fun),

  ct:comment("Registering a new user fails when error on POST"),
  {error, unexpected} = hexer_user:register(),

  PromptEmptyFun = fun(_, _) -> "" end,
  meck:expect(hexer_utils, prompt, PromptEmptyFun),
  ok = try ok = hexer_user:register(), error
       catch _:empty_password -> ok
       end,

  PromptPasswordFun = fun
                        ("Password: ", _) -> "a";
                        ("Password (confirm): ", _) -> "b";
                        (_, _) -> "value"
                      end,
  meck:expect(hexer_utils, prompt, PromptPasswordFun),
  ok = try ok = hexer_user:register(), error
       catch _:password_mismatch -> ok
       end,

  {comment, ""}.

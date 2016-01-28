%%% @doc commands for hexer escript
-module(hexer_commands).

-export([ 'user.auth'/0
        , 'user.register'/0
        , 'publish'/0
        , 'publish.docs'/0
        ]).

%% @doc User authentication
-spec 'user.auth'() -> ok.
'user.auth'() -> hexer_user:auth().

%% @doc User registration
-spec 'user.register'() -> ok.
'user.register'() -> hexer_user:register().

%% @doc Doc publishing
-spec 'publish.docs'() -> ok | {error, any()}.
'publish.docs'() -> hexer_docs:publish().

%% @doc Package publishing
-spec publish() -> ok | {error, any()}.
publish() -> hexer_package:publish().

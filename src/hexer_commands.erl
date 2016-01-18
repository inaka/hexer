-module(hexer_commands).

-export([ 'user.auth'/0
        , 'user.register'/0
        , 'publish'/0
        , 'publish.docs'/0
        ]).

-spec 'user.auth'() -> ok.
'user.auth'() -> hexer_user:auth().

-spec 'user.register'() -> ok.
'user.register'() -> hexer_user:register().

-spec 'publish.docs'() -> ok | {error, any()}.
'publish.docs'() -> hexer_docs:publish().

-spec publish() -> ok | {error, any()}.
publish() -> hexer_package:publish().

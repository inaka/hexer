-module(hexer_commands).

-export(['user.auth'/0]).

-spec 'user.auth'() -> ok.
'user.auth'() -> hexer_user:auth().

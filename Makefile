PROJECT = hexer

DEPS = getopt shotgun

dep_getopt  = git https://github.com/jcomellas/getopt       v0.8.2
dep_shotgun = git https://github.com/inaka/shotgun.git      13b6b4

TEST_DEPS = katana mixer

dep_katana = git https://github.com/inaka/erlang-katana.git 0.2.14
dep_mixer  = git https://github.com/inaka/mixer.git         0.1.4

SHELL_DEPS = sync

dep_sync  = git https://github.com/rustyio/sync.git        9c78e7b

include erlang.mk

CT_OPTS = -cover test/cover.spec

SHELL_OPTS = -name ${PROJECT}@`hostname` -s sync

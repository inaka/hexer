PROJECT = hexer

DEPS = shotgun

dep_shotgun = git git://github.com/inaka/shotgun.git 0.1.12

TEST_DEPS = katana mixer

dep_katana = git git://github.com/inaka/erlang-katana.git 0.2.14
dep_mixer  = git git://github.com/inaka/mixer.git         0.1.4

include erlang.mk

CT_OPTS = -cover test/cover.spec

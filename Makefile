PROJECT = hexer

DEPS = getopt shotgun

dep_getopt  = hex 0.8.2
dep_shotgun = hex 0.1.15

TEST_DEPS = katana inaka_mixer meck

dep_katana      = hex 0.2.18
dep_inaka_mixer = hex 0.1.5
dep_meck        = hex 0.8.4

SHELL_DEPS = sync

dep_sync = git https://github.com/rustyio/sync.git 9c78e7b

BUILD_DEPS = inaka_mk hexer_mk

dep_inaka_mk = git https://github.com/inaka/inaka.mk.git 1.0.0
dep_hexer_mk = git https://github.com/inaka/hexer.mk.git 1.0.1

DEP_PLUGINS = inaka_mk hexer_mk

LOCAL_DEPS += crypto common_test

include erlang.mk

ERLC_OPTS := +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

CT_OPTS = -cover test/cover.spec

SHELL_OPTS = -name ${PROJECT}@`hostname` -s sync

escript::
	mkdir -p bin
	mv ${PROJECT} bin/.

#-*- mode: makefile-gmake -*-
# Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

PROJECT = msc
PROJECT_DESCRIPTION = MySQL/MariaDB Client
PROJECT_VERSION = ${shell git describe --tags}

COVER = 1
COVER_REPORT_DIR = _site/cover
CT_LOGS_DIR = _site/ct
EDOC_OPTS = {preprocess, true}, {dir, "_site/edoc"}

DEPS += backoff
DEPS += envy
DEPS += msmp
DEPS += telemetry

SHELL_DEPS += sync

PLT_APPS += any
PLT_APPS += asn1
PLT_APPS += backoff
PLT_APPS += compiler
PLT_APPS += crypto
PLT_APPS += inets
PLT_APPS += mnesia
PLT_APPS += msmp
PLT_APPS += phrase
PLT_APPS += public_key
PLT_APPS += runtime_tools
PLT_APPS += scran
PLT_APPS += ssl
PLT_APPS += stdlib

LOCAL_DEPS += ssl

SHELL_OPTS += +pc unicode
SHELL_OPTS += -config dev.config
SHELL_OPTS += -enable-feature maybe_expr
SHELL_OPTS += -s $(PROJECT)
SHELL_OPTS += -s sync

dep_envy = $(if $(DEP_LN),ln ../../envy,git https://github.com/shortishly/envy.git)
dep_msmp = $(if $(DEP_LN),ln ../../msmp,git https://github.com/shortishly/msmp.git)
dep_narcs = $(if $(DEP_LN),ln ../../narcs,git https://github.com/shortishly/narcs.git)
dep_phrase = $(if $(DEP_LN),ln ../../phrase,git https://github.com/shortishly/phrase.git)
dep_scran = $(if $(DEP_LN),ln ../../scran,git https://github.com/shortishly/scran.git)
dep_telemetry = git https://github.com/beam-telemetry/telemetry.git


dep_backoff_commit = 1.1.6
dep_envy_commit = 0.9.0
dep_telemetry_commit = v1.1.0


include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)

app:: rebar.config

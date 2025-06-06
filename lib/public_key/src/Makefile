#
# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2008-2025. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# %CopyrightEnd%
#

#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk

VSN     = $(PUBLIC_KEY_VSN)
APP_VSN = "public_key-$(VSN)"


# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/public_key-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES = \
	public_key \
	pubkey_pem \
	pubkey_ssh \
        pubkey_cert_records \
	pubkey_crl \
	pubkey_os_cacerts \
	pubkey_translation \
	pubkey_pbe \
	pubkey_ocsp \
	pubkey_cert \
	pubkey_policy_tree

HRL_FILES = $(INCLUDE)/public_key.hrl $(INCLUDE)/OTP-PUB-KEY.hrl

INTERNAL_HRL_FILES = pubkey_moduli.hrl public_key_internal.hrl

ERL_FILES = $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))

APP_FILE= public_key.app
APPUP_FILE= public_key.appup

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

INCLUDE = ../include
# ----------------------------------------------------
# INETS FLAGS
# ----------------------------------------------------
PUB_KEY_FLAGS =

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
PUB_KEY_ERL_FLAGS += -I $(INCLUDE) -I ../asn1/

ERL_COMPILE_FLAGS += $(PUB_KEY_ERL_FLAGS) \
		     $(PUB_KEY_FLAGS) \
                     +'{parse_transform,sys_pre_attributes}' \
                     +'{attribute,insert,app_vsn,$(APP_VSN)}'

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(TYPES): $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) $(HRL_FILES)

$(EBIN)/pubkey_ssh.$(EMULATOR): pubkey_moduli.hrl
$(TARGET_FILES): $(HRL_FILES)

pubkey_moduli.hrl: ../priv/moduli
	escript ../priv/convert.escript $< $@

clean:
	rm -f $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) 
	rm -f core

docs:

$(APP_TARGET):	$(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET):	$(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(INTERNAL_HRL_FILES) $(ERL_FILES) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/include"
	$(INSTALL_DATA) $(HRL_FILES) "$(RELSYSDIR)/include"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) $(APP_TARGET) $(APPUP_TARGET) "$(RELSYSDIR)/ebin"

release_docs_spec:

# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------
include $(ERL_TOP)/make/dep.mk

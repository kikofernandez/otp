#
# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2025. All Rights Reserved.
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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(MAILBOX_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/mailbox-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES = \
	mailbox \
	mailbox_lint \
	analysis \
	mailbox_options

# HRL_FILES = ../include/mailbox.hrl

ERL_FILES= $(MODULES:%=%.erl)

INSTALL_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

TARGET_FILES= $(INSTALL_FILES)

APP_FILE= mailbox.app
# APP_SRC= $(APP_FILE).src
# APP_TARGET= $(EBIN)/$(APP_FILE)

# APPUP_FILE= mailbox.appup
# APPUP_SRC= $(APPUP_FILE).src
# APPUP_TARGET= $(EBIN)/$(APPUP_FILE)


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(TYPES): $(TARGET_FILES)

docs:

clean:
	rm -f $(TARGET_FILES)
	rm -f core

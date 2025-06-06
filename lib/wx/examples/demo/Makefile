#
# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2009-2025. All Rights Reserved.
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

include ../../vsn.mk
include ../../config.mk

TOPDIR   = ../..
SRC = .
BIN = .
ERLINC = $(TOPDIR)/include
ERLC = erlc
TESTMODS = \
	demo \
	demo_html_tagger \
	ex_aui \
	ex_button \
	ex_canvas \
	ex_canvas_paint \
	ex_choices \
	ex_cursor \
	ex_dialogs \
	ex_frame_utils \
	ex_gauge \
	ex_gl \
	ex_grid \
	ex_htmlWindow \
	ex_listCtrl \
	ex_notebook \
	ex_notificationMessage \
	ex_pickers \
	ex_popupMenu \
	ex_radioBox \
	ex_sashWindow \
	ex_sizers \
	ex_slider \
	ex_splitterWindow \
	ex_static \
	ex_textCtrl \
	ex_treeCtrl \
	ex_graphicsContext \
	ex_webview


TESTTARGETS = $(TESTMODS:%=%.beam)
TESTSRC = $(TESTMODS:%=%.erl)

ERL_COMPILE_FLAGS += +nowarn_missing_doc +nowarn_missing_spec_documented \
		     -I $(ERL_TOP)/lib -pa $(ERL_TOP)/lib/wx/ebin

# Targets
$(TYPES):	$(TESTTARGETS)
clean:
	rm -f $(TESTTARGETS)
	rm -f *~ core erl_crash.dump

docs:

run: opt
	erl -smp -detached -pa $(TOPDIR)/ebin -s demo

EXRELSYSDIR = "$(RELSYSDIR)/examples/demo"
include $(ERL_TOP)/make/otp_release_targets.mk

docs:

release_spec: opt
	$(INSTALL_DIR) $(EXRELSYSDIR)
	$(INSTALL_DATA) $(TESTSRC)  $(EXRELSYSDIR)
	$(INSTALL_DATA) $(TESTTARGETS) $(EXRELSYSDIR)
	$(INSTALL_DATA) image.jpg erlang.png ex_htmlWindow.html $(EXRELSYSDIR)

release_tests_spec:

release_docs_spec:

#!/usr/bin/env sh

## %CopyrightBegin%
##
## SPDX-License-Identifier: Apache-2.0
##
## Copyright Ericsson AB 2025-2026. All Rights Reserved.
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##
##

REPO=$1
PR_NUMBER=$2
# Fetch PR data using gh CLI
PR_STATUS=$(gh pr view "$PR_NUMBER" --repo "$REPO" --json state -q ".state")

if [ $? -ne 0 ]; then
  echo "Failed to fetch PR #$PR_NUMBER from $REPO"
  exit 2
fi

git config user.name "github-actions[bot]"
git config user.email "41898282+github-actions[bot]@users.noreply.github.com"

# Check if PR is closed
if [ "$PR_STATUS" = "CLOSED" ]; then
  echo "✅ Pull request #$PR_NUMBER is CLOSED."
  git checkout -b "$PR_NUMBER"
  git add make/openvex.table
  git add vex
  git commit -m "Automatic update of OpenVEX Statements for erlang/otp"
  git push --force origin "$PR_NUMBER"
  gh pr create --repo "$REPO" -B master \
               --title "Automatic update of OpenVEX Statements for erlang/otp" \
               --body "Automatic Action. There is a vulnerability from GH Advisories without a matching OpenVEX statement"
  exit 0
elif [ "$PR_STATUS" = "MERGED" ]; then
  echo "✅ Pull request #$PR_NUMBER is MERGED (also closed)."
  git checkout -b "$PR_NUMBER"
  git add make/openvex.table
  git add vex
  git commit -m "Automatic update of OpenVEX Statements for erlang/otp"
  git push --force origin "$PR_NUMBER"
  gh pr create --repo "$REPO" -B master \
               --title "Automatic update of OpenVEX Statements for erlang/otp" \
               --body "Automatic Action. There is a vulnerability from GH Advisories without a matching OpenVEX statement"
  exit 0
else
  echo "❌ Pull request #$PR_NUMBER is OPEN. Create a PR once the PR is closed or merged."
  exit 0
fi

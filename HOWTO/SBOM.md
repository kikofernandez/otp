# Software Bill-of-Materials (SBOM)

A Software Bill-of-Materials (SBOM) is a document to share information about the
software used, dependencies, and essentially what it is made of. SBOMs have many
different uses, and some examples include checking license compliance and/or
dependencies for vulnerabilities using databases such as
[CVE](https://www.cve.org/) and [OSV](https://osv.dev/).

Erlang/OTP has multiple third-party dependencies. Some are vendored into the 
source code of Erlang/OTP:
- pcre (`erts/emulator/pcre`)
- zlib (`erts/emulator/zlib`)
- asmjit (`erts/emulator/asmjit`)
- openssl (`erts/emulator/openssl`)
- zstd (`erts/emulator/zstd`)

The Erlang/OTP project provides source SBOMs starting with OTP-28. Below we detail
the steps necessary to run yourself the generation of the Erlang/OTP source SBOM.

## Source Software Bill-of-Materials

Erlang/OTP provides a source SBOM in each release. There are two different ways to
generate source SBOM. Here we provide details of the simplest form, together with
a script needed to state some fixes that should be run.

### Erlang/OTP source SBOM

This is the simplest source SBOM. All Erlang/OTP repository code gets into
a SPDX. We make no efforts into splitting Erlang/OTP generated SPDX into multiple
applications.

**Steps**

1. Install `oss-review-toolkit` (ORT) and `scancode`.
2. Run the ORT analyzer from source (some paths may need tweaking):
   ```   
   ./gradlew cli:run --args="-c <PATH-TO-OTP>/.ort/config/config.yml analyze -i <PATH-TO-OTP> -o . -f JSON --repository-configuration-file=otp/.ort.yml"
   ```
3. Run the ORT scanner (`scancode` is needed):
   ```
   ./gradlew cli:run --args="-c <PATH-TO-OTP>/.ort/config/config.yml scan -o . -f JSON -i cli/analyzer-result.json"
   ```
4. Generate ORT SPDX report
   ```
   ./gradlew cli:run --args="report -i cli/scan-result.json -o . -f SpdxDocument -O SpdxDocument=outputFileFormats=JSON"
   ```
5. From the Erlang/OTP repo, run the following escript to fix some known issues from the generated SPDX:   
   ```
   .github/scripts/otp-compliance.es sbom otp-info --sbom-file ort/cli/bom.spdx.json --input-file ort/cli/scan-result.json
   ```

### Erlang/OTP applications source SBOM

This source SBOM is identical to the one above, except that our script
identifies Erlang/OTP apps into SPDX packages. Companies that do not use all of
Erlang/OTP can easily opt-out (remove) non-used packages from the source SBOM
SPDX.

### Erlang/OTP SBOM Generation

Erlang/OTP generates and uploads to sigstore the generated SBOM via Github Actions.
All the details can be found in `otp/.github/workflows/main.yaml`.

# attest (development version)

* `att_export()` writes a portable JSON manifest of all sources in the store — capturing URLs, local paths, metadata, and archive classifications. The manifest defaults to `attest-manifest.json` in the working directory and is intended to be version-controlled.
* `att_import()` reads an `att_export()` manifest, generates a `get-data.R` script with explicit `att_source()`/`att_download()`/`att_register()` calls, and executes it. The script is kept as a readable record of data acquisition. Existing sources are skipped; missing local paths are reported as failures.
* `att_cite()` no longer renders `url = {list()}` in BibTeX entries when a source has no `landing_url`. The `url` field is now omitted entirely in that case.
* `att_add_metadata()` downloads additional metadata files (codebooks, schemas, documentation) to an existing source without re-downloading data files. Provenance is updated in place.
* `att_cite()` now generates a `data-sources.md` file alongside `data-sources.bib`, containing APA-style plain-text citations sorted alphabetically by author. The markdown file is kept in sync automatically when citations are added, updated, or removed.
* `att_download()` now supports `.tar.gz` and `.tgz` archives in addition to `.zip`. Archive detection, extraction, classification, and all downstream operations (`att_check()`, `att_compare()`, `att_refresh()`) work with both formats.

# attest 0.3.1
* `att_download()` now auto-classifies extracted archive files without prompting when all files have well-known extensions (e.g., shapefile bundles, CSVs, GeoJSON). The interactive classification prompt only appears when the archive contains files with ambiguous extensions.
* `att_source()` gains `format` and `author` arguments. The `format` field (e.g., `"CSV"`, `"Shapefile"`, `"File Geodatabase"`) is included in APA-style BibTeX citations as `[Data set; Format]`. `att_update()` can also set the format after the fact.
* `att_cite()` now includes a `note` field in BibTeX entries with the APA-style data set designation (`[Data set]` or `[Data set; Format]`).
* `att_prove()` generates a comprehensive provenance report across all sources in the store — verifies every file, compiles metadata, and writes a JSON audit document.
* `att_update()` modifies provenance metadata (title, publisher, author, year, landing URL) and regenerates the BibTeX citation. Use this to correct mistakes without manually editing `provenance.json`.
* `att_remove()` deletes a source directory and its BibTeX entry, with interactive confirmation or `force = TRUE` for scripts.

# attest 0.3.0

* `att_download()` now auto-detects `.zip` URLs and extracts archive contents. In interactive sessions, an interactive prompt lets you classify each extracted file as data, metadata, or ignore. In non-interactive sessions, use the `classify` argument for extension-based classification.
* `att_check()`, `att_compare()`, and `att_refresh()` now handle archive sources. `att_check()` does HTTP HEAD on the archive URL. `att_compare()` re-downloads and compares the archive hash. `att_refresh()` re-downloads, extracts, and updates only changed files while preserving classifications.
* All HTTP downloads now show progress bars via `httr2::req_progress()`.
* The metadata classification heuristic for archive extraction now only defaults PDFs and documentation name patterns (readme, codebook, dictionary, metadata, license, changelog) to metadata. File extensions like `.xml` and `.html` no longer default to metadata, avoiding misclassification of companion data files such as shapefile `.shp.xml` records.

# attest 0.2.0

* Renamed package from `acquire` to `attest`. All functions use the `att_*` prefix and the internal store directory is `_attest/`.
* `att_register()` adds support for local file acquisition. Copies (or moves with `move = TRUE`) local files into the canonical store structure with full provenance tracking.
* `att_compare()` re-fetches files and compares hashes for definitive change detection, supporting both remote and local sources.
* `att_check()` provides lightweight change detection via HTTP HEAD (remote) or file stat (local) without re-downloading.
* `att_refresh()` re-fetches, compares, archives changed files, and updates the store in place.
* `att_verify()` compares local file hashes against recorded provenance.
* `att_cite()` generates BibTeX entries from source metadata and provenance.
* `att_status()` provides a summary table of all tracked sources in a store.

# attest 0.1.0

* Initial release with `att_source()`, `att_download()`, `att_store()`, and `att_hash()`.

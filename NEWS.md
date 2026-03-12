# attest (development version)

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

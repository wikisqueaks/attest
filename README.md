# attest <img src="man/figures/logo.png" align="right" height="120" alt="" />

Document the provenance of external data.

Researchers working with data from government portals, institutional
repositories, and other external sources need to record where their data came
from. `attest` downloads files from known URLs, computes cryptographic hashes,
and generates BibTeX citations — producing a self-contained provenance record
alongside the data itself.

## Installation

``` r
# install.packages("pak")
pak::pak("wikisqueaks/attest")
```

## Example

Document and download the USGS M4.5+ earthquake feed:

``` r
library(attest)

# Define a data source: where it lives, what it is
usgs_quakes <- att_source(
  name = "usgs-earthquakes-4.5-month",
  landing_url = "https://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php",
  data_urls = c(
    "earthquakes.csv" = "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_month.csv"
  ),
  title = "USGS M4.5+ Earthquakes, Past 30 Days",
  publisher = "U.S. Geological Survey",
  year = "2025"
)

# Download files, compute hashes, write provenance, and generate citation
att_download(usgs_quakes)
#> ℹ Downloading 1 data file...
#> → Downloading <https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_month.csv>
#> ✔ BibTeX entry written to data/raw/data-sources.bib
#> ✔ Source "usgs-earthquakes-4.5-month" downloaded
```

After downloading, the store contains a complete provenance record:

```
data/raw/
├── data-sources.bib
└── usgs-earthquakes-4-5-month/
    ├── earthquakes.csv
    └── _attest/
        └── provenance.json
```

You can verify that local files haven't been modified:

``` r
att_verify(usgs_quakes)
#> ✔ earthquakes.csv: OK
#> ✔ All files verified
```

Or check whether the remote source has changed since download:

``` r
att_compare(usgs_quakes)
#> ℹ Comparing 1 file for "usgs-earthquakes-4-5-month"...
#> → Fetching earthquakes.csv
#> ! earthquakes.csv: remote has changed
```

When changes are detected, use `att_refresh()` to update. It archives the
current files, downloads fresh copies, and reports what changed:

``` r
att_refresh(usgs_quakes)
#> ℹ Refreshing 1 file for "usgs-earthquakes-4-5-month"...
#> → Fetching earthquakes.csv
#> ✔ Archived "usgs-earthquakes-4-5-month"
#> ! 1 file changed for "usgs-earthquakes-4-5-month":
#> ℹ earthquakes.csv: 3a7f1c9b0e2d... → 8b4e2f1a7c3d... (245891 → 251003 bytes)
```

Note that `att_download()` is for first-time acquisition only — calling it
again on an existing source will error and recommend `att_refresh()`.

### Registering local files

Not all data comes from a URL. For files received by email, transferred from a
shared drive, or already sitting in your project folder, use `att_register()`:

``` r
survey <- att_source(
  name = "annual-survey-2025",
  data_paths = c("survey.csv" = "~/shared-drive/survey-2025.csv"),
  metadata_paths = c("codebook.pdf" = "~/shared-drive/survey-codebook.pdf"),
  title = "Annual Household Survey 2025",
  publisher = "Ministry of Statistics",
  year = "2025"
)

# Copy files into the store and record provenance
att_register(survey)
#> ℹ Registering 1 data file...
#> → Copying '~/shared-drive/survey-2025.csv'
#> ℹ Registering 1 metadata file...
#> → Copying '~/shared-drive/survey-codebook.pdf'
#> ✔ Source "annual-survey-2025" registered
```

Use `move = TRUE` to avoid duplication — files are moved into the store and
the originals are deleted (after verifying the copy):

``` r
att_register(survey, move = TRUE)
```

If the files are already in the correct store location, `att_register()` skips
the copy and simply records provenance. All downstream functions
(`att_verify()`, `att_compare()`, `att_refresh()`) work with registered
sources the same way they do with downloaded sources.

### Sharing sources across projects

To start a new project with the same data sources, export a manifest and use
it to set up the new project:

``` r
# In the original project: export a manifest
att_export()
#> ✔ Exported 3 sources to 'attest-manifest.json'
```

Copy `attest-manifest.json` to the new project (or check it into version
control), then import:

``` r
# In the new project: generate get-data.R and acquire all sources
att_import()
#> ✔ Generated 'get-data.R'
#> ℹ Acquiring 3 sources from manifest...
```

`att_import()` writes a `get-data.R` script with explicit `att_source()` /
`att_download()` / `att_register()` calls, then executes it. The script stays
as a readable, editable record of your data acquisition step.

## What gets recorded

For each source, `attest` writes a `_attest/provenance.json` file containing:

- The **origin** of the source: `"remote"` (downloaded) or `"local"` (registered)
- The **URL** or **original file path** each file came from
- **SHA-256 hashes** computed at acquisition time
- **Timestamps** for when the acquisition occurred
- **HTTP metadata** (ETag, Last-Modified, Content-Type) when available (remote only)
- All user-supplied **metadata** (title, publisher, description, license, etc.)

Metadata files (data dictionaries, codebooks) can be specified via
`metadata_urls` (remote) or `metadata_paths` (local) and are saved in a
`metadata/` subdirectory. A single `data-sources.bib` at the store root
accumulates BibTeX entries across all sources.

## Key functions

| Function | Purpose |
|---|---|
| `att_store()` | Get or set the active store path |
| `att_source()` | Define a data source (URLs or local paths, metadata) |
| `att_download()` | First-time download (remote): hash, write provenance and citation |
| `att_register()` | First-time registration (local): copy/move files into store, record provenance |
| `att_refresh()` | Re-fetch, compare, archive if changed, update in place |
| `att_add_metadata()` | Add metadata files to an existing source |
| `att_verify()` | Compare local file hashes to recorded hashes |
| `att_compare()` | Re-fetch and compare hashes to detect source changes |
| `att_check()` | Lightweight change detection (HTTP HEAD or file stat) |
| `att_cite()` | Regenerate a BibTeX entry |
| `att_update()` | Modify provenance metadata (title, publisher, etc.) |
| `att_remove()` | Delete a source and its BibTeX entry |
| `att_export()` | Export a portable manifest of all sources |
| `att_import()` | Generate and run a data-acquisition script from a manifest |
| `att_prove()` | Generate a provenance report across all sources |
| `att_status()` | Summary table of all sources in the store |
| `att_read_provenance()` | Read a source's provenance record |
| `att_hash()` | SHA-256 hash of a file |

## License

MIT

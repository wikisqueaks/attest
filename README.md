# acquire <img src="man/figures/logo.png" align="right" height="120" alt="" />

Document the provenance of external data.

Researchers working with data from government portals, institutional
repositories, and other external sources need to record where their data came
from. `acquire` downloads files from known URLs, computes cryptographic hashes,
and generates BibTeX citations — producing a self-contained provenance record
alongside the data itself.

## Installation

``` r
# install.packages("pak")
pak::pak("user/acquire")
```

## Example

Document and download the USGS M4.5+ earthquake feed:

``` r
library(acquire)

# Define a data source: where it lives, what it is
usgs_quakes <- acq_source(
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
acq_download(usgs_quakes)
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
    └── _acquire/
        └── provenance.json
```

You can verify that local files haven't been modified:

``` r
acq_verify(usgs_quakes)
#> ✔ earthquakes.csv: OK
#> ✔ All files verified
```

Or check whether the remote source has changed since download:

``` r
acq_compare(usgs_quakes)
#> ℹ Comparing 1 file for "usgs-earthquakes-4-5-month"...
#> → Fetching earthquakes.csv
#> ! earthquakes.csv: remote has changed
```

When changes are detected, use `acq_refresh()` to update. It archives the
current files, downloads fresh copies, and reports what changed:

``` r
acq_refresh(usgs_quakes)
#> ℹ Refreshing 1 file for "usgs-earthquakes-4-5-month"...
#> → Fetching earthquakes.csv
#> ✔ Archived "usgs-earthquakes-4-5-month"
#> ! 1 file changed for "usgs-earthquakes-4-5-month":
#> ℹ earthquakes.csv: 3a7f1c9b0e2d... → 8b4e2f1a7c3d... (245891 → 251003 bytes)
```

Note that `acq_download()` is for first-time acquisition only — calling it
again on an existing source will error and recommend `acq_refresh()`.

## What gets recorded

For each source, `acquire` writes a `_acquire/provenance.json` file containing:

- The **URL** each file was downloaded from
- **SHA-256 hashes** computed at download time
- **Timestamps** for when the download occurred
- **HTTP metadata** (ETag, Last-Modified, Content-Type) when available
- All user-supplied **metadata** (title, publisher, description, license, etc.)

Metadata files (data dictionaries, codebooks) can be specified via
`metadata_urls` and are saved in a `metadata/` subdirectory. A single
`data-sources.bib` at the store root accumulates BibTeX entries across all
sources.

## Key functions

| Function | Purpose |
|---|---|
| `acq_store()` | Get or set the active store path |
| `acq_source()` | Define a data source (URLs, metadata) |
| `acq_download()` | First-time download: hash, write provenance and citation |
| `acq_refresh()` | Re-download, compare, archive if changed, update in place |
| `acq_verify()` | Compare local file hashes to recorded hashes |
| `acq_compare()` | Re-download and compare hashes to detect remote changes |
| `acq_cite()` | Regenerate a BibTeX entry |
| `acq_status()` | Summary table of all sources in the store |
| `acq_read_provenance()` | Read a source's provenance record |
| `acq_hash()` | SHA-256 hash of a file |

## License

MIT

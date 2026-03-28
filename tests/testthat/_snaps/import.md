# att_import validates manifest type

    Code
      att_import(bad_file)
    Condition
      Error in `att_import()`:
      ! File does not appear to be an attest manifest.
      i Expected "manifest_type" to be "attest_manifest".

# att_import errors on missing file

    Code
      att_import("/nonexistent/manifest.json")
    Condition
      Error in `att_import()`:
      ! Manifest not found at '/nonexistent/manifest.json'.


# Changelog - warc


## Unreleased

- Added
    - `idsmc` and `idsmc` folder shortcuts to `warc_dirs()`.
- Changed
    - `tbl_number()`, `fig_number()`, and `map_number()` no longer put `.` after the number (just outputs `"Table 1"` instead of `"Table 1."`).
    - `tbl_number()`, `fig_number()`, and `map_number()` no longer accept Character keyword input in `i` to look around at numbers without incrementing the counter. Instead, `i` now takes Numeric input: `tbl_number(0)` returns the current table number, `tbl_number(-2)` returns the number 2 tables back, and `tbl_number(+10)` returns the number 10 tables from now.
- Fixed
- Deprecated
- Removed


## 0.2.0 (2023-01-30)

- Added
    - `tbl_number()`
    - `fig_number()`
    - `map_number()`
- Changed
- Fixed
- Deprecated
- Removed


## 0.1.0 (2022-11-30)

- Added
    - `shrink_redcap_cols()`
    - `copy_definition()`
    - `with_label()`
    - `copy_dataframe_labels()`
    - `warc_dirs()`
- Changed
- Fixed
- Deprecated
- Removed



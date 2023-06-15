# Changelog - warc

# 1.0.0

- Removed
    - `warc_dirs()`. Use relative directory paths instead (e.g. "../../../03_Data/Raw_data/mydata.csv").

- Deprecated
    - `with_label()`. Use `labelled::set_variable_labels()` instead.
    - `set_labels()`. Use `labelled::set_variable_labels()` instead.

- Added
    - `appx_number()`, which autonumbers Appendix items.

- Changed
    - `tbl_number()`, `fig_number()`, and `map_number()` no longer put `.` after the number (just outputs `"Table 1"` instead of `"Table 1."`).
    - `tbl_number()`, `fig_number()`, and `map_number()` no longer accept Character keyword input in `i` to look around at numbers without incrementing the counter. Instead, `i` now takes Numeric input: `tbl_number(0)` returns the current table number, `tbl_number(-2)` returns the number 2 tables back, and `tbl_number(+10)` returns the number 10 tables from now.



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



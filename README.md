# warc convenience functions

- `shrink_redcap_cols()` --- Remove duplicate columns that are generated by REDCap's data import code
- `copy_definition()` --- Copy factor levels, ordering, and labels from one vector to another
- `with_label()` --- Create a new vector and label it with `Hmisc::label()` in one step (useful for pipelines)
- `copy_dataframe_labels()` --- Copy the `label` and `labels` attributes (if any) from one dataframe into another.

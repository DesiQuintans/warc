# Functions about working with WARC's standardised folder structure



#' Generate shortcuts to folders in WARC's standardised folder structure
#'
#' This function creates shortcuts to the folder paths that make up WARC's standard folder
#' structure for its projects on the Remote Data Store (RDS), part of which is shown below
#' (with the named shortcuts included):
#' \preformatted{
#' - Project root ($root)
#'     - 1. Study Design
#'     - 2. Data Monitoring
#'     - 3. Data
#'         - Raw data ($rawdata)
#'         - Derived data ($derived)
#'     - 4. IDSMC
#'     - 5. SAP
#'     - 6. Analysis
#'         - Analysis programs ($analysis)
#'     - 7. Analysis Validation
#'         - Validation programs ($validation)
#'     - 8. Statistical Report ($report)
#'     - 9. Post SAP analysis
#' }
#' This function also produces shortcuts to your working folder on the Remote Data Store
#' (`$me`) and your working folder's *_output/* folder (`$out`).
#'
#' @param rds_folder (Character) The full path to your personal working project directory
#'     on the Remote Data Store. This is usually a folder that you've created inside
#'     `/6. Analysis/Analysis programs/` or `/7. Analysis Validation/Validation programs/`.
#'
#' @details
#' This function created a named list of shortcuts. The names of the shortcuts are:
#'
#' 1. `root`, the project's root folder
#' 2. `rawdata`, the directory storing raw data
#' 3. `derived`, storing intermediate data
#' 4. `analysis`, which holds any analysis programs
#' 5. `validation`, which holds any validation programs
#' 6. `me`, which is what you entered in the `rds_folder` argument
#' 7. `out`, which is the `_output/` folder inside your `rds_folder`. If it doesn't exist,
#'    it will be created.
#' 8. `report`, contains statistical reports.
#'
#' @return A named list of directory paths to key folders on the RDS. Use [file.path()] to build a path using these shortcuts (see the examples).
#'
#' @export
#'
#' @examples
#' \preformatted{
#' rds <- warc_dirs("E:/PRJ-Clinical_Trial/6. Analysis/Analysis programs/myname")
#'
#' rds$root
#' ## [1] "E:/PRJ-Clinical_Trial"
#'
#' rds$rawdata
#' ## [1] "E:/PRJ-Clinical_Trial/3. Data/Raw data"
#'
#' rds$derived
#' ## [1] "E:/PRJ-Clinical_Trial/3. Data/Derived data"
#'
#' rds$analysis
#' ## [1] "E:/PRJ-Clinical_Trial/6. Analysis/Analysis programs"
#'
#' rds$validation
#' ## [1] "E:/PRJ-Clinical_Trial/7. Analysis Validation/Validation programs"
#'
#' rds$me
#' ## [1] "E:/PRJ-Clinical_Trial/6. Analysis/Analysis programs/myname"
#'
#' rds$out
#' ## [1] "E:/PRJ-Clinical_Trial/6. Analysis/Analysis programs/myname/_output"
#'
#' rds$report
#' ## [1] "E:/PRJ-Clinical_Trial/8. Statistical Report"
#'
#' # -------------------------------------------------------------------------
#'
#' # Example 1: Loading a CSV from the raw data folder
#' # raw_data <- read.csv(file.path(rds$rawdata, "mydata.csv"))
#'
#' # Example 2: Saving and loading transformed data to the derived data folder
#' # save(transformed_data, file = file.path(rds$derived, "02_data.Rdata"))
#' # load(file.path(rds$derived, "02_data.Rdata"))
#'
#' # Example 3: Saving a plot to your output folder
#' # my_hist <- hist(islands)
#' # saveRDS(my_hist, file.path(rds$out, "islands histogram.rds"))
#' }
warc_dirs <- function(rds_folder) {
    if (dir.exists(rds_folder) == FALSE) {
        stop(
            glue::glue("The folder '{rds_folder}' does not exist. \n
                        The 'rds_folder' argument should point to your working folder on
                        the Remote Data Store, which should be mapped to a drive. \n
                        For example, 'E:/PRJ-Example_Project/6. Analysis/myscripts'.")
        )
    }

    remote_root <- sub(pattern = "^(.*?)/(6. Analysis|7. Analysis Validation).*?$",
                       replacement = "\\1",
                       x = rds_folder)

    rds <- list(
        root = remote_root,
        rawdata = file.path(remote_root, "3. Data/Raw data"),
        derived = file.path(remote_root, "3. Data/Derived data"),
        analysis = file.path(remote_root, "6. Analysis/Analysis programs"),
        validation = file.path(remote_root, "7. Analysis Validation/Validation programs"),
        me = rds_folder,
        out = file.path(rds_folder, "_output"),
        report = file.path(remote_root, "8. Statistical Report")
    )

    if (dir.exists(rds$rawdata) == FALSE) {
        stop(
            glue::glue("The folder '{rds$root}' does not seem to have the standard WARC
                        folder structure. See ?warc_dirs for more information.")
        )
    }

    if (dir.exists(rds$out) == FALSE) {
        dir.create(rds$out)
    }

    return(rds)
}

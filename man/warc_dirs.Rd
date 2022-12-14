% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filesystem funcs.R
\name{warc_dirs}
\alias{warc_dirs}
\title{Generate shortcuts to folders in WARC's standardised folder structure}
\usage{
warc_dirs(rds_folder, quiet = FALSE)
}
\arguments{
\item{rds_folder}{(Character) The full path to your personal working project directory
on the Remote Data Store. This is usually a folder that you've created inside
\verb{/6. Analysis/Analysis programs/} or \verb{/7. Analysis Validation/Validation programs/}.}

\item{quiet}{(Logical) If \code{FALSE} (default), print the shortcuts and their paths. This
is good for reproducibility.}
}
\value{
A named list of directory paths to key folders on the RDS. Use \code{\link[=file.path]{file.path()}} to build a path using these shortcuts (see the examples).
}
\description{
This function creates shortcuts to the folder paths that make up WARC's standard folder
structure for its projects on the Remote Data Store (RDS), part of which is shown below
(with the named shortcuts included):
\preformatted{
- Project root ($root)
    - 1. Study Design
    - 2. Data Monitoring
    - 3. Data
        - Raw data ($rawdata)
        - Derived data ($derived)
    - 4. IDSMC
    - 5. SAP
    - 6. Analysis
        - Analysis programs ($analysis)
    - 7. Analysis Validation
        - Validation programs ($validation)
    - 8. Statistical Report ($report)
    - 9. Post SAP analysis
}
This function also produces shortcuts to your working folder on the Remote Data Store
(\verb{$me}) and your working folder's \emph{_output/} folder (\verb{$out}).
}
\details{
This function created a named list of shortcuts. The names of the shortcuts are:
\enumerate{
\item \code{root}, the project's root folder
\item \code{rawdata}, the directory storing raw data
\item \code{derived}, storing intermediate data
\item \code{analysis}, which holds any analysis programs
\item \code{validation}, which holds any validation programs
\item \code{me}, which is what you entered in the \code{rds_folder} argument
\item \code{out}, which is the \verb{_output/} folder inside your \code{rds_folder}. If it doesn't exist,
it will be created.
\item \code{report}, contains statistical reports.
}
}
\examples{
\preformatted{
rds <- warc_dirs("E:/PRJ-Clinical_Trial/6. Analysis/Analysis programs/myname")

rds$root
## [1] "E:/PRJ-Clinical_Trial"

rds$rawdata
## [1] "E:/PRJ-Clinical_Trial/3. Data/Raw data"

rds$derived
## [1] "E:/PRJ-Clinical_Trial/3. Data/Derived data"

rds$analysis
## [1] "E:/PRJ-Clinical_Trial/6. Analysis/Analysis programs"

rds$validation
## [1] "E:/PRJ-Clinical_Trial/7. Analysis Validation/Validation programs"

rds$me
## [1] "E:/PRJ-Clinical_Trial/6. Analysis/Analysis programs/myname"

rds$out
## [1] "E:/PRJ-Clinical_Trial/6. Analysis/Analysis programs/myname/_output"

rds$report
## [1] "E:/PRJ-Clinical_Trial/8. Statistical Report"

# -------------------------------------------------------------------------

# Example 1: Loading a CSV from the raw data folder
# raw_data <- read.csv(file.path(rds$rawdata, "mydata.csv"))

# Example 2: Saving and loading transformed data to the derived data folder
# save(transformed_data, file = file.path(rds$derived, "02_data.Rdata"))
# load(file.path(rds$derived, "02_data.Rdata"))

# Example 3: Saving a plot to your output folder
# my_hist <- hist(islands)
# saveRDS(my_hist, file.path(rds$out, "islands histogram.rds"))
}
}

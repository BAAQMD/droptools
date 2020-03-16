context("get_dropbox_data")

#'----------------------------------------------------------------------

cat_staff <- "Michael"
sheet_name <- "throughputs"

#
# How long ago are we interested in?
#
# This example defines "then" as "14 days ago".
#
then_dttm <- Sys.time() - lubridate::ddays(14)

XLSX_path <-
  file.path(
    "BY2015",
    "Work",
    "Area Sources",
    cat_staff,
    glue::glue("BY2015-area_sources-{cat_staff}.xlsx"))

now_data <-
  get_dropbox_data(
    XLSX_path,
    sheet = sheet_name,
    verbose = TRUE)

then_data <-
  get_dropbox_data(
    XLSX_path,
    sheet = sheet_name,
    as_of = then_dttm,
    verbose = TRUE)

difftools::diff_and_render(
  then_data,
  now_data)


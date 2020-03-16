context("get_dropbox_data")

XLSX_path <- "/BY2015/Categories/BY2015_categories_assignment.xlsx"

expect_true(
  drop_exists(
    XLSX_path))

get_dropbox_history(
  XLSX_path)

now_data <-
  get_dropbox_data(
    XLSX_path,
    sheet = "BY2015_categories (master list)",
    verbose = TRUE)

then_data <-
  get_dropbox_data(
    XLSX_path,
    sheet = "BY2015_categories (master list)",
    as_of = ISOdate(2019, 11, 01),
    verbose = TRUE)

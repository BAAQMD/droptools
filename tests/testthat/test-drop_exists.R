context("drop_exists")

# Checking for compliance / compatibility with
# https://github.com/karthik/rdrop2/pull/177/commits/2524ed753d171438af1e1319f837a7dc72771c87

test_that("title case", {

  test_path <- traceless("Test Drop Exists")

  expect_message(
    rdrop2::drop_create(test_path),
    "created successfully")

  expect_true(
    rdrop2::drop_exists(test_path))

  rdrop2::drop_delete(test_path)

})

test_that("case insensitivity", {

  test_path <- traceless("Test Drop Exists")

  expect_message(
    rdrop2:: drop_create(test_path),
    "created successfully")

  expect_true(
    rdrop2::drop_exists(
      tolower(test_path)))

  expect_true(
    rdrop2:: drop_exists(
      toupper(test_path)))

  rdrop2::drop_delete(test_path)

})

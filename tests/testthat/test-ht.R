test_that("ht works with tibbles", {
    require(tibble)
    ht_n <- 5
    dat_tibble <- as_tibble(iris)
    ht_tibble <- ht(dat_tibble, n = ht_n)
    expect_false("tbl_df" %in% class(ht_tibble))
    expect_true(nrow(ht_tibble) == 10 & ncol(ht_tibble) > 0)
})

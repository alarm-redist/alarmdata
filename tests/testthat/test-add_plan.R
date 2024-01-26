test_that("involved add plan works", {
    skip_on_cran()
    url <- 'https://github.com/PlanScore/Redistrict2020/raw/main/files/NM-2021-10/Congressional_Concept_A.zip'
    tf <- tempfile(fileext = '.zip')
    utils::download.file(url, tf)
    utils::unzip(tf, exdir = dirname(tf))
    baf <- readr::read_csv(file = paste0(dirname(tf), '/Congressional Concept A.csv'),
                           col_types = 'ci')
    names(baf) <- c('GEOID', 'concept_a')
    map_nm <- alarm_50state_map('NM')
    x <- alarm_add_plan(baf, plans = alarm_50state_plans('NM', stats = FALSE),
                        map = map_nm, name = 'concept_a')

    expect_equal(nrow(x), 15006)
})

test_that("single state add plan works", {
    x <- alarm_add_plan(ref_plan = 1, plans = alarm_50state_plans('DE', stats = FALSE),
                        map = alarm_50state_map('DE'), name = 'example')
    expect_equal(nrow(x), 5002)
    expect_s3_class(x, 'data.frame')
})

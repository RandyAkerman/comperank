cr_data <- ncaa2005
# Output table based on table 2.1 of "Who's #1"
output_ratings <- tibble::tibble(
  player = c("Duke", "Miami", "UNC", "UVA", "VT"),
  rating_overall = c(-24.8, 18.2, -8, -3.4, 18),
  rating_defensive = c(-26.8,-3.8,-9.4,-11.2,-2.7),
  rating_offensive = c(2,22,1.4,7.8,20.7),
)

output_rankings <- tibble::tibble(
  player = c("Duke", "Miami", "UNC", "UVA", "VT"),
  ranking_overall = c(5,1,4,3,2),
  ranking_defensive = c(5,2,3,4,1),
  ranking_offensive = c(4,1,5,3,2),
)


# rate_massey -------------------------------------------------------------
test_that("massey ratings works", {
  predictors <-
    ncaa2005 %>%
    dplyr::select(game, player)
  outcome <-
    ncaa2005 %>%
    dplyr::select(score)
  # XY interface
  mod_XY <- massey2(predictors, outcome) %>%
    purrr::pluck(2) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 1))

  expect_equal_tbls(mod_XY, output_ratings)

  mod_formula <- massey2(score ~ ., ncaa2005) %>%
    purrr::pluck(2) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 1))

  expect_equal_tbls(mod_formula, output_ratings)

  rec <- recipes::recipe(score ~ ., ncaa2005)
  mod_recipe <- massey2(rec, ncaa2005) %>%
    purrr::pluck(2) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 1))

  expect_equal_tbls(mod_recipe, output_ratings)
})

test_that("Input with `player` as factor remains in massey ratings", {
  inds <- c(5, 2, 1, 3, 4)
  players_1 <- output_ratings$player[inds]
  input_1 <- ncaa2005
  input_1$player <- factor(input_1$player, levels = players_1)

  output_1 <- massey2(score ~ ., input_1) %>%
    purrr::pluck(2) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 1)) %>%
    dplyr::arrange(player)

  output_ref_1 <- output_ratings
  output_ref_1$player <- factor(output_ref_1$player, levels = players_1)

  expect_equal_tbls(output_1, output_ref_1[inds, ])

})

test_that("Unknown players ('NA') are not given rankings", {
  players_2 <- output_ratings$player[1:3]
  input_2 <- ncaa2005
  input_2$player <- factor(input_2$player, levels = players_2)
  # output_2_x <- rate_massey(input_2)
  output_2_x <-
    massey2(score ~ ., input_2) %>%
    purrr::pluck(2) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 1))

  output_2_y <-
    massey2(score ~ ., input_2[c(1, 2, 3, 4, 9, 10), ]) %>%
    purrr::pluck(2) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 1))

  expect_equal_tbls(output_2_x, output_2_y)
})

test_that("rate_massey works with numeric `player`", {
  input <- ncaa2005
  input$player <- as.integer(as.factor(input$player))
  output <-
    massey2(score ~ ., input) %>%
    purrr::pluck(2) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 1))

  output_ref <- output_ratings
  output_ref$player <- 1:5

  expect_equal_tbls(output, output_ref)
})

test_that("rate_massey handles players absent in `cr_data`", {
  players <- c(output_ratings$player, "extra")
  input <- ncaa2005
  input$player <- factor(input$player, levels = players)

  expect_message(
    expect_error(massey2(score ~ ., input)),
    "^rate_massey: .* players .*absent.*  extra"
  )
})

test_that("rate_massey works with not all matchups present", {
  input <- ncaa2005[1:10, ]
  output <-
    massey2(score ~ ., input) %>%
    purrr::pluck(2) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 1)) %>%
    dplyr::select(player, rating_overall)

  output_ref <- output_ratings %>%
    dplyr::select(player, rating_overall)
  output_ref$rating_overall <- c(-24.8, 12.2, -13.8, 6.2, 20.2)

  expect_equal_tbls(output, output_ref)
})

test_that("rate_massey throws error on not pair games", {
  input_nonpair <- data.frame(
    game = rep(1, 3),
    player = 1:3,
    score = 10:12,
    extraCol = -(1:3)
  )

  expect_error(massey2(score ~ ., input_nonpair), "not.*pairgames")
})

# rank_massey -------------------------------------------------------------
test_that("rank_massey works", {
  input <- ncaa2005
  output_1 <-
    massey2(score ~ ., input) %>%
    purrr::pluck(1)

  expect_equal_tbls(output_1, output_rankings)

})

test_that("massey2 summary present ranking and ratings", {
  # replicate table 2.1 page 11
  output_2 <- rank_massey(cr_data, keep_rating = TRUE)
  output_ref_2 <- output_base
  output_ref_2[["ranking_massey"]] <- output_ref_1$ranking_massey

  expect_equal_tbls(output_2, output_ref_2)
})

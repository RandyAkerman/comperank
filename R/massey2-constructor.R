new_massey2 <- function(rankings, ratings, blueprint) {
  hardhat::new_model(rankings = rankings, ratings = ratings,
                     blueprint = blueprint, class = "massey2")
}

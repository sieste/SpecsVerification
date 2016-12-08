context('Scores of some specific forecasts')

test_that('score functions return correct values', {
#  expect_equal(EnsBrier(ens.bin, obs.bin), c(0,0))
#  expect_equal(EnsCrps(ens, obs), c(0,0))
#  expect_equal(EnsQs(ens.cat, obs.cat), c(0,0))
#  expect_equal(EnsRps(ens.cat, obs.cat), c(0,0))
#  expect_equal(GaussCrps(obs, rep(0, length(obs)), obs), c(0,0))

  expect_equal(EnsBrier(matrix(c(0,1,1), nrow=1), 1), 1/9)
  expect_equal(EnsBrier(matrix(c(0,0,0), nrow=1), 1), 1)

  expect_equal(EnsCrps(matrix(1, nrow=1), 2), 1)
})




context('Scores of perfect forecasts')

# perfect forecasts and observations for all forecast types
p = c(0, 1, 0)
o = c(0, 1, 0)

ens = matrix(c(1,1,1, 2,2,2), byrow=TRUE, nrow=2)
obs = c(1, 2)

ens.bin = matrix(c(1,1,1, 0,0,0), byrow=TRUE, nrow=2)
obs.bin = c(1, 0)

ens.cat = matrix(c(3,3,3, 1,1,1), byrow=TRUE, nrow=2)
obs.cat = c(3, 1)

test_that('Scores of perfect forecast is zero', {
expect_equal(EnsBrier(ens.bin, obs.bin), c(0,0))
})

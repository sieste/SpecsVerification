SpecsVerification:
	Rscript --vanilla -e '.libPaths("/home/stefan/lib/R/")' -e 'roxygen2::roxygenize(package.dir=".", clean=TRUE)';\


.SUFFIXES:
MAKEFLAGS += -r

PKGFILES := $(shell find R data src man tests -type f ! -name "*.swp") DESCRIPTION NAMESPACE
PACKAGE := $(shell awk -F": +" '/^Package/ { print $$2 }' DESCRIPTION)
VERSION := $(shell awk -F": +" '/^Version/ { print $$2 }' DESCRIPTION)
R_PKG_tgz := $(PACKAGE)_$(VERSION).tar.gz


.PHONY: all build install test clean

all: build install
build: $(R_PKG_tgz)
install: libtmp/$(PACKAGE)

$(R_PKG_tgz): $(PKGFILES)
	R-devel -e 'tools::package_native_routine_registration_skeleton(".", con=stderr())' 2> src/native_routine_registration.c;\
	R -e 'Rcpp::compileAttributes()';\
	R -e 'roxygen2::roxygenize(package.dir=".", clean=TRUE)';\
	R CMD build .

libtmp/$(PACKAGE): $(R_PKG_tgz)
	mkdir -p libtmp;\
	R CMD INSTALL $(R_PKG_tgz) -l libtmp

check: 
	R_LIBS=/home/stefan/lib/R R CMD check $(R_PKG_tgz)

check-cran: 
	R_LIBS=/home/stefan/lib/R R CMD check $(R_PKG_tgz) --as-cran; \
	R_LIBS=/home/stefan/lib/R R-devel CMD check $(R_PKG_tgz) --as-cran

test: 
	R -e 'devtools::test()'

clean:
	rm $(R_PKG_tgz);\
	rm -rf libtmp/$(PACKAGE)


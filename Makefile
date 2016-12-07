.DEFAULT_GOAL := all

PKGFILES := $(shell find R data src man -type f ! -name "*.swp") DESCRIPTION NAMESPACE
PACKAGE := $(shell awk -F": +" '/^Package/ { print $$2 }' DESCRIPTION)
VERSION := $(shell awk -F": +" '/^Version/ { print $$2 }' DESCRIPTION)
R_PKG_tgz := $(PACKAGE)_$(VERSION).tar.gz

all: build install
build: $(R_PKG_tgz)
install: libtmp/$(PACKAGE)

$(R_PKG_tgz): $(PKGFILES)
	R --vanilla -e '.libPaths("/home/stefan/lib/R/")' -e 'Rcpp::compileAttributes()' && \
	R --vanilla -e '.libPaths("/home/stefan/lib/R/")' -e 'roxygen2::roxygenize(package.dir=".", clean=TRUE)' && \
	R CMD build .

libtmp/$(PACKAGE): $(R_PKG_tgz)
	mkdir -p libtmp;\
	R CMD INSTALL $(R_PKG_tgz) -l libtmp

.PHONY: clean
clean:
	rm $(R_PKG_tgz);\
	rm -rf libtmp/$(PACKAGE)


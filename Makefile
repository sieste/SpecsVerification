.DEFAULT_GOAL := all

R := R --vanilla -e '.libPaths("/home/stefan/lib/R")'

PKGFILES := $(shell find R data src man -type f ! -name "*.swp") DESCRIPTION NAMESPACE
PACKAGE := $(shell awk -F": +" '/^Package/ { print $$2 }' DESCRIPTION)
VERSION := $(shell awk -F": +" '/^Version/ { print $$2 }' DESCRIPTION)
R_PKG_tgz := $(PACKAGE)_$(VERSION).tar.gz

all: build install
build: $(R_PKG_tgz)
install: libtmp/$(PACKAGE)

$(R_PKG_tgz): $(PKGFILES)
	$(R) -e 'Rcpp::compileAttributes()' && \
	$(R) -e 'roxygen2::roxygenize(package.dir=".", clean=TRUE)' && \
	R CMD build .

libtmp/$(PACKAGE): $(R_PKG_tgz)
	mkdir -p libtmp;\
	R CMD INSTALL $(R_PKG_tgz) -l libtmp

check: $(R_PKG_tgz)
	R_LIBS=/home/stefan/lib/R R CMD check $(R_PKG_tgz)

.PHONY: clean
clean:
	rm $(R_PKG_tgz);\
	rm -rf libtmp/$(PACKAGE)


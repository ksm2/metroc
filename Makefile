exec := metroc
version := $(shell sed -En 's/^version:\s+(.*)/\1/p' metroc.cabal)
ghc_version := $(shell ghc --numeric-version)
builddir := dist
destdir := /usr/bin

all: clean update build

clean:
	cabal clean --builddir ${builddir}

update:
	cabal update

build:
	cabal build \
		--builddir ${builddir} \
		--enable-optimization=2 \
		--enable-tests \
		--enable-benchmarks \
		all

install:
	install -Dm0755 ${builddir}/build/x86_64-linux/ghc-${ghc_version}/${exec}-${version}/x/${exec}/opt/build/${exec}/${exec} ${destdir}/${exec}

all:
	cabal configure
	cabal new-build
	cp dist-newstyle/build/x86_64-linux/ghc-8.4.4/instant-0.0.0.1/x/jvm/build/jvm/jvm insc_jvm
	cp dist-newstyle/build/x86_64-linux/ghc-8.4.4/instant-0.0.0.1/x/llvm/build/llvm/llvm insc_llvm

clean:
	cabal clean
	rm -rf insc_llvm insc_jvm

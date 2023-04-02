TARGET=./flp22-fun
BUILD_DIR=./build
SRCS=./src/Main.hs ./src/Types.hs ./src/Helpers.hs ./src/Parser.hs ./src/BruteSolver.hs ./src/OptimizedSolver.hs ./src/State.hs
ARCHIVE="flp-fun-xstrba05.zip"

default:
	make flp22-fun

# compile binary
flp22-fun: ${SRCS}
	ghc -Wall --make ${SRCS} -o ${TARGET} -odir ${BUILD_DIR} -hidir ${BUILD_DIR}

# remove binary and build dir and then compile binary again
force:
	make clean
	make

# remove binary and build dir
clean:
	rm -rf ${TARGET} ${BUILD_DIR}

# test brute force
test_b:
	./test.sh b

# test optimized version
test_o:
	./test.sh o

# crate resulting archive
zip:
	zip ${ARCHIVE} ${SRCS} ./tests/* ./test.sh ./Makefile ./README.md

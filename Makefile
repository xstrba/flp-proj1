TARGET=./flp22-fun
BUILD_DIR=./build
SRCS=./src/Main.hs ./src/Types.hs ./src/Helpers.hs ./src/Parser.hs ./src/BruteSolver.hs ./src/OptimizedSolver.hs ./src/State.hs

default:
	make flp22-fun

flp22-fun: ${SRCS}
	ghc -Wall --make ${SRCS} -o ${TARGET} -odir ${BUILD_DIR} -hidir ${BUILD_DIR}

force:
	make clean
	make

clean:
	rm -rf ${TARGET} ${BUILD_DIR}

test_b:
	./test.sh b

test_o:
	./test.sh o
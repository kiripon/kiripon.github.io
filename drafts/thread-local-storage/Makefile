TARGET=aarch64-linux-none-musl
INCLUDE=-I/usr/aarch64-linux-gnu/include/c++/10.1.0/aarch64-linux-gnu/ 

all: tls.objdump tls-PIC.objdump

%.objdump: %.o
	llvm-objdump --demangle -dr $< > $@

%.o: %.cpp
	clang++ ${INCLUDE} -std=c++11 --target=${TARGET} -c $< -o $@

%-PIC.o: %.cpp
	clang++ ${INCLUDE} -std=c++11 -fPIC --target=${TARGET} -c $< -o $@

clean:
	rm *.o *.objdump
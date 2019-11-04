all: test_driver
	./test_driver

clean:
	rm -f Main scheme_entry.o scheme_entry.s test_driver.o test_driver
# The compiled haskell. Running ./Main will produce the desired asm
Main: scheme/app/Main.hs scheme/src/Compiler.hs
	ghc --make -o Main scheme/app/Main.hs scheme/src/Compiler.hs

# The asm file to represent the function `int scheme_entry() in our test driver
scheme_entry.s: Main
	./Main > scheme_entry.s

# Compiled asm
scheme_entry.o: scheme_entry.s
	as scheme_entry.s -o scheme_entry.o

# Compiled test_driver.c to object
test_driver.o:
	gcc -c c-test-driver/test_driver.c

# Final compiled test driver
test_driver: scheme_entry.o test_driver.o
	gcc -o test_driver test_driver.o scheme_entry.o



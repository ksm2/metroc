.PHONY : clean
.PHONY : run

clean :
	-rm Main Main.hi Main.o

Main :
	ghc -dynamic Main

run : Main
	./Main test.txt


game_of_life.x: game_of_life.f90 glider.o utilities.o lifecycle.o mod_initializeField.o
	gfortran -o game_of_life.x game_of_life.f90 glider.o utilities.o lifecycle.o mod_initializeField.o

glider.o: glider.f90 utilities.o
	gfortran -c glider.f90

utilities.o: utilities.f90
	gfortran -c utilities.f90

mod_initializeField.o: mod_initializeField.f90
	gfortran -c mod_initializeField.f90

lifecycle.o: lifecycle.f90
	gfortran -c lifecycle.f90

run:game_of_life.x
	./game_of_life.x

clean: 
	rm *.mod *.o main.x

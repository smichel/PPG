
game_of_life.x: game_of_life.f90 glider.o utilities.o mod_lifecycle.o mod_initializeField.o
	f95 -o game_of_life.x game_of_life.f90 glider.o utilities.o mod_lifecycle.o mod_initializeField.o

glider.o: glider.f90 utilities.o
	f95 -c glider.f90

utilities.o: utilities.f90
	f95 -c utilities.f90

mod_initializeField.o: mod_initializeField.f90
	f95 -c mod_initializeField.f90

mod_lifecycle.o: mod_lifecycle.f90
	f95 -c mod_lifecycle.f90

run:game_of_life.x
	./game_of_life.x

clean: 
	rm *.mod *.o game_of_life.x

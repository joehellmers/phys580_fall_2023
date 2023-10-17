FC    = gfortran -O3  -Wall -ffree-line-length-none
FCNOWARN = gfortran -O3 -ffree-line-length-none
FC77  = gfortran -O3 -Wall

EXECS =   statstar
MODS = constants.mod bc.mod userio.mod zones.mod composition.mod ssequations.mod physics.mod ode.mod
OBJS = constants.o bc.o userio.o zones.o composition.o ssequations.o physics.o ode.o

default: $(EXECS)

all: $(EXECS)

statstar: statstar.f90 $(MODS)
	$(FC) statstar.f90 $(OBJS) -o statstar

constants.mod: constants.f90
	$(FC) -c constants.f90
    
composition.mod: composition.f90
	$(FC) -c composition.f90
    
ssequations.mod: ssequations.f90
	$(FC) -c ssequations.f90
    
physics.mod: physics.f90 zones.mod
	$(FC) -c physics.f90
    
bc.mod: bc.f90 composition.mod physics.mod ssequations.mod
	$(FC) -c bc.f90
   
zones.mod: zones.f90
	$(FC) -c zones.f90
    
ode.mod: ode.f90
	$(FC) -c ode.f90

userio.mod: userio.f90
	$(FC) -c userio.f90
    
.PHONY : clean

clean :
	rm -f $(EXECS) $(MODS) $(OBJS) starstat.dat ZAMSmodel.txt model.dat
	rm -rf .ipynb_checkpoints/

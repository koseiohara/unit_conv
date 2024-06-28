EXE = unit_conv
OBJS = NaNchecker.o fileio.o globals.o namelist.o change_unit.o main.o

FC = ifort
FLAGS = -warn all -O0 -convert little_endian -assume byterecl -traceback -check all

%.o : %.f90
	${FC} -c $< ${FLAGS}

all : ${EXE}

${EXE} : ${OBJS}
	${FC} -o $@ $^


.PHONY : clean re

clean : 
	rm -fv *.o *.mod ${EXE}

re : clean all


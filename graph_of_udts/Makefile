HDF5_HOME = /Users/miller86/silo/hdf5-1.10.0-patch1/build/my_install

SRC = udt_data_utils.c
OBJ = $(SRC:.c=.o)

%.o : %.c
	$(CC) -c $< $(CFLAGS) $(CPPFLAGS) -I. -I$(HDF5_HOME)/include

udt_graph1 : udt_graph1.o $(OBJ)
	$(CC) -o $@ $< $(OBJ) -L$(HDF5_HOME)/lib -lhdf5 $(LDFLAGS) $(LIBS)

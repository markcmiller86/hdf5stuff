HDF5_HOME = /Users/miller86/silo/hdf5-1.10.0-patch1/build/my_install

SRC = udt_data_utils.c
HDR = $(SRC:.c=.h)
OBJ = $(SRC:.c=.o)

%.o : %.c
	$(CC) -c $< $(CFLAGS) $(CPPFLAGS) -I. -I$(HDF5_HOME)/include

all: udt_graph1 udt_graph2

udt_graph1.o : udt_graph1.c

udt_graph1 : udt_graph1.o $(OBJ) $(HDR) $(SRC)
	$(CC) -o $@ $< $(OBJ) -L$(HDF5_HOME)/lib -lhdf5 $(LDFLAGS) $(LIBS)

udt_graph2.o : udt_graph2.c

udt_graph2 : udt_graph2.o $(OBJ) $(HDR) $(SRC)
	$(CC) -o $@ $< $(OBJ) -L$(HDF5_HOME)/lib -lhdf5 $(LDFLAGS) $(LIBS)

clean:
	rm -rf $(OBJ) udt_graph1.o udt_graph2.o
	rm -rf udt_graph1 udt_graph2

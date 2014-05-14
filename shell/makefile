OBJS=environ.o myshell.o LineParser.o
CFLAGS= -Wall -c
LFLAGS=-melf_i386
PROG=myshell

$(PROG): $(OBJS)
	gcc $(LFLAGS) -o $(PROG) $(OBJS)

#start.o: start.s
#	nasm -f elf start.s -o start.o

myshell.o: myshell.c
	gcc $(CFLAGS) myshell.c -o myshell.o

environ.o: environ.c environ.h
	gcc $(CFLAGS) environ.c -o environ.o

LineParser.o: LineParser.c LineParser.h
	gcc $(CFLAGS) LineParser.c -o LineParser.o

clean:
	rm $(OBJS) $(PROG)
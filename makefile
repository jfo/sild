SHELL = /bin/sh
CC = gcc
OBJDIR=obj
vpath %.c src

objects = $(addprefix $(OBJDIR)/, util.o cell.o eval.o builtins.o print.o read.o env.o main.o)

sild: CFLAGS = -std=c11
sild: $(objects)
	$(CC) $(CFLAGS) -o sild $(objects)

debug: CFLAGS = -Wall -Werror -g
debug: $(objects)
	$(CC) $(CFLAGS) -o sild $(objects)

$(OBJDIR)/%.o: %.c $(OBJDIR)
	$(CC) -c $(CFLAGS) $< -o $@

$(OBJDIR):
	mkdir $(OBJDIR)

.PHONY: clean run
clean:
	rm -r sild sild.dSYM $(OBJDIR)

run: debug
	./sild

install: sild
	cp ./sild /usr/local/bin/

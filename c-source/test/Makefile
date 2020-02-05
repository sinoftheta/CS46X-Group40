CC=gcc
# -lgsl -lgslcblas
CFLAGS := -Iinclude -pedantic -Wall -Werror -g -std=c99
CLIBS := -Llib -static -lgsl -lgslcblas -lm

SRCDIR := .
BUILDDIR := testbuild/objs

SRCS := $(shell find $(SRCDIR) -name '*.c')
OBJS := $(addprefix $(BUILDDIR)/,$(SRCS:%.c=%.o))

TARGET := testbuild/test

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS)  $^ -o $@ $(CLIBS)

$(BUILDDIR)/%.o: %.c
	mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@
 
clean: 
	rm -r $(BUILDDIR)
	rm $(TARGET)

run:
	$(TARGET)

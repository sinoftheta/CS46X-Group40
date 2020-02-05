CC=gcc
# -lgsl -lgslcblas
CFLAGS := -Iinclude -pedantic -Wall -Werror -g -std=c99
CLIBS := -Llib  -lgsl -lgslcblas -lm

SRCDIR := src
BUILDDIR := build/objs

SRCS := $(shell find $(SRCDIR) -name '*.c')
OBJS := $(addprefix $(BUILDDIR)/,$(SRCS:%.c=%.o))

LIB_SRCS := $(filter-out src/main.c, $(SRCS))
LIB_OBJS := $(filter-out build/objs/main.o, $(OBJS))

TARGET := build/gs2
LIBRARY := build/libgs2.so


all: $(TARGET)

library: $(LIBRARY)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS)  $^ -o $@ $(CLIBS)

$(LIBRARY): $(LIB_OBJS)
	$(CC) $(CFLAGS) -shared $^ -o $@ $(CLIBS)

$(BUILDDIR)/%.o: %.c
	mkdir -p $(@D)
	$(CC) $(CFLAGS) -fPIC -c $< -o $@
 
clean: 
	rm -r $(BUILDDIR)
	rm $(TARGET)

run:
	$(TARGET)

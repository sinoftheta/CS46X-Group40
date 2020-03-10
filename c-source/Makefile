CC=gcc
CFLAGS := -pedantic -Wall -Werror -g -std=c99 # -DDO_DEBUG
CLIBS := -lgsl -lgslcblas -lm

SRCDIR := src
BUILDDIR := build/objs

SRCS = $(wildcard src/*.c)
SRCS += $(wildcard src/gs2/*.c)
SRCS += $(wildcard src/capstone/*.c)

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
	rm $(TARGET)*

run:
	$(TARGET)

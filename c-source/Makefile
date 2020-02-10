CC=gcc
# -lgsl -lgslcblas
CFLAGS := -pedantic -Wall -Werror -g -std=c99
CLIBS := -lgsl -lgslcblas -lm

SRCDIR := src
BUILDDIR := build/objs

# SRCS := $(shell find $(SRCDIR) -name '*.c')
SRCS = $(wildcard src/*.c)
SRCS += $(wildcard src/gs2/*.c)
SRCS += $(wildcard src/capstone/*.c)

OBJS := $(addprefix $(BUILDDIR)/,$(SRCS:%.c=%.o))

TARGET := build/gs2

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS)  $^ -o $@ $(CLIBS)

$(BUILDDIR)/%.o: %.c
	mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@
 
clean: 
	rm -r $(BUILDDIR)
	rm $(TARGET).*

run:
	$(TARGET)

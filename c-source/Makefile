CC=gcc
# -lgsl -lgslcblas
CFLAGS := -Iinclude -pedantic -Wall -Werror -g -std=c99
CLIBS := -Llib -static -lgsl -lgslcblas -lm

SRCDIR := src
BUILDDIR := build/objs

SRCS := $(shell find $(SRCDIR) -name '*.c')
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
	rm $(TARGET)

run:
	$(TARGET)

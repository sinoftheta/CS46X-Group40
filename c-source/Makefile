CC=gcc
# -lgsl -lgslcblas
CFLAGS := -lm  -Iinclude -pedantic -Wall -Werror -g -std=c99
CLIBS := -static -Llib/gsl -static -Llib/gslcblas

SRCDIR := src
BUILDDIR := build/objs

SRCS := $(shell find $(SRCDIR) -name '*.c')
OBJS := $(addprefix $(BUILDDIR)/,$(SRCS:%.c=%.o))

TARGET := build/gs2

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $(CLIBS) $^ -o $@ -lm


$(BUILDDIR)/%.o: %.c
	mkdir -p $(@D)
	$(CC) $(CFLAGS) $(CLIBS) -c $< -o $@
 
clean: 
	rm -r $(BUILDDIR)
	rm $(TARGET)

run:
	$(TARGET)

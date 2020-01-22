CC=gcc
CFLAGS := -pedantic -Wall -Werror -g
CLIBS :=

SRCDIR := src
BUILDDIR := build/objs

SRCS := $(shell find $(SRCDIR) -name '*.c')
OBJS := $(addprefix $(BUILDDIR)/,$(SRCS:%.c=%.o))

TARGET := build/gs2

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $(CLIBS) $^ -o $@


$(BUILDDIR)/%.o: %.c
	mkdir -p $(@D)
	$(CC) $(CFLAGS) $(CLIBS) -c $< -o $@

clean: 
	rm -r $(BUILDDIR)
	rm $(TARGET)

run:
	$(TARGET)
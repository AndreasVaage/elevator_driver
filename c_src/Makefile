# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))


C_SRC_OUTPUT ?= $(CURDIR)/../priv/elevator_driver

# System type and C compiler/flags.

C_SRC_DIR = $(CURDIR)
CC ?= gcc
CFLAGS ?= -g -std=gnu11 -Wall -Wmissing-prototypes
LDFLAGS = -lcomedi -pthread -lm -lerl_interface -lei -lnsl


SOURCES := $(shell find $(C_SRC_DIR) -name '*.c')
OBJECTS = $(SOURCES:%.c=%.o)

# program executable file name.
TARGET = $(C_SRC_OUTPUT)

# top-level rule, to compile everything.
all:$(TARGET)

# rule to link the program
$(TARGET): $(OBJECTS)
	$(CC) -o $@ $^ $(LDFLAGS)

# Compile: create object files from C source files.
%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

# rule for cleaning re-compilable files.
clean:
	rm -f $(TARGET) $(OBJECTS)

rebuild:
	clean all

.PHONY: all rebuild clean




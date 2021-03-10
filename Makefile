#
# ┌───────────────────────────────────────────────────────────────┐
# │ RJH: A dead simple fortran project makefile for parse_statis. │
# │      (yes, overkill, copied from another project...!).        │
# └───────────────────────────────────────────────────────────────┘
#

FC       := gfortran
FFLAGS   := -Wall --std=f2003
FDEBUG   := -g -Wextra -Werror
FRELEASE := -O3
LINKER   := $(FC) -o
# In principle linker can have different flags.
# FCLINKS  := -g -Wall -Wextra -Werror
ARCH     := $(shell uname -m)

BUILD    := ./build-$(ARCH)
OBJ_DIR  := $(BUILD)/obj
APP_DIR  := $(BUILD)/bin

TARGET   := parse_statis

SRC_DIR := .
SRC     := $(wildcard $(SRC_DIR)/*.f03)
OBJ     := $(SRC:%.f03=$(OBJ_DIR)/%.o)

.DEFAULT_GOAL := all

red := $(shell echo "\033[0;31m")
grn := $(shell echo "\033[0;32m")
yel := $(shell echo "\033[0;33m")
blu := $(shell echo "\033[0;34m")
mag := $(shell echo "\033[0;35m")
cya := $(shell echo "\033[0;36m")
noc := $(shell echo "\033[0m")

.PHONY: all clean vclean remake

# Make all.
all: $(APP_DIR)/$(TARGET)

debug: FFLAGS += $(FDEBUG)
debug: TARGET += _DEBUG
debug: all

release: FFLAGS += $(FRELEASE)
release: all

# Objects.
$(OBJ_DIR)/%.o : %.f03
	@echo -n "$(grn)"
	@echo "[O] Making $@ from $< in $(@D)."
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -J$(OBJ_DIR)
	@echo -n "$(noc)"

# Linking objects/mods to target.
$(APP_DIR)/$(TARGET) : $(OBJ)
	@echo -n "$(blu)"
	@echo "[T] Making $@ from $< in $(@D)."
	@mkdir -p $(@D)
	$(LINKER) $@ $(OBJ) $(FFLAGS) -J$(OBJ_DIR)
	@echo -n "$(noc)"

# Clean objects/mods.
clean :
	@echo -n "$(yel)"
	-@rm -rvf $(OBJ_DIR)
	@echo -n "$(noc)"

# Clean everything.
vclean :
	@echo -n "$(red)"
	-@rm -rvf $(BUILD)
	@echo -n "$(noc)"

# vclean, redo.
remake : vclean all


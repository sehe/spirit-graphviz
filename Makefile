# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.5

# Default target executed when no arguments are given to make.
default_target: all

.PHONY : default_target

# Allow only one "make -f Makefile2" at a time, but pass parallelism.
.NOTPARALLEL:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/sehe/Projects/spirit-graphviz

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/sehe/Projects/spirit-graphviz

#=============================================================================
# Targets provided globally by CMake.

# Special rule for the target edit_cache
edit_cache:
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --cyan "No interactive CMake dialog available..."
	/usr/bin/cmake -E echo No\ interactive\ CMake\ dialog\ available.
.PHONY : edit_cache

# Special rule for the target edit_cache
edit_cache/fast: edit_cache

.PHONY : edit_cache/fast

# Special rule for the target rebuild_cache
rebuild_cache:
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --cyan "Running CMake to regenerate build system..."
	/usr/bin/cmake -H$(CMAKE_SOURCE_DIR) -B$(CMAKE_BINARY_DIR)
.PHONY : rebuild_cache

# Special rule for the target rebuild_cache
rebuild_cache/fast: rebuild_cache

.PHONY : rebuild_cache/fast

# The main all target
all: cmake_check_build_system
	$(CMAKE_COMMAND) -E cmake_progress_start /home/sehe/Projects/spirit-graphviz/CMakeFiles /home/sehe/Projects/spirit-graphviz/CMakeFiles/progress.marks
	$(MAKE) -f CMakeFiles/Makefile2 all
	$(CMAKE_COMMAND) -E cmake_progress_start /home/sehe/Projects/spirit-graphviz/CMakeFiles 0
.PHONY : all

# The main clean target
clean:
	$(MAKE) -f CMakeFiles/Makefile2 clean
.PHONY : clean

# The main clean target
clean/fast: clean

.PHONY : clean/fast

# Prepare targets for installation.
preinstall: all
	$(MAKE) -f CMakeFiles/Makefile2 preinstall
.PHONY : preinstall

# Prepare targets for installation.
preinstall/fast:
	$(MAKE) -f CMakeFiles/Makefile2 preinstall
.PHONY : preinstall/fast

# clear depends
depend:
	$(CMAKE_COMMAND) -H$(CMAKE_SOURCE_DIR) -B$(CMAKE_BINARY_DIR) --check-build-system CMakeFiles/Makefile.cmake 1
.PHONY : depend

#=============================================================================
# Target rules for targets named spirit-graphviz

# Build rule for target.
spirit-graphviz: cmake_check_build_system
	$(MAKE) -f CMakeFiles/Makefile2 spirit-graphviz
.PHONY : spirit-graphviz

# fast build rule for target.
spirit-graphviz/fast:
	$(MAKE) -f CMakeFiles/spirit-graphviz.dir/build.make CMakeFiles/spirit-graphviz.dir/build
.PHONY : spirit-graphviz/fast

spirit-graphviz.o: spirit-graphviz.cpp.o

.PHONY : spirit-graphviz.o

# target to build an object file
spirit-graphviz.cpp.o:
	$(MAKE) -f CMakeFiles/spirit-graphviz.dir/build.make CMakeFiles/spirit-graphviz.dir/spirit-graphviz.cpp.o
.PHONY : spirit-graphviz.cpp.o

spirit-graphviz.i: spirit-graphviz.cpp.i

.PHONY : spirit-graphviz.i

# target to preprocess a source file
spirit-graphviz.cpp.i:
	$(MAKE) -f CMakeFiles/spirit-graphviz.dir/build.make CMakeFiles/spirit-graphviz.dir/spirit-graphviz.cpp.i
.PHONY : spirit-graphviz.cpp.i

spirit-graphviz.s: spirit-graphviz.cpp.s

.PHONY : spirit-graphviz.s

# target to generate assembly for a file
spirit-graphviz.cpp.s:
	$(MAKE) -f CMakeFiles/spirit-graphviz.dir/build.make CMakeFiles/spirit-graphviz.dir/spirit-graphviz.cpp.s
.PHONY : spirit-graphviz.cpp.s

# Help Target
help:
	@echo "The following are some of the valid targets for this Makefile:"
	@echo "... all (the default if no target is provided)"
	@echo "... clean"
	@echo "... depend"
	@echo "... edit_cache"
	@echo "... spirit-graphviz"
	@echo "... rebuild_cache"
	@echo "... spirit-graphviz.o"
	@echo "... spirit-graphviz.i"
	@echo "... spirit-graphviz.s"
.PHONY : help



#=============================================================================
# Special targets to cleanup operation of make.

# Special rule to run CMake to check the build system integrity.
# No rule that depends on this can have commands that come from listfiles
# because they might be regenerated.
cmake_check_build_system:
	$(CMAKE_COMMAND) -H$(CMAKE_SOURCE_DIR) -B$(CMAKE_BINARY_DIR) --check-build-system CMakeFiles/Makefile.cmake 0
.PHONY : cmake_check_build_system


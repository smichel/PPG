# Generic Makefile
# Copyright (C) 2012 Nathanael Hübbe
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
# You can contact the authors of this programm via sourceforge:
# http://sourceforge.net/projects/brilliantasreal/

###############
# This is a generic Makefile. It does not contain any information on the project to be build. Instead it reads rules.mk files containing the required information in a very concise format and defines the rules accordingly.
#
#
# The files that contain the project information are:
#
# definitions.mk
# This file is included by the Makefile to define some global variables that control the compiler/linker options and where build products are placed.
#
# rules.mk
# This file is included by the Makefile to set a few variables that describe how the rules are constructed. It may also set the subdirs variable to tell the Makefile, what other directories belong to the project, the rules.mk files in these directories are parsed in a recursive fashion.
#
###############
# definitions.mk
# The following globals are expected to be defined (do not forget the export directive!):
#
# export builddir := build	# The directory into which to place the intermediary products.
# export productsdir := products	# The directory into which to place the final products.
#
# export includeFlags := -I $(rootdir)/A -I $(rootdir)/B
# export compileFlags := -Wall
# export deploymentOptimizationLevel := -O2	# The generic Makefile knows about optimization levels, and builds them completely separate from each other. This allows for quickly changing between performance testing with -O2 and debugging with -O0 without any need of recompilation when you switch back! This variable simply tells the Makefile, which version it should build by default and point the generic symlink to.
#
# export libFlags :=
# export linkFlags :=
#
# export debugCompilation := 1	# Set this variable to see the actual compilation command lines.
# export debugLinking := 1	# Set this variable to see the actual compilation command lines.
#
# export checkTodo := true	# Comment out to skip grepping for TODO and FIXME after building.
# export useColor := true	# Comment out if you don't like colors for some reason.
#
# $(rootdir) can be expected to contain the absolut path of the make invocation.
#
###############
# rules.mk
# All files/directories named by these files are taken to be paths relative to the directory containing the rules.mk file. The meaningful variables that can be set here are:
#
# sources
# Files named here are used to generate .o files. Dependency file generation happens completely automatically. Make sure to write it as a direct varible assingment! Looks like this:
# sources := main.c
#
# executables
# This variable lists the executeable files that are to be build. These will be placed directly into the $(productsdir). Make sure to write it as a direct varible assingment! Looks like this:
# executables := helloWorld
# The required objects are communicated via a corresponding xxx_objects variable:
# helloWorld_objects := main.o ../shared/foo.o
#
# $(rootdir) can be expected to contain the absolut path of the make invocation.

#################
# Start of code #
#################

# So that the .mk can also define rules without these rules becoming the default rule. However, they can add dependencies to the defaultTarget.
defaultTarget: genericMakefileAll

### Get the globals.

export optimizationLevels := -O0 -O1 -O2 -O3 -Os
export rootdir := $(shell pwd)
#some defaults that need to be set so that we won't do something stupid should definitions.mk not exist.
export builddir := build
export productsdir := .
export cppCompiler := gcc
export fortranCompiler := gfortran
include definitions.mk

### Parse the rules files.

# Returns all but the first word of its argument.
stripfirstword = $(wordlist 2,$(words $(1)),$(1))

# (call convertPaths,prefix,paths)
# Takes paths relative to prefix and returns paths relative to the Makefile.
convertPaths = $(subst $(rootdir)/,,$(abspath $(addprefix $(1)/,$(2))))

# (call parseRulesFile,directory)
# Clears the sources, executables and subdir variables,
# includes the file at directory/rules.mk,
# adds the contents of sources and executables to the all_* variables (with prefix),
# makes the *_objects path relative to the Makefile,
# and returns the contents of subdirs (with prefix).
parseRulesFile = $(eval all_rulefiles := $(all_rulefiles) $(1)/rules.mk)\
	$(eval sources := )\
	$(eval executables := )\
	$(eval libraries := )\
	$(eval subdirs := )\
	$(info Parsing $(1)/rules.mk)\
	$(eval include $(1)/rules.mk)\
	$(eval all_sources := $(all_sources) $(call convertPaths,$(1),$(sources)))\
	$(eval c_sources := $(c_sources) $(wildcard $(1)/*.c))\
	$(eval cpp_sources := $(cpp_sources) $(wildcard $(1)/*.cpp))\
	$(eval fortran77_sources := $(fortran_sources) $(wildcard $(1)/*.f77))\
	$(eval fortran90_sources := $(fortran_sources) $(wildcard $(1)/*.f90))\
	$(eval fortran95_sources := $(fortran_sources) $(wildcard $(1)/*.f95))\
	$(eval headers := $(headers) $(wildcard $(1)/*.h))\
	$(eval all_executables := $(all_executables) $(executables))\
	$(eval all_libraries := $(all_libraries) $(libraries))\
	$(foreach executeable,$(executables),$(eval $(executeable)_objects := $(call convertPaths,$(1),$($(executeable)_objects))))\
	$(foreach executeable,$(executables),$(eval $(executeable)_libraries := $(call convertPaths,$(1),$($(executeable)_libraries))))\
	$(foreach library,$(library),$(eval $(library)_objects := $(call convertPaths,$(1),$($(library)_objects))))\
	$(call convertPaths,$(1),$(subdirs))

# (call parseRules,directories)
# Call parseRulesFile for all given directories and subdirectories the rule files may name.
parseRules = $(if $(firstword $(1)),$(firstword $(1)) \
	$(call parseRules,$(call parseRulesFile,$(firstword $(1))) $(call stripfirstword,$(1))))

# Read the rules.mk files.
trash := $(call parseRules,.)

# Compute the final file lists.
all_sources := $(c_sources) $(cpp_sources) $(fortran77_sources) $(fortran90_sources) $(fortran95_sources)
c_objects := $(call convertPaths,$(builddir)/,$(c_sources:.c=.o))
cpp_objects := $(call convertPaths,$(builddir)/,$(cpp_sources:.cpp=.o))
fortran77_objects := $(call convertPaths,$(builddir)/,$(fortran77_sources:.f77=.o))
fortran90_objects := $(call convertPaths,$(builddir)/,$(fortran90_sources:.f90=.o))
fortran95_objects := $(call convertPaths,$(builddir)/,$(fortran95_sources:.f95=.o))
all_objects := $(c_objects) $(cpp_objects) $(fortran77_objects) $(fortran90_objects) $(fortran95_objects)

c_dependencyFiles := $(c_objects:.o=.d)
cpp_dependencyFiles := $(cpp_objects:.o=.d)
fortran77_dependencyFiles := $(fortran77_objects:.o=.d)
fortran90_dependencyFiles := $(fortran90_objects:.o=.d)
fortran95_dependencyFiles := $(fortran95_objects:.o=.d)
all_dependencyFiles := $(all_objects:.o=.d)

plain_products := $(call convertPaths,$(productsdir),$(all_executables)) \
	$(addsuffix .la,$(call convertPaths,$(productsdir),$(all_libraries)))

all_products := $(plain_products) $(foreach oLevel,$(optimizationLevels),\
	$(foreach product,$(call convertPaths,$(productsdir),$(all_executables)),$(product)$(oLevel)) \
	$(foreach product,$(call convertPaths,$(productsdir),$(all_libraries)),\
		$(product)$(oLevel).la \
		$(dir $(product)).libs/$(notdir $(product))$(oLevel).a \
		$(dir $(product)).libs/$(notdir $(product))$(oLevel).la \
	)\
)


### Define the rules.

genericMakefileAll: $(plain_products)

# (call makeDependencyRule,dependencyFiles,sourceExtension)
makeDependencyRule = $(eval $(1):$(builddir)/%.d:%$(2) .makeDependencies; \
	@echo making dependencies of $$*$(2); \
	./.makeDependencies $$(dir $$*) $$(notdir $$*)$(2) $$(notdir $$*).o $$(notdir $$*).d)

$(call makeDependencyRule,$(c_dependencyFiles),.c)
$(call makeDependencyRule,$(cpp_dependencyFiles),.cpp)
$(call makeDependencyRule,$(fortran77_dependencyFiles),.f77)
$(call makeDependencyRule,$(fortran90_dependencyFiles),.f90)
$(call makeDependencyRule,$(fortran95_dependencyFiles),.f95)

# Ignore it when a header disappears/moves
%.h:;@echo ignoring missing header $*.h

include $(all_dependencyFiles)

makeCompileRule = $(foreach oLevel,$(optimizationLevels),\
	$(eval $(1:.o=$(oLevel).o):$(builddir)/%$(oLevel).o: definitions.mk Makefile .compile; \
		@echo compiling $$*$(2) \($(oLevel)\); \
		compileFlags="$(compileFlags) $(oLevel)" ; \
		./.compile $$(dir $$*) $$(notdir $$*)$(2) $$(notdir $$*)$(oLevel).o) \
	$(eval $(1:.o=$(oLevel).lo):$(builddir)/%$(oLevel).lo: definitions.mk Makefile .compile; \
		@echo compiling $$*$(2) \($(oLevel)\); \
		compileFlags="$(compileFlags) $(oLevel)" ; \
		./.compile $$(dir $$*) $$(notdir $$*)$(2) $$(notdir $$*)$(oLevel).lo)) \

$(call makeCompileRule,$(c_objects),.c)
$(call makeCompileRule,$(cpp_objects),.cpp)
$(call makeCompileRule,$(fortran77_objects),.f77)
$(call makeCompileRule,$(fortran90_objects),.f90)
$(call makeCompileRule,$(fortran95_objects),.f95)

#TODO: Interpret $(product)_objects relatively.
$(foreach product,$(all_executables), \
	$(eval curObjects := $(call convertPaths,$(builddir),$($(product)_objects)))\
	$(eval curLibraries := $(call convertPaths,$(productsdir),$($(product)_libraries)))\
	$(foreach oLevel,$(optimizationLevels), \
		$(eval $(productsdir)/$(product)$(oLevel): $(curObjects:.o=$(oLevel).o) $(curLibraries:=$(oLevel).la) $(all_rulefiles) .link; \
			@echo linking $(product) \($(oLevel)\);\
			env linkerCommand="$(or $($(product)_linkCommand),gcc)" ./.link $(product)$(oLevel) $(curObjects:.o=$(oLevel).o) $(curLibraries:=$(oLevel).la))) \
	$(eval $(productsdir)/$(product): $(productsdir)/$(product)$(deploymentOptimizationLevel); \
		@rm -f $(productsdir)/$(product) ; \
		ln --symbolic $(product)$(deploymentOptimizationLevel) $(productsdir)/$(product)))

#TODO: Interpret $(product)_objects relatively.
$(foreach product,$(all_libraries), \
	$(eval curObjects := $(call convertPaths,$(builddir),$($(product)_objects)))\
	$(foreach oLevel,$(optimizationLevels), \
		$(eval $(productsdir)/$(product)$(oLevel).la: $(curObjects:.lo=$(oLevel).lo) $(all_rulefiles) .link; \
			@echo linking $(product).la \($(oLevel)\);\
			env linkerCommand="$(or $($(product)_linkCommand),gcc)" ./.link $(product)$(oLevel).la $(curObjects:.lo=$(oLevel).lo))) \
	$(eval $(productsdir)/$(product).la: $(productsdir)/$(product)$(deploymentOptimizationLevel).la; \
		@rm -f $(productsdir)/$(product).la ; \
		ln --symbolic $(product)$(deploymentOptimizationLevel).la $(productsdir)/$(product).la))

definitions.mk:
	@ ( echo 'export builddir := build'; \
	echo 'export productsdir := .'; \
	echo ''; \
	echo 'export includeFlags := '; \
	echo 'export compileFlags := '; \
	echo 'export deploymentOptimizationLevel := -O2'; \
	echo ''; \
	echo '# The cppCompiler is also used for C files.'; \
	echo 'export cppCompiler := g++'; \
	echo 'export fortranCompiler := gfortran'; \
	echo ''; \
	echo 'export libFlags :='; \
	echo 'export linkFlags :='; \
	echo ''; \
	echo '# Set this variable to see the actual compilation command lines.'; \
	echo '# export debugCompilation := 1'; \
	echo '# Set this variable to see the actual linking command lines.'; \
	echo '# export debugLinking := 1'; \
	echo ''; \
	echo '# Comment out to skip grepping for TODO and FIXME after building.'; \
	echo 'export checkTodo := true'; \
	echo '# Comment out if you do not like colors for some reason.'; \
	echo 'export useColor := true'; ) > definitions.mk

.compile: Makefile
	@ ( echo '#! /bin/bash'; \
	echo '# Generic Makefile'; \
	echo '# Copyright (C) 2012,2013 Nathanael Hübbe'; \
	echo '#'; \
	echo '# This program is free software: you can redistribute it and/or modify'; \
	echo '# it under the terms of the GNU General Public License as published by'; \
	echo '# the Free Software Foundation, either version 3 of the License, or'; \
	echo '# (at your option) any later version.'; \
	echo '#'; \
	echo '# This program is distributed in the hope that it will be useful,'; \
	echo '# but WITHOUT ANY WARRANTY; without even the implied warranty of'; \
	echo '# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the'; \
	echo '# GNU General Public License for more details.'; \
	echo '#'; \
	echo '# You should have received a copy of the GNU General Public License'; \
	echo '# along with this program.  If not, see <http://www.gnu.org/licenses/>.'; \
	echo '# '; \
	echo '# You can contact the authors of this programm via sourceforge:'; \
	echo '# http://sourceforge.net/projects/brilliantasreal/'; \
	echo ; \
	echo '############'; \
	echo '# Compile a source file located in a subdirectory.'; \
	echo '############'; \
	echo '# $$compileFlags: The options to be passed to the compiler.'; \
	echo '# $$includeFlags: More options to be passed to the compiler.'; \
	echo '# $$rootdir: The path the argument paths are relative to.'; \
	echo '# $$builddir: The directory to place the object file.'; \
	echo '#'; \
	echo '# $$1: The subdirectory of the source, relative to the root direktory of the Makefile.'; \
	echo '# $$2: The name of the source file.'; \
	echo '# $$3: The name of the corresponding object file.'; \
	echo '#'; \
	echo '# The effective path of the object file is of the form'; \
	echo '# $$rootdir/builddir/subdir/filename'; \
	echo '# The source file is found at'; \
	echo '# $$rootdir/subdir/filename'; \
	echo '############'; \
	echo ''; \
	echo 'subdir=$$1'; \
	echo 'sourcefile=$$2'; \
	echo 'objfile=$$3'; \
	echo ''; \
	echo 'cd $$rootdir'; \
	echo 'mkdir -p $$builddir/$$subdir'; \
	echo 'mkdir -p $$builddir/.modules'; \
	echo 'cd $$subdir'; \
	echo ''; \
	echo 'if [[ $$debugCompilation != "" ]] ; then'; \
	echo '	libtoolCommand="libtool --mode=compile"'; \
	echo 'else'; \
	echo '	libtoolCommand="libtool --quiet --mode=compile"'; \
	echo 'fi'; \
	echo ''; \
	echo 'libtoolCheck=$${objfile%.lo}'; \
	echo 'fortranCheck=$${sourcefile%.f{77,90,95}}'; \
	echo ''; \
	echo 'if [[ $$libtoolCheck != $$objfile ]] ; then'; \
	echo '	if [[ $$fortranCheck == $$sourcefile ]] ; then'; \
	echo '		commandLine="$$libtoolCommand $$cppCompiler -c -o $$rootdir/$$builddir/$$subdir/$${objfile%.lo}.o $$compileFlags $$includeFlags $$sourcefile"'; \
	echo '	else'; \
	echo '		commandLine="$$libtoolCommand $$fortranCompiler -cpp -c -J $$rootdir/$$builddir/.modules -o $$rootdir/$$builddir/$$subdir/$${objfile%.lo}.o $$compileFlags $$includeFlags $$sourcefile"'; \
	echo '	fi'; \
	echo 'else'; \
	echo '	if [[ $$fortranCheck == $$sourcefile ]] ; then'; \
	echo '		commandLine="$$cppCompiler -c -o $$rootdir/$$builddir/$$subdir/$$objfile $$compileFlags $$includeFlags $$sourcefile"'; \
	echo '	else'; \
	echo '		commandLine="$$fortranCompiler -cpp -c -J $$rootdir/$$builddir/.modules -o $$rootdir/$$builddir/$$subdir/$$objfile $$compileFlags $$includeFlags $$sourcefile"'; \
	echo '	fi'; \
	echo 'fi'; \
	echo 'if [[ $$debugCompilation != "" ]] ; then'; \
	echo '	echo in \"$$subdir\": $$commandLine'; \
	echo 'fi'; \
	echo '$$commandLine' ) > .compile
	@ chmod 755 .compile

.link: Makefile
	@ ( echo '#! /bin/bash'; \
	echo '# BrilliantAsReality - A raytracer combining simplicity and power.'; \
	echo '# Copyright (C) 2012 Nathanael Hübbe'; \
	echo '#'; \
	echo '# This program is free software: you can redistribute it and/or modify'; \
	echo '# it under the terms of the GNU General Public License as published by'; \
	echo '# the Free Software Foundation, either version 3 of the License, or'; \
	echo '# (at your option) any later version.'; \
	echo '#'; \
	echo '# This program is distributed in the hope that it will be useful,'; \
	echo '# but WITHOUT ANY WARRANTY; without even the implied warranty of'; \
	echo '# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the'; \
	echo '# GNU General Public License for more details.'; \
	echo '#'; \
	echo '# You should have received a copy of the GNU General Public License'; \
	echo '# along with this program.  If not, see <http://www.gnu.org/licenses/>.'; \
	echo '# '; \
	echo '# You can contact the authors of this programm via sourceforge:'; \
	echo '# http://sourceforge.net/projects/brilliantasreal/'; \
	echo ''; \
	echo '############'; \
	echo '# Link several object files together.'; \
	echo '############'; \
	echo '# $$linkerCommand: The command to be used as the linker.'; \
	echo '# $$linkFlags: The options to be passed to the linker.'; \
	echo '# $$libFlags: More options to be passed to the linker.'; \
	echo '# $$rootdir: The path the argument paths are relative to.'; \
	echo '# $$productsdir: The directory to place the linked file.'; \
	echo '#'; \
	echo '# $$1: The name of the output file.'; \
	echo '# $$...: The paths of the object files.'; \
	echo '#'; \
	echo '# The effective path of the object files is of the form'; \
	echo '# $$rootdir/filepath'; \
	echo '# The result is placed at'; \
	echo '# $$rootdir/productsdir/filename'; \
	echo '############'; \
	echo ''; \
	echo 'cd $$rootdir'; \
	echo 'mkdir -p $$productsdir'; \
	echo 'outputFile=$$1'; \
	echo 'shift 1'; \
	echo ; \
	echo 'libtoolTag='; \
	echo 'case $$linkerCommand in'; \
	echo '	(*fort*) libtoolTag="--tag=FC";;'; \
	echo '	(*f77*) libtoolTag="--tag=FC";;'; \
	echo '	(*f90*) libtoolTag="--tag=FC";;'; \
	echo '	(*f95*) libtoolTag="--tag=FC";;'; \
	echo '	(*cc*) libtoolTag="--tag=CC";;'; \
	echo 'esac'; \
	echo ; \
	echo 'if [[ $$debugLinking != "" ]] ; then'; \
	echo '	commandLine="libtool --mode=link $$libtoolTag $$linkerCommand $$linkFlags -o $$productsdir/$$outputFile $$@ $$libFlags"'; \
	echo '	echo $$commandLine'; \
	echo 'else'; \
	echo '	commandLine="libtool --quiet --mode=link $$libtoolTag $$linkerCommand $$linkFlags -o $$productsdir/$$outputFile $$@ $$libFlags"'; \
	echo 'fi'; \
	echo '$$commandLine' ) > .link
	@ chmod 755 .link

.makeDependencies: Makefile
	@ ( echo '#! /bin/bash'; \
	echo '# Generic Makefile'; \
	echo '# Copyright (C) 2012,2013 Nathanael Hübbe'; \
	echo '#'; \
	echo '# This program is free software: you can redistribute it and/or modify'; \
	echo '# it under the terms of the GNU General Public License as published by'; \
	echo '# the Free Software Foundation, either version 3 of the License, or'; \
	echo '# (at your option) any later version.'; \
	echo '#'; \
	echo '# This program is distributed in the hope that it will be useful,'; \
	echo '# but WITHOUT ANY WARRANTY; without even the implied warranty of'; \
	echo '# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the'; \
	echo '# GNU General Public License for more details.'; \
	echo '#'; \
	echo '# You should have received a copy of the GNU General Public License'; \
	echo '# along with this program.  If not, see <http://www.gnu.org/licenses/>.'; \
	echo '# '; \
	echo '# You can contact the authors of this programm via sourceforge:'; \
	echo '# http://sourceforge.net/projects/brilliantasreal/'; \
	echo ''; \
	echo '############'; \
	echo '# Build the correct dependencies for a source file located in a subdirectory.'; \
	echo '############'; \
	echo '# $$includeFlags: The options to be passed to the compiler.'; \
	echo '# $$rootdir: The path the argument paths are relative to.'; \
	echo '# $$builddir: The directory to place the object file.'; \
	echo '#'; \
	echo '# $$1: The subdirectory of the source, relative to the root direktory of the Makefile.'; \
	echo '# $$2: The name of the source file.'; \
	echo '# $$3: The name of the corresponding object file.'; \
	echo '# $$4: The name of the dependency file to be generated.'; \
	echo '#'; \
	echo '# The effective path of the object and dependency files are of the form'; \
	echo '# $$rootdir/builddir/subdir/filename'; \
	echo '# The source file is found at'; \
	echo '# $$rootdir/subdir/filename'; \
	echo '############'; \
	echo ''; \
	echo 'subdir=$${1%/}'; \
	echo 'sourcefile=$$2'; \
	echo 'objfile=$$3'; \
	echo 'depfile=$$4'; \
	echo ''; \
	echo 'tempFiles='; \
	echo ''; \
	echo '# Create the raw dependency file and parse the preprocessor output for modules and uses if the source is a fortran file.'; \
	echo 'cd $$rootdir'; \
	echo 'mkdir -p $$builddir/$$subdir'; \
	echo 'cd $$subdir'; \
	echo 'modules='; \
	echo 'uses='; \
	echo 'fortranCheck=$${sourcefile%.f77}'; \
	echo 'fortranCheck=$${fortranCheck%.f90}'; \
	echo 'fortranCheck=$${fortranCheck%.f95}'; \
	echo 'if [[ $$fortranCheck == $$sourcefile ]] ; then'; \
	echo '	$$cppCompiler -M -MF $$rootdir/$$builddir/$$subdir/$$depfile.temp $$includeFlags $$sourcefile'; \
	echo '	tempFiles="$$tempFiles $$rootdir/$$builddir/$$subdir/$$depfile.temp"'; \
	echo 'else'; \
	echo '	$$fortranCompiler $$compileFlags -cpp -E -MD -MF $$rootdir/$$builddir/$$subdir/$$depfile.temp -o $$rootdir/$$builddir/$$subdir/$$sourcefile.temp $$includeFlags $$sourcefile'; \
	echo '	tempFiles="$$tempFiles $$rootdir/$$builddir/$$subdir/$$depfile.temp $$rootdir/$$builddir/$$subdir/$$sourcefile.temp"'; \
	echo "	moduleStream=\"\$$(grep -i -e \"^[^!&\\\"']*module\\s*\\w\" -e \"^[^!&\\\"']*use\" -e \"^[^!&\\\"']*include\" < \$$rootdir/\$$builddir/\$$subdir/\$$sourcefile.temp)\""; \
	echo '	moduleStream="$${moduleStream,,}"'; \
	echo '	interpretModule=no'; \
	echo '	interpretUse=no'; \
	echo '	interpretEnd=no'; \
	echo '	for curWord in $$moduleStream ; do'; \
	echo '		case $$curWord in'; \
	echo '			(module)'; \
	echo '				if [[ $$interpreteUse == "yes" || $$interpreteModule == "yes" ]] ; then'; \
	echo '					echo Error: use or module keyword followed by module keyword'; \
	echo '					exit 1'; \
	echo '				fi'; \
	echo '				if [[ $$interpreteEnd == "yes" ]] ; then'; \
	echo '					interpreteEnd=no'; \
	echo '				else'; \
	echo '					interpreteModule=yes'; \
	echo '				fi'; \
	echo '			;; (use)'; \
	echo '				if [[ $$interpreteUse == "yes" || $$interpreteModule == "yes" ]] ; then'; \
	echo '					echo Error: use or module keyword followed by use keyword'; \
	echo '					exit 1'; \
	echo '				fi'; \
	echo '				interpreteUse=yes'; \
	echo '			;; (end)'; \
	echo '				if [[ $$interpreteUse == "yes" || $$interpreteModule == "yes" || $$interpreteEnd == "yes" ]] ; then'; \
	echo '					echo Error: use, module or end keyword followed by end keyword'; \
	echo '					exit 1'; \
	echo '				fi'; \
	echo '				interpreteEnd=yes'; \
	echo '			;; (include)'; \
	echo '				echo "Error: Cannot handle fortran include statements. Use #include instead."'; \
	echo '				exit 1'; \
	echo '			;; (*)'; \
	echo '				if [[ $$interpreteUse == "yes" ]] ; then'; \
	echo '					uses="$$uses $${curWord/,/}"'; \
	echo '					interpreteUse=no'; \
	echo '				elif [[ $$interpreteModule == "yes" ]] ; then'; \
	echo '					modules="$$modules $${curWord/,/}"'; \
	echo '					interpreteModule=no'; \
	echo '				fi'; \
	echo '				interpreteEnd=no'; \
	echo '			;;'; \
	echo '		esac'; \
	echo '	done'; \
	echo 'fi'; \
	echo 'cd $$rootdir'; \
	echo ''; \
	echo '# Add the path information to local includes.'; \
	echo '# $$1: The path to be converted.'; \
	echo '# $$convertedPath: Output. The possibly modified path.'; \
	echo 'function convertPath() {'; \
	echo '	if [[ $$1 == "" ]] ; then'; \
	echo '		convertedPath='; \
	echo '	elif [[ $$1 == /* ]] ; then'; \
	echo '		convertedPath=$$1'; \
	echo '	else'; \
	echo '		convertedPath=$$subdir/$$1'; \
	echo '		convertedPath=$${convertedPath#./}	#subdir may be . - remove it.'; \
	echo '	fi'; \
	echo '}'; \
	echo ''; \
	echo 'outputDeps='; \
	echo 'truncate -s 0 $$builddir/$$subdir/$$depfile'; \
	echo ''; \
	echo '# If there are fortran modules produced by this file, add rules to touch the .mod files and to clean them'; \
	echo 'for curModule in $$modules ; do'; \
	echo '	echo "clean$$curModule.mod:;rm -f $$builddir/.modules/$$curModule.mod" >> $$builddir/$$subdir/$$depfile'; \
	echo '	echo "clean: clean$$curModule.mod" >> $$builddir/$$subdir/$$depfile'; \
	echo '	outputDeps="$$outputDeps $$builddir/.modules/$$curModule.mod"'; \
	echo 'done'; \
	echo 'if [[ $$modules != "" ]] ; then'; \
	echo '	touchArguments="$$outputDeps"'; \
	echo '	# XXX: This is extremely awkward, but I am at a loss at how to do this better (= no double compilation when not building the default and still correct).'; \
	echo '	# The problem is, that the modules are updated by compiling at _any_ optimization level, but there is simply no way to tell make "Consider this file up to date if you have rebuilt _any_ of its prerequisites". So the only work around seems to be to make the module files depend on the object file at the deployment optimization level and put up with the fact, that this optimization level will allways be kept up to date.'; \
	echo '	outputDeps="$$outputDeps: $$builddir/$$subdir/$${objfile%.o}\$$(deploymentOptimizationLevel).o"'; \
	echo '	outputDeps=$${outputDeps//\/.\//\/}	#subdir may be . - remove it, if that is the case.'; \
	echo '	echo $$outputDeps >> $$builddir/$$subdir/$$depfile'; \
	echo '	echo -e "\t@touch $$touchArguments" >> $$builddir/$$subdir/$$depfile'; \
	echo 'fi'; \
#	echo '# Add dummy rules for the used modules, so that make will not get confused with modules provided by libraries.'; \
#	echo 'for curModule in $$uses ; do'; \
#	echo '	echo "$$builddir/.modules/$$curModule.mod:" >> $$builddir/$$subdir/$$depfile'; \
#	echo 'done'; \
	echo ''; \
	echo '# Rules for object file and dependency file are separated, so that the object file can depend on more things than the dependency file (like the .mod files in the fortran case).'; \
	echo 'outputDeps="\$$(foreach oLevel,\$$(optimizationLevels),$$builddir/$$subdir/$${objfile%.o}\$$(oLevel).o $$builddir/$$subdir/$${objfile%.o}\$$(oLevel).lo ): $$builddir/$$subdir/$$depfile"'; \
	echo 'outputDeps=$${outputDeps//\/.\//\/}	#subdir may be . - remove it, if that is the case.'; \
	echo 'for curModule in $$uses ; do'; \
	echo '	# Module files may be provided by libraries. These must not be listed as prerequisites in builddir/.modules/, otherwise we get either build failures or (with dummy rules) compile dependent files every time.'; \
	echo '	( echo program test ; echo use $$curModule ; echo end program ) > $$builddir/.makeDepedenciesTest$$sourcefile'; \
	echo '	tempFiles="$$tempFiles $$builddir/.makeDepedenciesTest$$sourcefile $$builddir/.makeDepedenciesTest$$objfile"'; \
	echo '	if $$fortranCompiler $$compileFlags $$includeFlags -o $$builddir/.makeDepedenciesTest$$objfile $$builddir/.makeDepedenciesTest$$sourcefile 2> /dev/null ; then'; \
	echo '		#FIXME: To be correct, we would need to determine where the fortran compiler found this module file.'; \
	echo '		echo "Module $$curModule is provided by some library. Skipping this dependency."'; \
	echo '	else'; \
	echo '		outputDeps="$$outputDeps $$builddir/.modules/$$curModule.mod"'; \
	echo '	fi'; \
	echo 'done'; \
	echo 'echo $$outputDeps >> $$builddir/$$subdir/$$depfile'; \
	echo ''; \
	echo '# Finally the rule with all the dependencies caused by preprocessing'; \
	echo 'outputDeps="$$builddir/$$subdir/$$depfile:"	#The targets of the dependencies.'; \
	echo 'outputDeps=$${outputDeps//\/.\//\/}	#subdir may be . - remove it, if that is the case.'; \
	echo 'dependencies="$$(cat $$builddir/$$subdir/$$depfile.temp)"	#Get the dependencies.'; \
	echo 'dependencies="$${dependencies#*:}"	#Cleave off the wrong target.'; \
	echo 'for path in $$dependencies ; do	#Convert the dependency information.'; \
	echo '	path=$${path%\\\\}'; \
	echo '	convertPath $$path'; \
	echo '	outputDeps="$$outputDeps $$convertedPath"'; \
	echo 'done'; \
	echo 'echo $$outputDeps >> $$builddir/$$subdir/$$depfile'; \
	echo ''; \
	echo '#cleanup'; \
	echo 'rm -f $$tempFiles' ) > .makeDependencies
	@ chmod 755 .makeDependencies

.PHONY: grepSource
grepSource: Makefile
	@echo Making grepSource
	@(echo '#! /bin/bash'; \
		echo '############'; \
		echo '# This is an autogenerated file. If you do not like its contents, look at the Makefile.'; \
		echo '############'; \
		echo '# Usage:'; \
		echo '#'; \
		echo '# This script is a grep-wrapper to search in all source code files recognized by the generic Makefile. Pass it a grep pattern and any other grep options you like, they are simply forwarded, but there is no need to specify any files to search.'; \
		echo '#'; \
		echo '# If the first argument is --use-red, a different color scheme is used and this argument is stripped before the other arguments are passed to grep.'; \
		echo ; \
		$(if $(useColor),\
			echo 'if [[ $$1 == --use-red ]] ; then'; \
			echo '	shift'; \
			echo '	export GREP_COLORS="ms=01;31:mc=01;31:sl=:cx=:fn=36:ln=32:bn=32:se=36"'; \
			echo 'else'; \
			echo '	export GREP_COLORS="ms=01;33:mc=01;33:sl=:cx=:fn=36:ln=32:bn=32:se=36"'; \
			echo 'fi'; \
		)\
		echo 'grep --color -n "$$@" $(all_sources) $(headers)';\
	) > grepSource
	@chmod 755 grepSource

#TODO: Encapsulate the coloring magic in a makefile function. Somehow. Because this _is_ ugly.
printTodo: grepSource
	@bugCount=$$(./grepSource FIXME | wc -l); \
	todoCount=$$(./grepSource TODO | wc -l); \
	echo 'if (('$$bugCount')); then $(if $(useColor),echo -e "\e[01;31m'"Number of FIXMEs left: $$bugCount"'\e[00m",echo Number of FIXMEs left: '$$bugCount'); fi' | bash; \
	echo 'if (('$$todoCount')); then $(if $(useColor),echo -e "\e[01;33m'"Number of TODOs left: $$todoCount"'\e[00m",echo Number of TODOs left: '$$todoCount'); fi' | bash; \
	echo 'if (('$$bugCount+$$todoCount')); then echo Run "make todo" to list all FIXME and TODO comments in the code.; fi' | bash

fixme: grepSource
	@echo $(if $(useColor),'echo -e "\e[01;31m'"FIXMEs:"'\e[00m"' | bash,FIXMEs:)
	@./grepSource --use-red FIXME || true

todo: fixme grepSource
	@echo
	@echo $(if $(useColor),'echo -e "\e[01;33m'"TODOs:"'\e[00m"' | bash,TODOs:)
	@./grepSource TODO || true
	@bugCount=$$(./grepSource FIXME | wc -l); \
	todoCount=$$(./grepSource TODO | wc -l); \
	echo 'if (('$$bugCount')); then $(if $(useColor),echo -e "\e[01;31m'"Number of FIXMEs left: $$bugCount"'\e[00m",echo Number of FIXMEs left: '$$bugCount'); fi' | bash; \
	echo 'if (('$$todoCount')); then $(if $(useColor),echo -e "\e[01;33m'"Number of TODOs left: $$todoCount"'\e[00m",echo Number of TODOs left: '$$todoCount'); fi' | bash;

clean:
	rm -f grepSource .compile .link .makeDependencies $(all_products) \
		$(foreach oLevel,$(optimizationLevels),\
			$(foreach object,$(all_objects),\
				$(object:.o=$(oLevel).o) \
				$(object:.o=$(oLevel).lo) \
				$(dir $(object)).libs/$(notdir $(object:.o=$(oLevel).o)) \
			) \
		) \
		$(all_dependencyFiles)

# Report the number of TODOs and FIXMEs at the end of each build.
$(if $(checkTodo),defaultTarget: printTodo)

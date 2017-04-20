export builddir := build
export productsdir := .

export includeFlags := 
export compileFlags := -Wtabs -ggdb
export deploymentOptimizationLevel := -O0

export libFlags :=
export linkFlags := -ggdb

# Set this variable to see the actual compilation command lines.
# export debugCompilation := 1
# Set this variable to see the actual linking command lines.
# export debugLinking := 1

# Comment out to skip grepping for TODO and BUG after building.
export checkTodo := true
# Comment out if you do not like colors for some reason.
export useColor := true

cleansorter.mod:;rm -f build/.modules/sorter.mod
clean: cleansorter.mod
build/.modules/sorter.mod: build/sorter$(deploymentOptimizationLevel).o
	@touch  build/.modules/sorter.mod
$(foreach oLevel,$(optimizationLevels),build/sorter$(oLevel).o build/sorter$(oLevel).lo ): build/sorter.d
build/sorter.d: sorter.f95

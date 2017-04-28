cleanalgorithmtester.mod:;rm -f build/.modules/algorithmtester.mod
clean: cleanalgorithmtester.mod
build/.modules/algorithmtester.mod: build/algorithmTester$(deploymentOptimizationLevel).o
	@touch  build/.modules/algorithmtester.mod
$(foreach oLevel,$(optimizationLevels),build/algorithmTester$(oLevel).o build/algorithmTester$(oLevel).lo ): build/algorithmTester.d build/.modules/sorter.mod
build/algorithmTester.d: algorithmTester.f95

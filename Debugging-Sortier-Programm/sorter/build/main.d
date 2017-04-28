$(foreach oLevel,$(optimizationLevels),build/main$(oLevel).o build/main$(oLevel).lo ): build/main.d build/.modules/algorithmtester.mod
build/main.d: main.f95

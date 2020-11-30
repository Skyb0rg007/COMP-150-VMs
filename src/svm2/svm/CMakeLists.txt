
include(CheckIncludeFile)
include(CheckSymbolExists)

# Check for non-ANSI headers + functions
check_include_file("stdbool.h"  SVM_HAS_STDBOOL_H)
check_include_file("inttypes.h" SVM_HAS_INTTYPES_H)
check_symbol_exists("strtok_r" "string.h" SVM_HAS_STRTOK_R)
check_symbol_exists("getline"  "stdio.h"  SVM_HAS_GETLINE)

# Options (TODO: turn into option()s and move to root CMakeLists.txt)
set(SVM_STRING_MAX_SHORTLEN 10)
set(SVM_STRING_MIN_STRTAB 128)
set(SVM_DEBUG_LEVEL 2)
set(SVM_DEBUG_LINEINFO ON)
set(SVM_USE_COMPUTED_GOTO ON)

configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/include/svm/config.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/include/svm/config.h
    @ONLY)

add_library(svmlib
    include/svm/alloc.h
    include/svm/assert.h
    ${CMAKE_CURRENT_BINARY_DIR}/include/svm/config.h
    include/svm/heap.h
    include/svm/instruction.h
    include/svm/opcodes.h
    include/svm/parse.h
    include/svm/run.h
    include/svm/string.h
    include/svm/value.h
    include/svm/vector.h
    include/svm/vm.h

    src/alloc.c
    src/heap.c
    src/instruction.c
    src/parse.c
    src/run.c
    src/string.c
    src/value.c
    src/vm.c
    )
target_include_directories(svmlib
    PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    )

add_executable(svm
    src/svm.c)
target_link_libraries(svm PRIVATE svmlib)


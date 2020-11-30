## Boilerplate
cmake_minimum_required(VERSION 3.17 FATAL_ERROR)
project(SVM
    VERSION 0.0.1
    DESCRIPTION "A simple virtual machine"
    HOMEPAGE_URL https://github.com/Skyb0rg007/COMP-150-VMs
    LANGUAGES C)

## Settings
set(CMAKE_C_FLAGS_DEBUG "-g -fno-omit-frame-pointer -fsanitize=address")
set(CMAKE_LINKER_FLAGS_DEBUG "-g -fno-omit-frame-pointer -fsanitize=address")
# set(CMAKE_C_FLAGS_DEBUG "-g")
# set(CMAKE_LINKER_FLAGS_DEBUG "-g")

## Dependencies
find_package(Doxygen MODULE OPTIONAL_COMPONENTS dot)
find_package(cmocka)

## Building
add_subdirectory(svm)

## Testing
include(CTest)
if(BUILD_TESTING AND cmocka_FOUND)
    # Setup cmocka imported target
    add_library(cmocka::cmocka IMPORTED SHARED)
    set_target_properties(cmocka::cmocka PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES ${CMOCKA_INCLUDE_DIR}
        IMPORTED_LOCATION ${CMOCKA_LIBRARY})
    # Load tests
    add_subdirectory(tests)
endif()

## Documentation
if(DOXYGEN_FOUND)
    set(DOXYGEN_GENERATE_HTML TRUE)
    set(DOXYGEN_EXTRACT_STATIC TRUE)
    set(DOXYGEN_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/docs)
    set(DOXYGEN_STRIP_FROM_PATH "svm/include,${CMAKE_CURRENT_BINARY_DIR}")
    doxygen_add_docs(docs
        svm/include
        ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating Doxygen documentation")
endif()



function(svm_test testname)
    set(opts "")
    set(oneValue "")
    set(multiValue SOURCES LIBS)
    cmake_parse_arguments(svm_test "${opts}" "${oneValue}" "${multiValue}" ${ARGN})
    if (NOT svm_test_SOURCES)
        set(svm_test_SOURCES "${testname}")
    endif()
    if (NOT svm_test_LIBS)
        set(svm_test_LIBS svmlib cmocka::cmocka)
    endif()
    add_executable("${testname}"
        ${svm_test_SOURCES})
    target_link_libraries("${testname}"
        ${svm_test_LIBS})
    add_test(
        NAME "${testname}"
        COMMAND "${testname}" $<CONFIGURATION>)
endfunction()

svm_test(string-test)
svm_test(vector-test)
svm_test(heap-test)
svm_test(playground)


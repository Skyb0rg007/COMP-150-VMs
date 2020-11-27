/** @file svm/parse.h
 * @author Skye Soss
 * @copyright MIT
 * @brief Routines for parsing .vo files
 */
#ifndef SVM_PARSE_H
#define SVM_PARSE_H

#include <svm/config.h>
#include <svm/vm.h>
#include <svm/instruction.h>

struct svm_function_t *svm_parse_module(struct svm_vm_t *vm, FILE *input);

#endif /* ifndef SVM_PARSE_H */

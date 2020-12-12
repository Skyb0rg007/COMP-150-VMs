/** @file svm/assert.h
 * @author Skye Soss
 * @copyright MIT
 * @brief Assertions
 * Assertions and panic functions used for sanity checking and debugging
 */
#ifndef SVM_ASSERT_H
#define SVM_ASSERT_H

#include <svm/config.h>

#define SVM_FILE ((strrchr(__FILE__, '/') ? strrchr(__FILE__, '/') : __FILE__) + 1)

#define svm_panic(...) svm_panic_impl(SVM_FILE, __LINE__, __VA_ARGS__)

#define svm_assert_release(x)    ((x) ? (void)0 : svm_panic("Assertion failed: \"%s\"", #x))
#if SVM_DEBUG_LEVEL == 0
#  define svm_assert(_)          ((void)0)
#  define svm_assert_paranoid(_) ((void)0)
#  define svm_log(...)           ((void)0)
#elif SVM_DEBUG_LEVEL == 1
#  define svm_assert(x)          ((x) ? (void)0 : svm_panic("Assertion failed: \"%s\"", #x))
#  define svm_assert_paranoid(_) ((void)0)
#  define svm_log(...)           ((void)0)
#elif SVM_DEBUG_LEVEL == 2
#  define svm_assert(x)          ((x) ? (void)0 : svm_panic("Assertion failed: \"%s\"", #x))
#  define svm_assert_paranoid(x) ((x) ? (void)0 : svm_panic("Assertion failed: \"%s\"", #x))
#  define svm_log(...)           svm_log_impl(SVM_FILE, __LINE__, __VA_ARGS__)
#else
#  error "Invalid SVM_DEBUG_LEVEL: should be in the range 0-2"
#endif

SVM_ATTR_NORETURN SVM_ATTR_NONNULL(1, 3) SVM_ATTR_PRINTF(3, 4)
static inline void svm_panic_impl(const char *file, int line, const char *fmt, ...)
{
    va_list args;
    SVM_UNUSED(file);
    SVM_UNUSED(line);
    va_start(args, fmt);
#if SVM_DEBUG_LINEINFO
    fprintf(stderr, "%s:%d: ", file, line);
#endif
    vfprintf(stderr, fmt, args);
    fputs("\n", stderr);
    fflush(stderr);
    va_end(args);
    abort();
}

SVM_ATTR_NONNULL(1, 3) SVM_ATTR_PRINTF(3, 4)
static inline void svm_log_impl(const char *file, int line, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    fprintf(stderr, "%s:%d: ", file, line);
    vfprintf(stderr, fmt, args);
    fputs("\n", stderr);
    fflush(stderr);
    va_end(args);
}

#endif /* ifndef SVM_ASSERT_H */

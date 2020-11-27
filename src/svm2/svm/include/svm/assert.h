/** @file svm/assert.h
 * @author Skye Soss
 * @copyright MIT
 * @brief Assertions
 * Assertions and panic functions used for sanity checking and debugging
 */
#ifndef SVM_ASSERT_H
#define SVM_ASSERT_H

#include <svm/config.h>

/** @brief The current file's basename
 * @note This does not expand to a string literal
 */
#define SVM_FILE ((strrchr(__FILE__, '/') ? strrchr(__FILE__, '/') : __FILE__) + 1)

/** @brief Prints the formatted output to stderr, aborting the program.
 * @param ... The format + printf-style arguments
 * Prints file and line info, followed by formatted output.
 * Outputs a newline before aborting.
 */
#define svm_panic(...) svm_panic_impl(SVM_FILE, __LINE__, __VA_ARGS__)

SVM_ATTR_NORETURN SVM_ATTR_NONNULL(1, 3) SVM_ATTR_PRINTF(3, 4)
static inline void svm_panic_impl(const char *file, int line, const char *fmt, ...)
{
    va_list args;
    (void)file;
    (void)line;
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

/** @def svm_assert_release
 * @param x An expression to check
 * @brief Aborts the program if @p x evaluates to 0
 */
/** @def svm_assert
 * @param x An expression to check
 * @brief Aborts the program if @p x evaluates to 0
 * @note This macro expands to @code (void)0 @endcode if SVM_DEBUG_LEVEL < 1
 */
/** @def svm_assert_paranoid
 * @param x An expression to check
 * @brief Aborts the program if @p x evaluates to 0
 * @note This macro expands to @code (void)0 @endcode if SVM_DEBUG_LEVEL < 2
 */
/** @def svm_log
 * @brief Debugging output
 * @note This macro expands to @code (void)0 @endcode if SVM_DEBUG_LEVEL < 2
 */
#define svm_assert_release(x)    ((x) ? (void)0 : svm_panic("assertion failed: \"%s\"", #x))
#if SVM_DEBUG_LEVEL == 0
#  define svm_assert(_)          ((void)0)
#  define svm_assert_paranoid(_) ((void)0)
#  define svm_log(...)           ((void)0)
#elif SVM_DEBUG_LEVEL == 1
#  define svm_assert(x)          ((x) ? (void)0 : svm_panic("assertion failed: \"%s\"", #x))
#  define svm_assert_paranoid(_) ((void)0)
#  define svm_log(...)           ((void)0)
#elif SVM_DEBUG_LEVEL == 2
#  define svm_assert(x)          ((x) ? (void)0 : svm_panic("assertion failed: \"%s\"", #x))
#  define svm_assert_paranoid(x) ((x) ? (void)0 : svm_panic("assertion failed: \"%s\"", #x))
#  define svm_log(...)           svm_log_impl(SVM_FILE, __LINE__, __VA_ARGS__)
#else
#  error "Invalid SVM_DEBUG_LEVEL: should be in the range 0-2"
#endif

#endif /* ifndef SVM_ASSERT_H */

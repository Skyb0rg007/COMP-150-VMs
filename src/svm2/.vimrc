
let ale_c_gcc_options = '-Isvm/include -Isvm/src -I_build/svm/include -D_GNU_SOURCE -std=c99 -Wall -Wextra -pedantic'
let &path = 'svm/include,svm/src,_build/svm/include,/usr/include,/usr/lib/gcc/x86_64-redhat-linux/10/include/,/usr/local/include/'

set makeprg=cmake\ --build\ _build

augroup svm
    autocmd BufRead,BufNewFile *.c,*.h,*.h.in set ft=c.doxygen
    autocmd BufRead,BufNewFile *.h.in ALEDisableBuffer
augroup end

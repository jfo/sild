#include <stdio.h>
#include <stdlib.h>

#include "cell.h"
#include "read.h"
#include "eval.h"
#include "print.h"

int main() {

    char *a_string = "(cons ((cond car) (quote (1))) (cdr (quote (2 3 4 5))))";

    C *a_list          = read(&a_string);
    C *an_evalled_list = eval(a_list);
                         print(an_evalled_list);
                         /* debug_list(an_evalled_list); */
    return 0;
}
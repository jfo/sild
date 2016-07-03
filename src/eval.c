#include <stdio.h>
#include <stdlib.h>

#include "cell.h"
#include "env.h"
#include "eval.h"
#include "util.h"

/* ---------- */
/* eval/apply */
/* ---------- */

static int count_list(C *c){
    int i= 0;
    while(c->type != NIL) {
        i++;
        c = c->next;
    }
    return i;
};

static C *apply_proc(C* proc, Env *env) {

    C *cur = proc->val.proc.args->val.list;
    C *curarg = proc->next;

    int arity = count_list(cur);
    int numpassed = count_list(curarg);

    if (arity != numpassed) {
        printf("arity error on proc application\n");
        exit(1);
    }

    struct Env *frame = new_env();
    C *nextarg;
    for(int i = 0; i < arity; i++) {
        nextarg = curarg->next;
        curarg->next = &nil;
        set(frame, cur->val.label, eval(curarg, env));
        curarg = nextarg;
        cur = cur->next;
    }

    frame->next = proc->val.proc.env;

    // eval will free the body, here:
    C *out = eval(proc->val.proc.body, frame);
    // but the args and the proc cell itself still need to be freed manually
    // after application:

    free_cell(proc->val.proc.args);
    free(proc);


    // TODO: freeing a frame means higher order functions retain dangling
    // references to their enclosing calling env. This is broke! Currently,
    // defining a higher order function will result in a segfault. Commenting
    // this line out will retain that memory but will also cause a memory leak.
    // free_env(frame);

    return out;
}

static C *apply(C* c, Env *env) {
    switch (c->type) {
        case BUILTIN:
            {
                C *out = c->val.func.addr(c->next, env);
                free_one_cell(c);
                return out;
            }
        case LIST:
            return apply(eval(c, env), env);
        case TRUTH:
        case LABEL:
            fprintf(stderr, "\nError: attempted to apply non-procedure %s\n", c->val.label);
            exit(1);
        case PROC:
            return apply_proc(c, env);
        case NIL:
            fprintf(stderr, "\nError: attempted to evaluate an empty list: ()\n");
            exit(1);
    }
    unreachable();
}

C *eval(C* c, Env *env) {
    switch (c->type) {
        case LIST:
        {
            C *out = apply(eval(c->val.list, env), env);
            out->next = c->next;
            free(c);
            return out;
        }
        case LABEL:
        {
            C *out = get(env, c);
            if (out) {
                out->next = c->next;
                free_one_cell(c);
                return out;
            } else {
                fprintf(stderr, "\nError: unbound label: %s\n", c->val.label);
                exit(1);
            }
        }
        case TRUTH:
        case PROC:
        case BUILTIN:
        case NIL:
            return c;
    }
    unreachable();
}


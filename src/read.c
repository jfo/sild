#include <stdio.h>
#include <stdlib.h>

#include "util.h"
#include "cell.h"
#include "builtins.h"
#include "read.h"

/* ------ */
/* reader */
/* ------ */

static int is_not_delimiter(char c) {
    return (c != ' ' && c != '\n' &&c != '\0' && c != '(' && c != ')' && c != EOF && c != ';');
};

static char *read_substring(FILE *s) {
    int l = 0;
    while (is_not_delimiter(getc(s))) { l++; }
    char *out = malloc(l + 1);
    if (!out) { exit(1); }

    fseek(s, -l - 1, SEEK_CUR);

    for (int i = 0; i < l; i++) {
        out[i] = getc(s);
    }
    out[l] = '\0';
    return out;
};


static void verify(char c, int depth) {
    if (
            depth < 0
            ||
            (c == ')' && depth == 0)
            ||
            (c == '\0' && depth != 0)
            ||
            (c == EOF && depth != 0)
       )
    {
        fprintf(stderr, "ERROR: mismatched parens\n");
        exit(1);
    }
}

static C * read_inner(FILE *s, int depth);

static C *quote_next(FILE *s, int depth) {
    C *quotecell = makecell(BUILTIN, (V){ .func = { scpy("quote"), quote } }, read(s));
    return makecell(
            LIST, (V)
            { .list = quotecell },
            (depth > 0 ? read_inner(s,depth) : &nil));
};

static C* categorize(FILE *s, int depth) {
    char *token = read_substring(s);
    C *out;

    if (scmp(token, "quote")) {
        out = makecell(BUILTIN, (V){ .func = {token, quote} }, &nil);
    } else if (scmp(token, "car")) {
        out = makecell(BUILTIN, (V){ .func = {token, car} }, &nil);
    } else if (scmp(token, "cdr")) {
        out = makecell(BUILTIN, (V){ .func = {token, cdr} }, &nil);
    } else if (scmp(token, "cons")) {
        out = makecell(BUILTIN, (V){ .func = {token, cons} }, &nil);
    } else if (scmp(token, "atom")) {
        out = makecell(BUILTIN, (V){ .func = {token, atom} }, &nil);
    } else if (scmp(token, "eq")) {
        out = makecell(BUILTIN, (V){ .func = {token, eq} }, &nil);
    } else if (scmp(token, "cond")) {
        out = makecell(BUILTIN, (V){ .func = {token, cond} }, &nil);
    } else if (scmp(token, "lambda")) {
        out = makecell(BUILTIN, (V){ .func = {token, lambda} }, &nil);
    } else if (scmp(token, "true")) {
        out = truth();
    } else if (scmp(token, "false")) {
        out = empty_list();
    } else if (scmp(token, "define")) {
        if (depth > 1) {
            fprintf(stderr, "Error: define found in inner form.");
            exit(1);
        }
        out = makecell(BUILTIN, (V){ .func = {token, define} }, &nil);
    } else if (scmp(token, "display")) {
        if (depth > 1) {
            fprintf(stderr, "Error: display found in inner form.");
            exit(1);
        }
        out = makecell(BUILTIN, (V){ .func = {token, display} }, &nil);
    } else {
        out = makecell(LABEL, (V){ token }, &nil);
    }

    if (depth > 0) {
        out->next = read_inner(s, depth);
    }

    return out;
}

static C * read_inner(FILE *s, int depth) {
    char current_char = getc(s);

    verify(current_char, depth);

    switch(current_char) {
        case ')': case '\0': case EOF:
            return &nil;
        case ';':
            while(getc(s) != '\n');
            // falls through
        case ' ': case '\n':
            return read_inner(s, depth);
        case '\'':
            return quote_next(s, depth);
        case '(':
            {
                V val = (V){.list = read_inner(s, depth + 1)};
                C *next= (depth > 0 ? read_inner(s, depth) : &nil);
                return makecell(LIST, val, next);
            }
        default:
            fseek(s, -1, SEEK_CUR);
            return categorize(s, depth);
    }
}

C * read(FILE *s) {
    return read_inner(s, 0);
}

#ifndef UTIL_GUARD
#define UTIL_GUARD

int scmp(char*, char*);
char *scpy(char*);

__attribute__((noreturn))
__attribute__((cold))
void unreachable();

#endif

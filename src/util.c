#include <stdlib.h>
#include <stdio.h>

/* ----------*/
/* utilities */
/* ----------*/

int scmp(char *str1, char *str2) {
    int i;
    for (i = 0; str1[i] != '\0'; i++) {
        if (str1[i] != str2[i] || str2[i] == '\0') {
            return 0;
        }
    }
    if (str2[i] == '\0') {
        return 1;
    } else {
        return 0;
    }
}

char *scpy(char *s) {
    int l = 0;
    while (s[l] != '\0') { l++; }
    char *out = malloc(l + 1);
    if (!out) { exit(1); }

    for (int i = 0; i < l; i++) {
        out[i] = s[i];
    }
    out[l] = '\0';
    return out;
};

void unreachable() {
    fprintf(stderr, "unreachable\n");
    abort();
}


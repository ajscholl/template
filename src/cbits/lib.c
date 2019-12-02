#include "lib.h"

#include <stdlib.h>
#include <stdbool.h>
#include <HsFFI.h>

// exported functions

char *hsRunTemplate(
    const char *tmplFileName,
    const char *varFileName,
    const char *tmplFile,
    const char *luaFile,
    const char *varFile,
    const char *jsonFile,
    int *status
);

void hsFreeTemplate(char *data);

// library initialization and finalization

static bool initialized = false;

static void init(void) {
    if (initialized) {
        return;
    }

    int argc = 1;
    char *argv[] = { "libtemplates-to-go-runtime.so", NULL };
    char **pargv = argv;
    hs_init(&argc, &pargv);

    initialized = true;
}

static void __attribute__((destructor)) finit(void) {
    if (initialized) {
        hs_exit();
        initialized = false;
    }
}

char *runTemplate(
    const char *tmplFileName,
    const char *varFileName,
    const char *tmplFile,
    const char *luaFile,
    const char *varFile,
    const char *jsonFile,
    int *status
) {
    init();
    return hsRunTemplate(tmplFileName, varFileName, tmplFile, luaFile, varFile, jsonFile, status);
}

void freeTemplate(char *data) {
    if (data != NULL) {
        init();
        hsFreeTemplate(data);
    }
}

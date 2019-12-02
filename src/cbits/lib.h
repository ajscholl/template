char *runTemplate(
    const char *tmplFileName,
    const char *varFileName,
    const char *tmplFile,
    const char *luaFile,
    const char *varFile,
    const char *jsonFile,
    int *status
);

void freeTemplate(char *data);

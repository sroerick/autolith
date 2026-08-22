#include <stddef.h>
#include <string.h>

struct autolith_static_symbol {
    const char *name;
    void *address;
};

extern const struct autolith_static_symbol autolith_static_symbols[];
extern const size_t autolith_static_symbol_count;

static _Thread_local const char *autolith_static_dlerror;

void *dlopen(const char *filename, int flags)
{
    (void)filename;
    (void)flags;
    autolith_static_dlerror = NULL;
    return (void *)1;
}

void *dlsym(void *handle, const char *name)
{
    size_t index;

    (void)handle;
    for (index = 0; index < autolith_static_symbol_count; ++index) {
        if (strcmp(name, autolith_static_symbols[index].name) == 0) {
            autolith_static_dlerror = NULL;
            return autolith_static_symbols[index].address;
        }
    }
    autolith_static_dlerror = "symbol is absent from the static Autolith runtime";
    return NULL;
}

int dlclose(void *handle)
{
    (void)handle;
    autolith_static_dlerror = NULL;
    return 0;
}

char *dlerror(void)
{
    const char *message = autolith_static_dlerror;

    autolith_static_dlerror = NULL;
    return (char *)message;
}

#include  <vpi_user.h>
#include "HsFFI.h"
#include "VPITest_stub.h"

void haskell_startup() {
    vpi_printf("Starting up Haskell\n");

    int argc = 0;
    char *argv[] = {};
    char **pargv = argv;
    hs_init(&argc, &pargv);

    vpi_printf("Done starting Haskell\n");
}

void (*vlog_startup_routines[])() = {
    haskell_startup,
    doIt,
    0
};


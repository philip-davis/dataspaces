#ifndef INSITU_DATA_DESCRIPTION_H
#define INSITU_DATA_DESCRIPTION_H

const int gNumInsituVars_ghost = 2;
const int gNumInsituVars_noghost = 9;
static const char* gInsituVarNames_ghost[gNumInsituVars_ghost] = {
"HO2",
"OH",
};

static const char* gInsituVarNames_noghost[gNumInsituVars_noghost] = {
"u",
"v",
"w",
"temp",
"OH",
"HO2",
"du/dx",
"dv/dy",
"dw/dz",
};

#endif

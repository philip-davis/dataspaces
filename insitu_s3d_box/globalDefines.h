#ifndef GLOBAL_DEFINES
#define GLOBAL_DEFINES

#define MIXAVG_TRANSPORT

! #undef ARK */

! #undef BUNSENPOST */
#define SAVEFILEINSEPDIR
#define BLOCKIO
! #undef ADIOS */

! #undef ARCH_SGI */
! #undef ARCH_PC */
! #undef ARCH_X1 */
! #undef ARCH_SP2 */
! #undef ARCH_T3E */
#define ARCH_OPTERON

! #undef POST_RANS_BUNSEN */
! #undef POST_LES_FILTER */
! #undef POST_HDF5 */
! #undef POST_KLM */
! #undef POST_SURFACE */
! #undef POST_TAU */
! #undef BUNSENPOST */
! #undef PQR_CLASSIFICATION */

! #undef BUILD_LIBS3D */

#ifdef POST_TAU
#define BUILD_TAU_POST_SOURCES
#endif

#ifdef MIXAVG_TRANSPORT
#define MIXAVG
#else
#define LEWIS
#endif

! #undef GETRATES_NEEDS_DIFFUSION */
! #undef MECH_SPECIFIC_TRANLIB */

#endif

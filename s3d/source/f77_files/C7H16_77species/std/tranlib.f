C     tranlib_C7H16_77species.f:
C     Tian Feng Lu diffusion bundling routine for 77 species n-Heptane mechanism.
C     (Skeletal mechanism)
      SUBROUTINE MCABS
C
C     Revision 4.6, 1999/06/03 (E. Meeks)
C     1) Fixed problem with KERR not declared LOGICAL in MCORDF
C     Revision 4.5, 1999/04/10 (E. Meeks)
C     1)  Added 4 new subroutines to allow a user to modify
C         data stored in a Transport linking file through
C         library calls.  The new routines are:  MCREWR, which
C         writes a linking file based on data stored in the
C         passed-in work arrays;  MCCDEX, MCCCEX, and
C         MCCVEX which allow extraction of the fitting
C         coefficients for diffusion coefficients, conductivity,
C         and viscosity, respectively, as well as
C         replacement of those values (similar to CKRAEX)
C     Revision 4.4, 1998/12/04 (E. Meeks)
C     1)  Per Action#195:  Added error messages for later parsing
C         into error-handling system, for all fatal errors.
C     2)  Replaced two occurences of 'STOP' statement in MCLMDT,
C         with RETURN; added logical variable in call list KERR
C         that is returned as .TRUE. where the 'STOP's were.
C     3)  Replaced two occurences of 'STOP' statement in MCORDF,
C         with RETURN; added logical variable in call list KERR
C         that is returned as .TRUE. where the 'STOP's were.
C     4)  Added logical KERR to calls to MCORDF and MCLMDT in
C         user-callable subroutines MCMDIF and MCMCDT,respectively.
C     Revision 4.3, 1998/03/03 (E. Meeks)
C     1)  Action #116: Put a change block for LINPACK around
C         the declaration and initialization of DET and JOB
C         variables in routine MCORDF to avoid unused variable warning.
C     Revision: 4.2, Date: 1997/06/10, Author: J. Grcar
C     1)  Fixed bug #035.  Corrected spelling of "corporation".
C     2)  Edited "CVS $Revision" line to fit in 72 columns.
C     3)  Updated output version number and creation date.
C     V. 4.1, 96/05/24
C     1. initial sccs version
C     CHANGES FOR VERSION 4.0 (Initial CHEMKIN-III version,
C                              January 1996 F. Rupley)
C     1.  Add/modify PROLOGUE sections
C     2.  Reverse change comment order in MCABS.
C     3.  binary/ascii linkfile option.
C     4.  separate linkfile, code version numbers.
C
C     CHANGES FOR VERSION 3.9 (2/27/95 F. Rupley)
C     1.  Change character index "(:" to "(1:"
C     CHANGES FOR VERSION 3.8 (1/20/95 F. Rupley)
C     CHANGES FOR VERSION 3.7 (10/3/94) H.K. Moffat
C     1.  Made the small parameter even small for 64 bit systems
C         (change block).
C     CHANGES FOR VERSION 3.6 (8/31/94) H.K. Moffat
C           - Passed check against v3.5 with tranlibtest
C     1.  Performance improvements for MCEVAL and MCEDIF
C     2.  MCEDIF now special cases KK = 1
C     3.  Added a hard-coded Horner's polynomial evaluation routine
C         for NO=4, MCEVAL4
C     4.  Changed MCEVAL so that the MAX function is used to massage
C         the mole fractions instead of the addition function.
C     5.  Changed a statement in MCAVIS to use more exact arithmetic,
C         causing change in 8th digit to occur
C     6.  Performance improvement to MCAVIS - SQRT calls used instead
C         of powers of floating point variables.
C     CHANGES FOR VERSION 3.5 (8/10/94 H.K. Moffat
C     1.  Accepts to 3.2 version number
C     CHANGES FOR VERSION 3.4 (6/12/94) H.K. Moffat
C     1.  Added change blocks for LAPACK linear algebra
C     CHANGES FOR VERSION 3.3 (6/9/94) H.K. Moffat
C     1.  Changed the Gas constant to agree with 1986 CODATA value.
C         (two extra significant digits of accuracy)
C     2.  Changed parameter values for pi and its powers to
C         so that the values are accurate to machine precision.
C     CHANGES FOR VERSION 3.2 (6/8/94 H.K. Moffat
C     1.  Made MCLMDT roughly 30 times faster on a sample problem.
C         Opcount in the function now scales like KK**2, instead
C         of the previous KK**3.
C     2.  No longer need a delta function in the library -
C         CHANGES to MCLMDT and MCORDF
C     CHANGES FOR VERSION 3.1 (4/5/94 M. Coltrin)
C     1.  Fix error in MCLMDT around line 1674 ("ZERO" replaced by
C         "ONE").
C     2.  Indices in formula for BINDIF(I,I) in loop 150 should have
C         been (K,K).
C     3.  A factor of PFAC was omitted from the statement before
C         statement number 1600 in MCLMDT.
C     4.  The factor PIFAC and the statement in which it was used in
C         loop 1600 in MCLMDT were both incorrect.
C     CHANGES FOR VERSION 3.0 (3/15/94 F. Rupley)
C     1.  DOS/PC compatibility effort includes adding file names to
C         OPEN statements, removing unused variables in CALL lists,
C         unusued but possibly initialized variables.
C     CHANGES FOR VERSION 1.7 (10/1/92 F. Rupley per M. Coltrin)
C     1. Created MCABS to hold version and change information
C     2. COMMON /MCCONS/ VERS, PREC, KERR, LENI, LENR eliminates
C        the need for argument LINKMC in the MCSAVE call list
C     CHANGES FOR VERSION 1.6
C     1. Versions 1.8 and 1.9 added (TRANLIB V.1.5
C        and TRANFIT V.1.8 were intermediate versions which may
C        not be legitimate; TRANFIT V.1.9 is actually a
C        correction to V.1.7, and TRANLIB 1.6 is an update of
C        V.1.4)
C     CHANGES FOR VERSION 1.4
C     1. Additional record to binary file indicates
C        version, machine precision, and error status
C     2. Additional record to binary file has required lengths for
C        integer, real work arrays
C     3. New Subroutines MCPNT, MCSAVE read, write binary
C        file information, work arrays
C     CHANGES FOR VERSION 1.3
C     1. SUBROUTINE MCLEN
C     CHANGES FROM VERSION 1.1:
C     1. Eliminated many GOTO's
C     CHANGES FROM VERSION 1.0:
C     1. Changed REAL*8 to real
C
C     end of SUBROUTINE MCABS
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCINIT (LINKMC, LOUT, LENIMC, LENRMC, IMCWRK, RMCWRK,
     1           IFLAG)
C
C  START PROLOGUE
C
C  SUBROUTINE MCINIT (LINKMC, LOUT, LENIMC, LENRMC, IMCWRK, RMCWRK,
C                     IFLAG)
C  This subroutine reads the transport linkfile from the fitting code
C  and creates the internal storage and work arrays, IMCWRK(*) and
C  RMCWRK(*).  MCINIT must be called before any other transport
C  subroutine is called.  It must be called after the CHEMKIN package
C  is initialized.
C
C  INPUT
C  LINKMC    - Integer scalar, transport linkfile input unit number.
C  LOUT      - Integer scalar, formatted output file unit number.
C  LENIMC    - Integer scalar, minimum dimension of the integer
C              storage and workspace array IMCWRK(*);
C              LENIMC must be at least:
C              LENIMC = 4*KK + NLITE,
C              where KK is the total species count, and
C                    NLITE is the number of species with molecular
C                          weight less than 5.
C  LENRMC    - Integer scalar, minimum dimension of the real storage
C              and workspace array RMCWRK(*);
C              LENRMC must be at least:
C              LENRMC = KK*(19 + 2*NO + NO*NLITE) + (NO+15)*KK**2,
C              where KK is the total species count,
C                    NO is the order of the polynomial fits (NO=4),
C                    NLITE is the number of species with molecular
C                          weight less than 5.
C
C  OUTPUT
C  IMCWRK(*) - Integer workspace array; dimension at least LENIMC.
C  RMCWRK(*) - Real    workspace array; dimension at least LENRMC.
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,
     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST, NCST, NXL,
     4                NR, NWRK, K3
      COMMON /MCCONS/ VERS, PREC, KERR, LENI, LENR
C
      DIMENSION IMCWRK(*), RMCWRK(*)
      CHARACTER*16 PRVERS, VERS, PREC, PRDATE, IFMT, RFMT, CFMT, LFMT
      CHARACTER*80 MSGSTR
      PARAMETER
     1(CFMT='(8A16)', IFMT='(10I12)', LFMT='(L8)', RFMT='(1P,5E24.16)')
C
      LOGICAL IOK, ROK, KERR, LBIN
      INTEGER CKLSCH
      EXTERNAL CKLSCH
C
C     The following number SMALL is used in the mixture diffusion
C     coefficient calculation; its use allows a smooth and well-
C     defined diffusion coefficient as the mixture approaches a pure
C     species, even though stictlyh speaking there does not exist a
C     diffusion coefficient in this case.  The value of SMALL should
C     be small relative to any species mole fraction of importance,
C     but large enough to be represented on the computer.
C
C*****SMALL 1) 64 bit floats
C      SMALL = 1.0E-50
C*****END SMALL 1) 64 bit floats
C*****SMALL 2) 32 bit floats
      SMALL = 1.0E-20
C*****END SMALL 2) 32 bit floats
C
C     Gas constant as reported in 1993 CRC, (J. Research of
C     National Bureau of Standards, 92, 85, 1987).
C     ( 8.314510(70)E+07 Joules mol-1 K-1)
C
      RU    = 8.314510E+07
C
C     Standard atmosphere (defined as an exact quantity)
C
      PATMOS= 1.01325E+06
C
C     Write version number
C
C*****precision > double
      PREC = 'DOUBLE'
C*****END precision > double
C*****precision > single
C      PREC = 'SINGLE'
C*****END precision > single
C
      PRVERS ='4.6'
      PRDATE ='1999/06/03'
C
      WRITE (LOUT, '(/A, /1X,A, A, A, A, /A, /A, //)')
     1 ' TRANLIB:  CHEMKIN-III MULTICOMPONENT TRANSPORT LIBRARY,',
     2 PREC(1:CKLSCH(PREC)), ' PRECISION Vers. ',
     3 PRVERS(1:CKLSCH(PRVERS)+1), PRDATE,
     4 ' Copyright 1995, Sandia Corporation.',
     5' The U.S. Government retains a limited license in this software.'
C
C     Read the problem size
      CALL MCLEN (LINKMC, LOUT, LI, LR, IFLAG)
      IOK = (LENIMC .GE. LI)
      ROK = (LENRMC .GE. LR)
C
      IF (.NOT.IOK .OR. .NOT.ROK) THEN
         IF (.NOT. IOK) WRITE (LOUT, 300) LI
C <error module="tranlib" severity="error">
C <id>1</id>
C <message>The size of the TRANSPORT integer work array
C (variable IMCWRK) is too small.  You must increase the value of
C LENIMC from %1 to at least %2 in the transport driver and rebuild this
C program.
C </message>
C </error>
         IDERR = 1
         MSGSTR = ' '
         WRITE (MSGSTR, '(I12,A1)') LENIMC,'$', LI
         CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
         IF (.NOT. ROK) WRITE (LOUT, 350) LR
C <error module="tranlib" severity="error">
C <id>2</id>
C <message>The size of the TRANSPORT real work array
C (variable RMCWRK) is too small.  You must increase the value of
C LENRMC from %1 to at least %2 in the transport driver and rebuild this
C program.
C </mesage>
C </error>
         IDERR = 2
         MSGSTR = ' '
         WRITE (MSGSTR, '(I12,A1)') LENRMC,'$', LR
         CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
         REWIND (LINKMC)
         IFLAG = 1
         RETURN
      ENDIF
C
      REWIND LINKMC
C*****linkfile (transport) > binary
C      LBIN = .TRUE.
C*****END linkfile (transport) > binary
C*****linkfile (transport) > ascii
      LBIN = .FALSE.
C*****END linkfile (transport) > ascii
C
      NREC = 1
      IF (LBIN) THEN
         READ (LINKMC, ERR=999) VERS
         NREC = 2
         READ (LINKMC, ERR=999) PRVERS
         NREC = 3
         READ (LINKMC, ERR=999) PREC
         NREC = 4
         READ (LINKMC, ERR=999) KERR
         NREC = 5
         READ (LINKMC, ERR=999) LI, LR, NO, NKK, NLITE
         NREC = 6
         READ (LINKMC, ERR=999) PATMOS
      ELSE
         READ (LINKMC, CFMT, ERR=999) VERS
         NREC = 2
         READ (LINKMC, CFMT, ERR=999) PRVERS
         NREC = 3
         READ (LINKMC, CFMT, ERR=999) PREC
         NREC = 4
         READ (LINKMC, LFMT, ERR=999) KERR
         NREC = 5
         READ (LINKMC, IFMT, ERR=999) LI, LR, NO, NKK, NLITE
         NREC = 6
         READ (LINKMC, RFMT, ERR=999) PATMOS
      ENDIF
C
      NK  = NO*NKK
      NK2 = NO*NKK*NKK
      K2  = NKK*NKK
      K3  = 3*NKK
      K32 = K3*K3
      NKT = NO*NKK*NLITE
C
C     APPORTION THE REAL WORKSPACE:
C
C     molecular weights for the species
      NWT  = 1
C     the epsilon/k well depth for the species
      NEPS = NWT + NKK
C     the collision diameter for the species
      NSIG = NEPS + NKK
C     the dipole moments for the species
      NDIP = NSIG + NKK
C     the polarizabilities for the species
      NPOL = NDIP + NKK
C     the rotational relaxation collision numbers
      NZROT= NPOL + NKK
C     the fit coefficients for conductivity
      NLAM = NZROT + NKK
C     the fit coefficients for viscosity
      NETA = NLAM + NK
      NDIF = NETA + NK
C     the fit coefficients for thermal diffusion ratio
      NTDIF= NDIF + NK2
C     mole fractions of the mixture
      NXX  = NTDIF + NO*NKK*NLITE
C     species viscosities
      NVIS = NXX + NKK
C     rotational relaxation collision numbers before Parker coffection
      NXI  = NVIS + NKK
C     species specific heats
      NCP  = NXI + NKK
C     rotational parts of the specific heats
      NCROT= NCP + NKK
C     internal parts of the specific heats
      NCINT= NCROT + NKK
C     the binary diffusion coefficients
      NBIND= NCINT + NKK
C     the matrix of reduced well depths
      NEOK = NBIND + K2
C     the matrix of reduced collision diameters
      NSGM = NEOK + K2
C     the matrix of A* collision integrals for each species pair
      NAST = NSGM + K2
C     the matrix of B* collision integrals for each species pair
      NBST = NAST + K2
C     the matrix of C* collision integrals for each species pair
      NCST = NBST + K2
C     the "L" matrix
      NXL  = NCST + K2
C     the right-hand sides of the linear system involving the
C     "L" matrix
      NR   = NXL + K32
C     the workspace needed by LINPACK to solve the "L" matrix linear
C     system
      NWRK = NR + K3
C      NTOT = NWRK + K3 - 1
C
C     APPORTION THE INTEGER WORKSPACE:
C
C     the indicators for the molecule linearity
      INLIN = 1
C     the species indices for the "light" species
      IKTDIF= INLIN + NKK
C     the pivot indices for LINPACK calls
      IPVT  = IKTDIF + NLITE
C      ITOT  = IPVT + K3 - 1
C
C     Read the data from the linkfile
C
      IF (LBIN) THEN
         NREC = 7
         READ (LINKMC, ERR=999) (RMCWRK(NWT+N-1), N = 1, NKK)
         NREC = 8
         READ (LINKMC, ERR=999) (RMCWRK(NEPS+N-1), N = 1, NKK)
         NREC = 9
         READ (LINKMC, ERR=999) (RMCWRK(NSIG+N-1), N = 1, NKK)
         NREC = 10
         READ (LINKMC, ERR=999) (RMCWRK(NDIP+N-1), N = 1, NKK)
         NREC = 11
         READ (LINKMC, ERR=999) (RMCWRK(NPOL+N-1), N = 1, NKK)
         NREC = 12
         READ (LINKMC, ERR=999) (RMCWRK(NZROT+N-1), N = 1, NKK)
         NREC = 13
         READ (LINKMC, ERR=999) (IMCWRK(INLIN+N-1), N = 1, NKK)
         NREC = 14
         READ (LINKMC, ERR=999) (RMCWRK(NLAM+N-1), N = 1, NK)
         NREC = 15
         READ (LINKMC, ERR=999) (RMCWRK(NETA+N-1), N = 1, NK)
         NREC = 16
         READ (LINKMC, ERR=999) (RMCWRK(NDIF+N-1), N = 1, NK2)
         NREC = 17
         READ (LINKMC, ERR=999) (IMCWRK(IKTDIF+N-1), N = 1, NLITE)
         NREC = 18
         READ (LINKMC, ERR=999) (RMCWRK(NTDIF+N-1), N = 1, NKT)
      ELSE
         NREC = 7
         READ (LINKMC, RFMT, ERR=999) (RMCWRK(NWT+N-1), N = 1, NKK)
         NREC = 8
         READ (LINKMC, RFMT, ERR=999) (RMCWRK(NEPS+N-1), N = 1, NKK)
         NREC = 9
         READ (LINKMC, RFMT, ERR=999) (RMCWRK(NSIG+N-1), N = 1, NKK)
         NREC = 10
         READ (LINKMC, RFMT, ERR=999) (RMCWRK(NDIP+N-1), N = 1, NKK)
         NREC = 11
         READ (LINKMC, RFMT, ERR=999) (RMCWRK(NPOL+N-1), N = 1, NKK)
         NREC = 12
         READ (LINKMC, RFMT, ERR=999) (RMCWRK(NZROT+N-1), N = 1, NKK)
         NREC = 13
         READ (LINKMC, IFMT, ERR=999) (IMCWRK(INLIN+N-1), N = 1, NKK)
         NREC = 14
         READ (LINKMC, RFMT, ERR=999) (RMCWRK(NLAM+N-1), N = 1, NK)
         NREC = 15
         READ (LINKMC, RFMT, ERR=999) (RMCWRK(NETA+N-1), N = 1, NK)
         NREC = 16
         READ (LINKMC, RFMT, ERR=999) (RMCWRK(NDIF+N-1), N = 1, NK2)
         NREC = 17
         READ (LINKMC, IFMT, ERR=999) (IMCWRK(IKTDIF+N-1), N = 1, NLITE)
         NREC = 18
         READ (LINKMC, RFMT, ERR=999) (RMCWRK(NTDIF+N-1), N = 1, NKT)
         NREC = 19
      ENDIF
C
C     Set EPS/K and SIG for all I,J pairs
C
      CALL MCEPSG (NKK, RMCWRK(NEPS), RMCWRK(NSIG), RMCWRK(NDIP),
     1            RMCWRK(NPOL), RMCWRK(NEOK), RMCWRK(NSGM) )
C
  300 FORMAT (10X,'IMCWRK MUST BE DIMENSIONED AT LEAST ', I5)
  350 FORMAT (10X,'RMCWRK MUST BE DIMENSIONED AT LEAST ', I5)
      RETURN
  999 CONTINUE
C <error module="tranlib" severity="error">
C <id>3</id>
C <message>An error was encountered while trying to read
C the TRANSPORT linking file.  Check the output from the TRAN
C fitting routine for errors or rerun TRAN.
C </message>
C </error>
      IDERR = 3
      MSGSTR = ' '
      CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
      WRITE (LOUT, *) ' Error reading Transport linkfile...'
      REWIND (LINKMC)
      IFLAG = NREC
C
C     end of SUBROUTINE MCINIT
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCLEN (LINKMC, LOUT, LI, LR, IFLAG)
C
C  START PROLOGUE
C
C  SUBROUTINE MCLEN (LINKMC, LOUT, LI, LR, IFLAG)
C  Returns the lengths required for work arrays.
C
C  INPUT
C  LINKMC   - Integer scalar, input file unit for the linkfile.
C  LOUT     - Integer scalar, formatted output file unit.
C
C  OUTPUT
C  LI       - Integer scalar, minimum length required for the
C             integer work array.
C  LR       - Integer scalar, minimum length required for the
C             real work array.
C  IFLAG    - Integer scalar, indicates successful reading of
C             linkfile; IFLAG>0 indicates error type.
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
      COMMON /MCCONS/ VERS, PREC, KERR, LENI, LENR
      PARAMETER (NLIST = 1)
      LOGICAL KERR, VOK, POK, LBIN
      CHARACTER*16 LIST(NLIST), VERS, PREC, PRVERS, IFMT, RFMT, LFMT,
     1             CFMT
      CHARACTER*80 MSGSTR
      PARAMETER
     1(CFMT='(8A16)', IFMT='(10I12)', LFMT='(L8)', RFMT='(1P,5E24.16)')
      DATA LIST/'1.0'/
C
      IFLAG = 0
      VERS = ' '
      PREC = ' '
      LENI = 0
      LENR = 0
      LI   = LENI
      LR   = LENR
C
C*****linkfile (transport) > binary
C      LBIN = .TRUE.
C*****END linkfile (transport) > binary
C*****linkfile (transport) > ascii
      LBIN = .FALSE.
C*****END linkfile (transport) > ascii
C
      REWIND (LINKMC)
      NREC = 1
      IF (LBIN) THEN
         READ (LINKMC, ERR=999) VERS
         NREC = 2
         READ (LINKMC, ERR=999) PRVERS
         NREC = 3
         READ (LINKMC, ERR=999) PREC
         NREC = 4
         READ (LINKMC, ERR=999) KERR
      ELSE
         READ (LINKMC, CFMT, ERR=999) VERS
         NREC = 2
         READ (LINKMC, CFMT, ERR=999) PRVERS
         NREC = 3
         READ (LINKMC, CFMT, ERR=999) PREC
         NREC = 4
         READ (LINKMC, LFMT, ERR=999) KERR
      ENDIF
C
      VOK = .FALSE.
      DO 5 N = 1, NLIST
         IF (VERS .EQ. LIST(N)) VOK = .TRUE.
    5 CONTINUE
C
      POK = .FALSE.
C*****precision > double
      IF (INDEX(PREC, 'DOUB') .GT. 0) POK = .TRUE.
C*****END precision > double
C*****precision > single
C      IF (INDEX(PREC, 'SING') .GT. 0) POK = .TRUE.
C*****END precision > single
C
      IF (KERR .OR. (.NOT.POK) .OR. (.NOT.VOK)) THEN
         IF (KERR) THEN
C <error module="tranlib" severity="error">
C <id>4</id>
C <message>There is an error in the transport linkfile...',
C Check TRAN output for error conditions.'
C </message>
C </error>
            IDERR = 4
            MSGSTR = ' '
            CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
            WRITE (LOUT,'(/A,/A)')
     1      ' There is an error in the transport linkfile...',
     2      ' Check TRANFIT output for error conditions.'
         ENDIF
         IF (.NOT. VOK) THEN
C <error module="tranlib" severity="error">
C <id>5</id>
C <message>The transport linkfile is incompatible with ',
C this version of the Transport Library.  Rerun the TRAN program.
C </message>
C </error>
            IDERR = 5
            MSGSTR = ' '
            CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
            WRITE (LOUT,'(/A,A)')
     1      ' Transport linkfile is incompatible with Transport',
     2      ' Library Version 3.6'
         ENDIF
         IF (.NOT. POK) THEN
C <error module="tranlib" severity="error">
C <id>6</id>
C <message>The floating-point precision in the transport
C linkfile is incompatible with that of the Transport Library.
C Rerun the TRAN program.
C </message>
C </error>
            IDERR = 6
            MSGSTR = ' '
            CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
            WRITE (LOUT, '(/A,A)')
     1      ' Precision of Transport linkfile does not agree with',
     2      ' precision of Transport Library'
         ENDIF
C <error module="tranlib" severity="error">
C <id>7</id>
C <message>Returning from MCLEN with fatal error condition.
C </message>
C </error>
         IDERR = 7
         MSGSTR = ' '
         CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
         IFLAG = 21
         RETURN
      ENDIF
C
      NREC = 5
      IF (LBIN) THEN
         READ (LINKMC, ERR=1111) LENIMC, LENRMC, NO, NKK, NLITE
      ELSE
         READ (LINKMC, IFMT, ERR=1111) LENIMC, LENRMC, NO, NKK, NLITE
      ENDIF
      REWIND (LINKMC)
      LENI = LENIMC
      LENR = LENRMC
      LI   = LENI
      LR   = LENR
      RETURN
C
  999 CONTINUE
C <error module="tranlib" severity="error">
C <id>8</id>
C <message>An error was encountered during READ of the
C records 1-4 of the transport linkfile in subroutine MCLEN.
C Rerun the TRAN program and check for errors in the output.
C </message>
C </error>
      IDERR = 8
      MSGSTR = ' '
      CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
      WRITE (LOUT, 50)
   50 FORMAT
     1 (' Error reading Transport linkfile.')
      REWIND (LINKMC)
      IFLAG = NREC
      RETURN
 1111 CONTINUE
C <error module="tranlib" severity="error">
C <id>9</id>
C <message>An error was encountered during READ of
C record #5 of the transport linkfile in subroutine MCLEN.
C Rerun the TRAN program and check for errors in the output.
C </message>
C </error>
      IDERR = 9
      MSGSTR = ' '
      CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
      WRITE (LOUT, 50)
      REWIND (LINKMC)
      IFLAG = 22
C
C     end of SUBROUTINE MCLEN
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCPNT (LSAVE, LOUT, NPOINT, V, P, LI, LR, IERR)
C
C  START PROLOGUE
C
C  SUBROUTINE MCPNT (LSAVE, LOUT, NPOINT, V, P, LI, LR, IERR)
C  Reads from a binary file information about a Transport linkfile,
C  pointers for the Transport Library, and returns lengths of work
C  arrays.
C
C  INPUT
C  LSAVE  - Integer scalar, input unit for binary data file.
C  LOUT   - Integer scalar, formatted output file unit.
C
C  OUTPUT
C  NPOINT - Integer scalar, total number of pointers.
C  V      - Real scalar, version number of the Transport linkfile.
C  P      - Character string, machine precision of the linkfile.
C  LI     - Integer scalar, minimum dimension required for integer
C           workspace array.
C  LR     - Integer scalar, minimumm dimension required for real
C           workspace array.
C  IERR   - Logical, error flag.
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT,  NEPS,  NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF,  NTDIF, NXX, NVIS, NXI,  NCP,  NCROT,
     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST, NCST,
     4                NXL, NR, NWRK, K3
      COMMON /MCCONS/ VERS, PREC, KERR, LENI, LENR
C
      LOGICAL KERR, IERR
      CHARACTER PREC*16, VERS*16, P*16, V*16
      CHARACTER*80 MSGSTR
C
      KERR = .FALSE.
      IERR = .FALSE.
C
      READ (LSAVE, ERR=100) NPOINT, VERS, PREC, LENI, LENR,
     *                RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP,
     3                NCROT, NCINT, NBIND, NEOK, NSGM,
     4                NAST, NBST, NCST, NXL, NR, NWRK, K3
      LI = LENI
      LR = LENR
      IERR = KERR
      V = VERS
      P = PREC
      RETURN
C
  100 CONTINUE
C <error module="tranlib" severity="error">
C <id>10</id>
C <message>An error was encountered during READ of the
C the transport linkfile in subroutine MCPNT.
C Rerun the TRAN program and check for errors in the output.
C </message>
C </error>
      IDERR = 10
      MSGSTR = ' '
      CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
      WRITE (LOUT, *) ' Error reading Transport linkfile data...'
      KERR   = .TRUE.
      IERR   = KERR
      NPOINT = 0
      VERS   = ' '
      V      = VERS
      PREC   = ' '
      P      = PREC
C
C     end of SUBROUTINE MCPNT
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCPRAM (IMCWRK, RMCWRK, EPS, SIG, DIP, POL, ZROT, NLIN)
C
C  START PROLOGUE
C
C  SUBROUTINE MCPRAM (IMCWRK, RMCWRK, EPS, SIG, DIP, POL, ZROT, NLIN)
C  Returns the arrays of molecular parameters as read from the
C  transport database.
C
C  INPUT
C  IMCWRK(*) - Integer workspace array; dimension at least LENIMC.
C  RMCWRK(*) - Real    workspace array; dimension at least LENRMC.
C
C  OUTPUT
C  EPS(*)    - Real array, Lennard-Jones Potential well depths for
C              the species;
C              dimension at least KK, the total species count.
C                 cgs units, K
C  SIG(*)    - Real array, Lennary-Jones collision diameters for
C              the species;
C              dimension at least KK, the total species count.
C                 cgs units, Angstrom
C  DIP(*)    - Real array, dipole moments for the species;
C              dimension at least KK, the total species count.
C                 cgs units, Debye
C  POL(*)    - Real array, polarizabilities for the species;
C              dimension at least KK, the total species count.
C                 cgs units, Angstrom**3
C  ZROT(*)   - Real array, rotational collision numbers evaluated at
C              298K for the species;
C              dimension at least KK, the total species count.
C  NLIN(*)   - Integer array, flags for species linearity;
C              dimension at least KK, the total species count.
C              NLIN=0, single atom,
C              NLIN=1, linear molecule,
C              NLIN=2, linear molecule.
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,
     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST, NCST,
     4                NXL, NR, NWRK, K3
      DIMENSION IMCWRK(*), RMCWRK(*), EPS(*), SIG(*), DIP(*), POL(*),
     1          ZROT(*), NLIN(*)
C
      DO 100 K = 1, NKK
         KIND = K - 1
         EPS(K) = RMCWRK(NEPS + KIND)
         SIG(K) = RMCWRK(NSIG + KIND)
         DIP(K) = RMCWRK(NDIP + KIND)
         POL(K) = RMCWRK(NPOL + KIND)
         ZROT(K)= RMCWRK(NZROT+ KIND)
         NLIN(K)= IMCWRK(INLIN+ KIND)
  100 CONTINUE
C
C     end of SUBROUTINE MCPRAM
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCREWR (LINKMC, LOUT, IMCWRK, RMCWRK, IFLAG)
C
C  START PROLOGUE
C
C  SUBROUTINE MCREWR (LINKMC, LOUT, IMCWRK, RMCWRK, IFLAG)
C  This subroutine writes a new the transport linkfile from
C  the data stored in the integer and real work arrays,
C  IMCWRK(*) and RMCWRK(*).
C
C  INPUT
C  LINKMC    - Integer scalar, transport linkfile output unit number.
C  LOUT      - Integer scalar, formatted output file unit number.
C
C  OUTPUT
C  IMCWRK(*) - Integer workspace array; dimension at least LENIMC.
C  RMCWRK(*) - Real    workspace array; dimension at least LENRMC.
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,
     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST, NCST, NXL,
     4                NR, NWRK, K3
      COMMON /MCCONS/ VERS, PREC, KERR, LENI, LENR
C
      DIMENSION IMCWRK(*), RMCWRK(*)
      CHARACTER*16 PRVERS, VERS, PREC, IFMT, RFMT, CFMT, LFMT
      CHARACTER*80 MSGSTR
      PARAMETER
     1(CFMT='(8A16)', IFMT='(10I12)', LFMT='(L8)', RFMT='(1P,5E24.16)')
C
      LOGICAL KERR, LBIN
C
C     Write version number
C
C*****precision > double
      PREC = 'DOUBLE'
C*****END precision > double
C*****precision > single
C      PREC = 'SINGLE'
C*****END precision > single
C
      PRVERS ='4.6'
C
      REWIND LINKMC
C*****linkfile (transport) > binary
C      LBIN = .TRUE.
C*****END linkfile (transport) > binary
C*****linkfile (transport) > ascii
      LBIN = .FALSE.
C*****END linkfile (transport) > ascii
C
      NREC = 1
      IF (LBIN) THEN
         WRITE (LINKMC, ERR=999) VERS
         NREC = 2
         WRITE (LINKMC, ERR=999) PRVERS
         NREC = 3
         WRITE (LINKMC, ERR=999) PREC
         NREC = 4
         WRITE (LINKMC, ERR=999) KERR
         NREC = 5
         WRITE (LINKMC, ERR=999) LENI, LENR, NO, NKK, NLITE
         NREC = 6
         WRITE (LINKMC, ERR=999) PATMOS
      ELSE
         WRITE (LINKMC, CFMT, ERR=999) VERS
         NREC = 2
         WRITE (LINKMC, CFMT, ERR=999) PRVERS
         NREC = 3
         WRITE (LINKMC, CFMT, ERR=999) PREC
         NREC = 4
         WRITE (LINKMC, LFMT, ERR=999) KERR
         NREC = 5
         WRITE (LINKMC, IFMT, ERR=999) LENI, LENR, NO, NKK, NLITE
         NREC = 6
         WRITE (LINKMC, RFMT, ERR=999) PATMOS
      ENDIF
C
      NK  = NO*NKK
      NK2 = NO*NKK*NKK
      K2  = NKK*NKK
      K3  = 3*NKK
      K32 = K3*K3
      NKT = NO*NKK*NLITE
C
C     Write the data to the linkfile
C
      IF (LBIN) THEN
         NREC = 7
         WRITE (LINKMC, ERR=999) (RMCWRK(NWT+N-1), N = 1, NKK)
         NREC = 8
         WRITE (LINKMC, ERR=999) (RMCWRK(NEPS+N-1), N = 1, NKK)
         NREC = 9
         WRITE (LINKMC, ERR=999) (RMCWRK(NSIG+N-1), N = 1, NKK)
         NREC = 10
         WRITE (LINKMC, ERR=999) (RMCWRK(NDIP+N-1), N = 1, NKK)
         NREC = 11
         WRITE (LINKMC, ERR=999) (RMCWRK(NPOL+N-1), N = 1, NKK)
         NREC = 12
         WRITE (LINKMC, ERR=999) (RMCWRK(NZROT+N-1), N = 1, NKK)
         NREC = 13
         WRITE (LINKMC, ERR=999) (IMCWRK(INLIN+N-1), N = 1, NKK)
         NREC = 14
         WRITE (LINKMC, ERR=999) (RMCWRK(NLAM+N-1), N = 1, NK)
         NREC = 15
         WRITE (LINKMC, ERR=999) (RMCWRK(NETA+N-1), N = 1, NK)
         NREC = 16
         WRITE (LINKMC, ERR=999) (RMCWRK(NDIF+N-1), N = 1, NK2)
         NREC = 17
         WRITE (LINKMC, ERR=999) (IMCWRK(IKTDIF+N-1), N = 1, NLITE)
         NREC = 18
         WRITE (LINKMC, ERR=999) (RMCWRK(NTDIF+N-1), N = 1, NKT)
      ELSE
         NREC = 7
         WRITE (LINKMC, RFMT, ERR=999) (RMCWRK(NWT+N-1), N = 1, NKK)
         NREC = 8
         WRITE (LINKMC, RFMT, ERR=999) (RMCWRK(NEPS+N-1), N = 1, NKK)
         NREC = 9
         WRITE (LINKMC, RFMT, ERR=999) (RMCWRK(NSIG+N-1), N = 1, NKK)
         NREC = 10
         WRITE (LINKMC, RFMT, ERR=999) (RMCWRK(NDIP+N-1), N = 1, NKK)
         NREC = 11
         WRITE (LINKMC, RFMT, ERR=999) (RMCWRK(NPOL+N-1), N = 1, NKK)
         NREC = 12
         WRITE (LINKMC, RFMT, ERR=999) (RMCWRK(NZROT+N-1), N = 1, NKK)
         NREC = 13
         WRITE (LINKMC, IFMT, ERR=999) (IMCWRK(INLIN+N-1), N = 1, NKK)
         NREC = 14
         WRITE (LINKMC, RFMT, ERR=999) (RMCWRK(NLAM+N-1), N = 1, NK)
         NREC = 15
         WRITE (LINKMC, RFMT, ERR=999) (RMCWRK(NETA+N-1), N = 1, NK)
         NREC = 16
         WRITE (LINKMC, RFMT, ERR=999) (RMCWRK(NDIF+N-1), N = 1, NK2)
         NREC = 17
         WRITE (LINKMC, IFMT, ERR=999)
     &         (IMCWRK(IKTDIF+N-1), N = 1, NLITE)
         NREC = 18
         WRITE (LINKMC, RFMT, ERR=999) (RMCWRK(NTDIF+N-1), N = 1, NKT)
         NREC = 19
      ENDIF
C
      RETURN
  999 CONTINUE
C <error module="tranlib" severity="error">
C <id>3</id>
C <message>An error was encountered while trying to write
C a new TRANSPORT linking file, in subroutine MCREWR.
C </message>
C </error>
      IDERR = 11
      MSGSTR = ' '
      CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
      WRITE (LOUT, *) ' Error writing new Transport linkfile...'
      REWIND (LINKMC)
      IFLAG = NREC
C
C     end of SUBROUTINE MCREWR
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCSAVE (LOUT, LSAVE, IMCWRK, RMCWRK)
C
C  START PROLOGUE
C
C  SUBROUTINE MCSAVE (LOUT, LSAVE, IMCWRK, RMCWRK)
C  Writes to a binary file information about a Transport linkfile,
C  pointers for the Transport library, and Transport work arrays.
C
C  INPUT
C  LOUT      - Integer scalar, formatted output file unit number.
C  LSAVE     - Integer scalar, unformatted output file unit number.
C  IMCWRK(*) - Integer workspace array; dimension at least LENIMC.
C  RMCWRK(*) - Real    workspace array; dimension at least LENRMC.
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP,
     3                NCROT, NCINT, NBIND, NEOK, NSGM,
     4                NAST, NBST, NCST, NXL, NR, NWRK, K3
      COMMON /MCCONS/ VERS, PREC, KERR, LENI, LENR
      DIMENSION IMCWRK(*), RMCWRK(*)
      CHARACTER VERS*16, PREC*16
      CHARACTER*80 MSGSTR
      LOGICAL KERR
C
      NPOINT = 41
      WRITE (LSAVE,ERR=999)   NPOINT, VERS,   PREC,   LENI,   LENR,
     *                RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP,
     3                NCROT, NCINT, NBIND, NEOK, NSGM,
     4                NAST, NBST, NCST, NXL, NR, NWRK, K3
      WRITE (LSAVE,ERR=999) (IMCWRK(L), L = 1, LENI)
      WRITE (LSAVE,ERR=999) (RMCWRK(L), L = 1, LENR)
C
C     end of SUBROUTINE MCSAVE
      RETURN
  999 CONTINUE
C
C <error module="tranlib" severity="error">
C <id>11</id>
C <message>An error was encountered during WRITE of the
C the transport linkfile information in subroutine MCSAVE.
C Rerun the TRAN program and check for errors in the output.
C </message>
C </error>
      IDERR = 12
      MSGSTR = ' '
      CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
      WRITE (LOUT, *)
     1    ' Error writing Transport linkfile information...'
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCSVIS (T, RMCWRK, VIS)
C
C  START PROLOGUE
C
C  SUBROUTINE MCSVIS (T, RMCWRK, VIS)
C  Returns the array of pure species viscosities, given temperature.
C
C  INPUT
C  T         - Real scalar, temperature.
C                 cgs units, K
C  RMCWRK(*) - Real workspace array; dimension at least LENRMC.
C
C  OUTPUT
C  VIS(*)    - Real array, species viscosities;
C              dimension at least KK, the total species count.
C                 cgs units, gm/cm*s
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,
     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST,
     4                NCST, NXL, NR, NWRK, K3
      DIMENSION VIS(*), RMCWRK(*)
C
      ALOGT = LOG(T)
      IF (NO .EQ. 4) THEN
        CALL MCEVAL4 (ALOGT, NKK, RMCWRK(NETA), VIS)
      ELSE
        CALL MCEVAL (ALOGT, NKK, NO, RMCWRK(NETA), VIS)
      ENDIF
C
      DO 25 K = 1, NKK
         VIS(K) = EXP(VIS(K))
   25 CONTINUE
C
C     end of SUBROUTINE MCSVIS
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCAVIS (T, X, RMCWRK, VISMIX)
C
C  START PROLOGUE
C
C  SUBROUTINE MCAVIS (T, X, RMCWRK, VISMIX)
C  Returns mixture viscosity, given temperature and species mole
C  fractions.  It uses modification of the Wilke semi-empirical
C  formulas.
C
C  INPUT
C  T         - Real scalar, temperature.
C                 cgs units, K
C  X(*)      - Real array, mole fractions of the mixture;
C              dimension at least KK, the total species count.
C  RMCWRK(*) - Real workspace array; dimension at least LENRMC.
C
C  OUTPUT
C  VISMIX    - Real scalar, mixture viscosity.
C                 cgs units, gm/cm*s
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
      PARAMETER (ZERO=0.0D0, ONE=1.0D0, EIGHT=8.0D0)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C      PARAMETER (ZERO=0.0, ONE=1.0, EIGHT=8.0)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,
     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST,
     4                NCST, NXL, NR, NWRK, K3
      DIMENSION X(*), RMCWRK(*)
C
C     In the following call, the species molecular weights are stored
C     in RMCWRK(NWT) and the pure species viscosities are in
C     RMCWRK(NVIS)
C
      ALOGT = LOG(T)
      IF (NO .EQ. 4) THEN
        CALL MCEVAL4 (ALOGT, NKK, RMCWRK(NETA), RMCWRK(NVIS))
      ELSE
        CALL MCEVAL (ALOGT, NKK, NO, RMCWRK(NETA), RMCWRK(NVIS))
      ENDIF
C
      DO 25 K = 1, NKK
         RMCWRK(NVIS+K-1) = EXP(RMCWRK(NVIS+K-1))
   25 CONTINUE
C
      SUMO = ZERO
      DO 200 K = 1, NKK
C
         SUMI = ZERO
         DO 100 J = 1, NKK
            SUMI = SUMI + X(J) *
     $             (ONE +
     $              SQRT( RMCWRK(NVIS+K-1) / RMCWRK(NVIS+J-1) *
     $                    SQRT( RMCWRK(NWT+J-1)/RMCWRK(NWT+K-1))
     $                  )
     $             )**2   /
     $             SQRT(ONE + RMCWRK(NWT+K-1) / RMCWRK(NWT+J-1))
  100    CONTINUE
C
         SUMO = SUMO + X(K) * RMCWRK(NVIS+K-1) / SUMI
  200 CONTINUE
C
      VISMIX = SUMO * SQRT(EIGHT)
C
C     end of SUBROUTINE MCAVIS
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCSCON (T, RMCWRK, CON)
C
C  START PROLOGUE
C
C  SUBROUTINE MCSCON (T, RMCWRK, CON)
C  Returns the array of pur species conductivities given temperature.
C
C  INPUT
C  T         - Real scalar, temperature.
C                cgs units, K
C  RMCWRK(*) - Real workspace array; dimension at least LENRMC.
C
C  OUTPUT
C  CON(*)    - Real array, species thermal conductivities;
C              dimension at least KK, the total species count.
C                cgs units, erg/cm*K*s
C  END PROLOGUE
C
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,
     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST,
     4                NCST, NXL, NR, NWRK, K3
      DIMENSION CON(*), RMCWRK(*)
C
      ALOGT = LOG(T)
      IF (NO .EQ. 4) THEN
        CALL MCEVAL4 (ALOGT, NKK, RMCWRK(NLAM), CON)
      ELSE
        CALL MCEVAL (ALOGT, NKK, NO, RMCWRK(NLAM), CON)
      ENDIF
C
      DO 25 K = 1, NKK
         CON(K) = EXP(CON(K))
   25 CONTINUE
C
C     end of SUBROUTINE MCSCON
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCACON (T, X, RMCWRK, CONMIX)
C
C  START PROLOGUE
C
C  SUBROUTINE MCACON (T, X, RMCWRK, CONMIX)
C  Returns the mixture thermal conductivity given temperature and
C  species mole fractions.
C
C  INPUT
C  T         - Real scalar, temperature.
C                 cgs units, K
C  X(*)      - Real array, mole fractions of the mixture;
C              dimension at least KK, the total species count.
C  RMCWRK(*) - Real workspace array; dimension at least LENRMC.
C
C  OUTPUT
C  CONMIX    - Real scalar, mixture thermal conductivity.
C                 cgs units, erg/cm*K*s
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
      PARAMETER (ZERO=0.0D0, ONE=1.0D0)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C      PARAMETER (ZERO=0.0, ONE=1.0)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,
     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST,
     4                NCST, NXL, NR, NWRK, K3
      DIMENSION X(*), RMCWRK(*)
C
C     In the following call, the pure species conductivities are in
C     RMCWRK(NXI)
C
      ALOGT = LOG(T)
      IF (NO .EQ. 4) THEN
        CALL MCEVAL4 (ALOGT, NKK, RMCWRK(NLAM), RMCWRK(NXI))
      ELSE
        CALL MCEVAL (ALOGT, NKK, NO, RMCWRK(NLAM), RMCWRK(NXI))
      ENDIF
C
      SUM = ZERO
      SUMR = ZERO
      DO 100 K = 1, NKK
         RMCWRK(NXI+K-1) = EXP(RMCWRK(NXI+K-1))
         SUM =  SUM  + X(K)*RMCWRK(NXI+K-1)
         SUMR = SUMR + X(K)/RMCWRK(NXI+K-1)
  100 CONTINUE
C
      CONMIX = 0.5 * (SUM + ONE/SUMR)
C
C     end of SUBROUTINE MCACON
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCSDIF (P, T, KDIM, RMCWRK, DJK)
C
C  START PROLOGUE
C
C  SUBROUTINE MCSDIF (P, T, KDIM, RMCWRK, DJK)
C  Returns the binary diffusion coefficients given pressure and
C  temperature.
C
C  INPUT
C  P         - Real scalar, pressure.
C                 cgs units, dynes/cm**2
C  T         - Real scalar, temperature.
C                 cgs units, K
C  KDIM      - Integer scalar, actual first dimension of DJK(KDIM,KK).
C  RMCWRK(*) - Real workspace array; dimension at least LENRMC.
C
C  OUTPUT
C  DJK(*,*)  - Real matrix, binary diffusion coefficients;
C              dimension at least KK, the total species count, for
C              both the first and second dimensions.
C                 cgs units, cm**2/s
C              CJK(J,K) is the diffusion coefficient of species J
C              in species K.
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,
     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST,
     4                NCST, NXL, NR, NWRK, K3
      DIMENSION RMCWRK(*), DJK(KDIM,*)
C
      PFAC = PATMOS/P
      ALOGT = LOG(T)
C
C     Use the fact that DJK(J,K) = DJK(K,J) here.
C     Also, call a hard-wired horner's routine for NO=4.
C
      NOKK = NO * NKK
      ISTART = NDIF
      IF (NO .EQ. 4) THEN
        DO 100 K = 1, NKK
           CALL MCEVAL4 (ALOGT, K, RMCWRK(ISTART), DJK(1,K))
#ifdef VECTORVERSION
CMRF
C Cray optimizations by Mark Fahey(ORNL)
           DO 90 J = 1, NKK
              DJK(J,K) = EXP(DJK(J,K)) * PFAC
   90      CONTINUE
#else
           DO 90 J = 1, K
              DJK(J,K) = EXP(DJK(J,K)) * PFAC
              DJK(K,J) = DJK(J,K)
   90      CONTINUE
#endif
           ISTART = ISTART + NOKK
  100   CONTINUE
      ELSE
C
         DO 200 K = 1, NKK
            CALL MCEVAL (ALOGT, NKK, NO, RMCWRK(ISTART), DJK(1,K))
            DO 190 J = 1, NKK
               DJK(J,K) = EXP(DJK(J,K)) * PFAC
  190       CONTINUE
            ISTART = ISTART + NOKK
  200    CONTINUE
      ENDIF
C
C     end of SUBROUTINE MCSDIF
      RETURN
      END
CC                                                                      C
CC----------------------------------------------------------------------C
CC                                                                      C
C      SUBROUTINE MCADIF (P, T, X, RMCWRK, D)
CC
CC  START PROLOGUE
CC
CC  SUBROUTINE MCADIF (P, T, X, RMCWRK, D)
CC  Returns mixture-averaged diffusion coefficients given pressure,
CC  temperature, and species mole fractions.
CC
CC  INPUT
CC  P         - Real scalar, pressure.
CC                 cgs units, dynes/cm**2
CC  T         - Real scalar, temperature.
CC                 cgs units, K
CC  X(*)      - Real array, mole fractions of the mixture;
CC              dimension at least KK, the total species count.
CC  RMCWRK(*) - Real workspace array; dimension at least LENRMC.
CC
CC  OUTPUT
CC  D(*)      - Real array, mixture diffusion coefficients;
CC              dimension at least KK, the total species count.
CC                 cgs units, cm**2/s
CC
CC  END PROLOGUE
CC
CC*****precision > double
C      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
CC*****END precision > double
CC*****precision > single
CC      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
CC*****END precision > single
CC
C      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
C     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
C     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,
C     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST,
C     4                NCST, NXL, NR, NWRK, K3
C      DIMENSION X(*), D(*), RMCWRK(*)
CC
C      CALL MCEDIF (T, NO, NKK, X, RMCWRK(NDIF), RMCWRK(NWT), SMALL,
C     1             RMCWRK(NXX), RMCWRK(NBIND), D)
CC
C      PFAC = PATMOS / P
C      DO 100 K = 1, NKK
C         D(K) = D(K) * PFAC
C  100 CONTINUE
CC
CC     end of SUBROUTINE MCADIF
C      RETURN
C      END
C                                                                      C

C
C----------------------------------------------------------------------
C  Mixture averaged diffusion coefficients
C  For reduced n-Heptane mechanism (77 species)
C  Ref. Tianfeng and Law, CF 148, 2007
C----------------------------------------------------------------------
C
      SUBROUTINE MCADIF (P, T, XX, RMCWRK, D)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C
      DIMENSION XX(*), D(*), RMCWRK(*)
      DIMENSION X(77), Y(77), WT(77), XXWT(77)
      DIMENSION DG(17,17), XG(17), QG(17)
      DATA WT/1.00797, 2.01594, 15.9994, 31.9988, 
     *  17.0074, 18.0153, 28.0106, 29.0185, 
     *  44.01, 15.0351, 16.043, 33.0068, 
     *  34.0147, 30.0265, 31.0345, 30.0701, 
     *  28.0542, 29.0622, 25.0303, 26.0382, 
     *  27.0462, 32.0424, 31.0345, 42.0376, 
     *  41.0297, 43.0456, 43.0456, 44.0536, 
     *  40.0653, 40.0653, 42.0813, 54.0924, 
     *  43.0892, 43.0892, 55.1004, 55.1004, 
     *  56.1084, 57.1163, 57.0727, 58.0807, 
     *  69.1275, 70.1355, 71.1434, 45.0616, 
     *  47.0339, 48.0418, 43.0456, 55.0568, 
     *  56.0647, 57.0727, 71.0998, 41.0733, 
     *  39.0574, 38.0494, 14.0271, 70.0918, 
     *  71.0998, 70.0918, 72.1078, 71.0998, 
     *  71.0998, 56.0647, 97.138, 86.1349, 
     *  85.1269, 70.0918, 69.0839, 85.1705, 
     *  100.206, 99.1976, 99.1976, 131.196, 
     *  131.196, 163.195, 114.189, 146.188, 
     *  28.0134/
C
      SUMXW = ZERO
      SUMXX = ZERO
      DO K = 1, 77
         X(K) = MAX(XX(K), 1.0D-50)
         SUMXX = SUMXX + X(K)
         XXWT(K) = X(K)*WT(K)
         SUMXW = SUMXW + XXWT(K)
      ENDDO
      DO K = 1, 77
         X(K) = X(K)/SUMXX
         Y(K) = XXWT(K)/SUMXW
      ENDDO
C
      ZERO = 0.0D0
      ALOGT = LOG(T)
      PA = P/1.01325D+06
C
      XG(  1) = +X(  1) 
      XG(  2) = +X(  2) 
      XG(  3) = +X(  3) +X(  5) 
      XG(  4) = +X(  4) +X(  7) +X( 12) +X( 13) +X( 77) 
      XG(  5) = +X(  6) 
      XG(  6) = +X(  8) +X(  9) +X( 14) +X( 15) +X( 22) 
     *          +X( 23) +X( 54) 
      XG(  7) = +X( 10) +X( 11) +X( 55) 
      XG(  8) = +X( 16) +X( 18) 
      XG(  9) = +X( 17) +X( 19) +X( 20) +X( 21) 
      XG( 10) = +X( 24) +X( 26) +X( 27) +X( 28) +X( 29) 
     *          +X( 30) +X( 31) +X( 45) +X( 46) +X( 47) +X( 48) 
     *          +X( 52) +X( 53) +X( 62) 
      XG( 11) = +X( 25) 
      XG( 12) = +X( 32) +X( 33) +X( 34) +X( 35) +X( 36) 
     *          +X( 37) +X( 44) +X( 50) 
      XG( 13) = +X( 38) +X( 39) +X( 40) +X( 41) +X( 42) 
     *          +X( 43) +X( 49) +X( 51) +X( 56) +X( 57) +X( 58) 
     *          +X( 59) +X( 60) +X( 61) +X( 66) +X( 67) 
      XG( 14) = +X( 63) +X( 64) +X( 65) +X( 68) 
      XG( 15) = +X( 69) +X( 70) +X( 71) +X( 72) +X( 73) 
     *          +X( 75) 
      XG( 16) = +X( 74) 
      XG( 17) = +X( 76) 
C
      DG(1,1)=EXP(-15.1908 + ALOGT*(4.38431
     1           +ALOGT*(-0.355755+ALOGT*0.0154801)))
      DG(2,1)=EXP(-12.0191 + ALOGT*(3.03314
     1           +ALOGT*(-0.186015+ALOGT*0.00836163)))
      DG(2,2)=EXP(-10.3183 + ALOGT*(2.19241
     1           +ALOGT*(-0.0753904+ALOGT*0.00351123)))
      DG(3,1)=EXP(-15.3129 + ALOGT*(4.27089
     1           +ALOGT*(-0.348233+ALOGT*0.015451)))
      DG(3,2)=EXP(-10.9337 + ALOGT*(2.30613
     1           +ALOGT*(-0.0873643+ALOGT*0.00389753)))
      DG(3,3)=EXP(-13.5685 + ALOGT*(3.06218
     1           +ALOGT*(-0.188969+ALOGT*0.00845414)))
      DG(4,1)=EXP(-17.6111 + ALOGT*(5.04825
     1           +ALOGT*(-0.449072+ALOGT*0.0198133)))
      DG(4,2)=EXP(-12.7071 + ALOGT*(2.92962
     1           +ALOGT*(-0.173967+ALOGT*0.0079012)))
      DG(4,3)=EXP(-15.1117 + ALOGT*(3.51544
     1           +ALOGT*(-0.24912+ALOGT*0.0111163)))
      DG(4,4)=EXP(-16.0508 + ALOGT*(3.68823
     1           +ALOGT*(-0.269095+ALOGT*0.011875)))
      DG(5,1)=EXP(-16.5132 + ALOGT*(4.2207
     1           +ALOGT*(-0.282595+ALOGT*0.0101807)))
      DG(5,2)=EXP(-18.185 + ALOGT*(5.04362
     1           +ALOGT*(-0.437707+ALOGT*0.0188764)))
      DG(5,3)=EXP(-19.0651 + ALOGT*(4.98906
     1           +ALOGT*(-0.419283+ALOGT*0.0176187)))
      DG(5,4)=EXP(-20.298 + ALOGT*(5.16799
     1           +ALOGT*(-0.426063+ALOGT*0.0172536)))
      DG(5,5)=EXP(-11.8024 + ALOGT*(0.888791
     1           +ALOGT*(0.24617+ALOGT*-0.0160706)))
      DG(6,1)=EXP(-17.2503 + ALOGT*(4.56692
     1           +ALOGT*(-0.350919+ALOGT*0.0140269)))
      DG(6,2)=EXP(-15.6474 + ALOGT*(4.02583
     1           +ALOGT*(-0.31812+ALOGT*0.0142214)))
      DG(6,3)=EXP(-17.9367 + ALOGT*(4.48988
     1           +ALOGT*(-0.369756+ALOGT*0.0160925)))
      DG(6,4)=EXP(-18.6851 + ALOGT*(4.56551
     1           +ALOGT*(-0.375454+ALOGT*0.0161731)))
      DG(6,5)=EXP(-19.2482 + ALOGT*(4.35673
     1           +ALOGT*(-0.283583+ALOGT*0.00964712)))
      DG(6,6)=EXP(-20.7468 + ALOGT*(5.12591
     1           +ALOGT*(-0.430004+ALOGT*0.0178019)))
      DG(7,1)=EXP(-18.0164 + ALOGT*(5.09009
     1           +ALOGT*(-0.445755+ALOGT*0.0192914)))
      DG(7,2)=EXP(-13.5842 + ALOGT*(3.23985
     1           +ALOGT*(-0.21505+ALOGT*0.00971523)))
      DG(7,3)=EXP(-15.6169 + ALOGT*(3.70389
     1           +ALOGT*(-0.271241+ALOGT*0.0119728)))
      DG(7,4)=EXP(-17.0709 + ALOGT*(4.11981
     1           +ALOGT*(-0.324284+ALOGT*0.0142318)))
      DG(7,5)=EXP(-20.4245 + ALOGT*(5.22244
     1           +ALOGT*(-0.432499+ALOGT*0.0175196)))
      DG(7,6)=EXP(-19.2314 + ALOGT*(4.77165
     1           +ALOGT*(-0.394497+ALOGT*0.0166728)))
      DG(7,7)=EXP(-17.7299 + ALOGT*(4.3677
     1           +ALOGT*(-0.353728+ALOGT*0.0153976)))
      DG(8,1)=EXP(-17.3894 + ALOGT*(4.51602
     1           +ALOGT*(-0.339571+ALOGT*0.0133246)))
      DG(8,2)=EXP(-16.1086 + ALOGT*(4.15191
     1           +ALOGT*(-0.335419+ALOGT*0.0150109)))
      DG(8,3)=EXP(-18.0079 + ALOGT*(4.47152
     1           +ALOGT*(-0.367359+ALOGT*0.0159894)))
      DG(8,4)=EXP(-18.7746 + ALOGT*(4.57308
     1           +ALOGT*(-0.376416+ALOGT*0.0162149)))
      DG(8,5)=EXP(-19.9914 + ALOGT*(4.69023
     1           +ALOGT*(-0.337503+ALOGT*0.0124032)))
      DG(8,6)=EXP(-20.7622 + ALOGT*(5.10621
     1           +ALOGT*(-0.426491+ALOGT*0.0176095)))
      DG(8,7)=EXP(-19.3853 + ALOGT*(4.79778
     1           +ALOGT*(-0.398432+ALOGT*0.0168686)))
      DG(8,8)=EXP(-20.8859 + ALOGT*(5.13427
     1           +ALOGT*(-0.430394+ALOGT*0.0177929)))
      DG(9,1)=EXP(-17.1872 + ALOGT*(4.60996
     1           +ALOGT*(-0.360588+ALOGT*0.0146263)))
      DG(9,2)=EXP(-15.2134 + ALOGT*(3.88532
     1           +ALOGT*(-0.298924+ALOGT*0.0133485)))
      DG(9,3)=EXP(-17.4129 + ALOGT*(4.34682
     1           +ALOGT*(-0.351535+ALOGT*0.0153205)))
      DG(9,4)=EXP(-18.3974 + ALOGT*(4.52892
     1           +ALOGT*(-0.371222+ALOGT*0.0160123)))
      DG(9,5)=EXP(-20.1625 + ALOGT*(4.89987
     1           +ALOGT*(-0.368962+ALOGT*0.0139311)))
      DG(9,6)=EXP(-20.4108 + ALOGT*(5.06965
     1           +ALOGT*(-0.422158+ALOGT*0.0174363)))
      DG(9,7)=EXP(-19.065 + ALOGT*(4.77759
     1           +ALOGT*(-0.397547+ALOGT*0.0169053)))
      DG(9,8)=EXP(-20.5739 + ALOGT*(5.10676
     1           +ALOGT*(-0.427372+ALOGT*0.0176807)))
      DG(9,9)=EXP(-20.3335 + ALOGT*(5.12453
     1           +ALOGT*(-0.43143+ALOGT*0.0179293)))
      DG(10,1)=EXP(-15.1063 + ALOGT*(3.40906
     1           +ALOGT*(-0.165909+ALOGT*0.00458546)))
      DG(10,2)=EXP(-17.6907 + ALOGT*(4.73969
     1           +ALOGT*(-0.405373+ALOGT*0.0177634)))
      DG(10,3)=EXP(-19.0024 + ALOGT*(4.75701
     1           +ALOGT*(-0.390789+ALOGT*0.0164302)))
      DG(10,4)=EXP(-20.0981 + ALOGT*(4.97764
     1           +ALOGT*(-0.416532+ALOGT*0.0174464)))
      DG(10,5)=EXP(-18.3639 + ALOGT*(3.82497
     1           +ALOGT*(-0.199293+ALOGT*0.00547861)))
      DG(10,6)=EXP(-21.4052 + ALOGT*(5.18187
     1           +ALOGT*(-0.417965+ALOGT*0.0165309)))
      DG(10,7)=EXP(-20.0456 + ALOGT*(4.90985
     1           +ALOGT*(-0.395229+ALOGT*0.0160105)))
      DG(10,8)=EXP(-21.366 + ALOGT*(5.13937
     1           +ALOGT*(-0.411254+ALOGT*0.0161916)))
      DG(10,9)=EXP(-20.9877 + ALOGT*(5.08911
     1           +ALOGT*(-0.404951+ALOGT*0.0159221)))
      DG(10,10)=EXP(-20.4789 + ALOGT*(4.54905
     1           +ALOGT*(-0.308371+ALOGT*0.0107267)))
      DG(11,1)=EXP(-16.9521 + ALOGT*(4.85636
     1           +ALOGT*(-0.415536+ALOGT*0.0179947)))
      DG(11,2)=EXP(-13.0937 + ALOGT*(3.19206
     1           +ALOGT*(-0.208051+ALOGT*0.00937728)))
      DG(11,3)=EXP(-15.7571 + ALOGT*(3.85904
     1           +ALOGT*(-0.292067+ALOGT*0.0129045)))
      DG(11,4)=EXP(-16.8714 + ALOGT*(4.07587
     1           +ALOGT*(-0.317554+ALOGT*0.0138971)))
      DG(11,5)=EXP(-19.9385 + ALOGT*(5.09866
     1           +ALOGT*(-0.412985+ALOGT*0.0165358)))
      DG(11,6)=EXP(-19.3468 + ALOGT*(4.84412
     1           +ALOGT*(-0.405065+ALOGT*0.0171856)))
      DG(11,7)=EXP(-17.8251 + ALOGT*(4.47255
     1           +ALOGT*(-0.366822+ALOGT*0.0159418)))
      DG(11,8)=EXP(-19.4151 + ALOGT*(4.83742
     1           +ALOGT*(-0.403966+ALOGT*0.0171303)))
      DG(11,9)=EXP(-19.0566 + ALOGT*(4.81974
     1           +ALOGT*(-0.402837+ALOGT*0.0171294)))
      DG(11,10)=EXP(-20.4238 + ALOGT*(5.09119
     1           +ALOGT*(-0.421091+ALOGT*0.0172354)))
      DG(11,11)=EXP(-17.5305 + ALOGT*(4.40588
     1           +ALOGT*(-0.357139+ALOGT*0.0154805)))
      DG(12,1)=EXP(-15.1876 + ALOGT*(3.35897
     1           +ALOGT*(-0.158122+ALOGT*0.00415385)))
      DG(12,2)=EXP(-17.7644 + ALOGT*(4.7224
     1           +ALOGT*(-0.406574+ALOGT*0.0179621)))
      DG(12,3)=EXP(-19.3044 + ALOGT*(4.8235
     1           +ALOGT*(-0.40355+ALOGT*0.0171471)))
      DG(12,4)=EXP(-19.8914 + ALOGT*(4.83845
     1           +ALOGT*(-0.401727+ALOGT*0.0169299)))
      DG(12,5)=EXP(-19.0712 + ALOGT*(4.0884
     1           +ALOGT*(-0.242506+ALOGT*0.00771067)))
      DG(12,6)=EXP(-21.4587 + ALOGT*(5.17088
     1           +ALOGT*(-0.423095+ALOGT*0.0170041)))
      DG(12,7)=EXP(-19.8824 + ALOGT*(4.80176
     1           +ALOGT*(-0.384622+ALOGT*0.0156762)))
      DG(12,8)=EXP(-21.3069 + ALOGT*(5.0878
     1           +ALOGT*(-0.410251+ALOGT*0.0163617)))
      DG(12,9)=EXP(-20.8649 + ALOGT*(5.0007
     1           +ALOGT*(-0.398465+ALOGT*0.0158283)))
      DG(12,10)=EXP(-21.2156 + ALOGT*(4.83564
     1           +ALOGT*(-0.355523+ALOGT*0.0131523)))
      DG(12,11)=EXP(-20.1951 + ALOGT*(4.92915
     1           +ALOGT*(-0.402979+ALOGT*0.0165546)))
      DG(12,12)=EXP(-21.7684 + ALOGT*(5.04792
     1           +ALOGT*(-0.392399+ALOGT*0.0151034)))
      DG(13,1)=EXP(-13.2716 + ALOGT*(2.39353
     1           +ALOGT*(-0.0105983+ALOGT*-0.00313506)))
      DG(13,2)=EXP(-19.5154 + ALOGT*(5.34662
     1           +ALOGT*(-0.484957+ALOGT*0.0212441)))
      DG(13,3)=EXP(-19.2062 + ALOGT*(4.63779
     1           +ALOGT*(-0.366487+ALOGT*0.0149915)))
      DG(13,4)=EXP(-20.506 + ALOGT*(4.93488
     1           +ALOGT*(-0.402946+ALOGT*0.0165157)))
      DG(13,5)=EXP(-16.2548 + ALOGT*(2.69037
     1           +ALOGT*(-0.0349941+ALOGT*-0.00215614)))
      DG(13,6)=EXP(-21.3438 + ALOGT*(4.94595
     1           +ALOGT*(-0.379231+ALOGT*0.0145301)))
      DG(13,7)=EXP(-19.8132 + ALOGT*(4.63266
     1           +ALOGT*(-0.3506+ALOGT*0.013714)))
      DG(13,8)=EXP(-21.1751 + ALOGT*(4.88411
     1           +ALOGT*(-0.371753+ALOGT*0.014227)))
      DG(13,9)=EXP(-20.4613 + ALOGT*(4.67563
     1           +ALOGT*(-0.342327+ALOGT*0.012848)))
      DG(13,10)=EXP(-20.4689 + ALOGT*(4.36775
     1           +ALOGT*(-0.280929+ALOGT*0.00940188)))
      DG(13,11)=EXP(-20.1068 + ALOGT*(4.72787
     1           +ALOGT*(-0.36345+ALOGT*0.014295)))
      DG(13,12)=EXP(-21.4421 + ALOGT*(4.75637
     1           +ALOGT*(-0.342161+ALOGT*0.0124547)))
      DG(13,13)=EXP(-19.5019 + ALOGT*(3.74426
     1           +ALOGT*(-0.188554+ALOGT*0.00496301)))
      DG(14,1)=EXP(-11.9422 + ALOGT*(1.69741
     1           +ALOGT*(0.0952233+ALOGT*-0.00838361)))
      DG(14,2)=EXP(-20.2665 + ALOGT*(5.58923
     1           +ALOGT*(-0.517245+ALOGT*0.022673)))
      DG(14,3)=EXP(-19.2759 + ALOGT*(4.56836
     1           +ALOGT*(-0.35299+ALOGT*0.0142079)))
      DG(14,4)=EXP(-20.3987 + ALOGT*(4.80203
     1           +ALOGT*(-0.382074+ALOGT*0.0154516)))
      DG(14,5)=EXP(-16.593 + ALOGT*(2.79532
     1           +ALOGT*(-0.0538067+ALOGT*-0.00110303)))
      DG(14,6)=EXP(-21.1507 + ALOGT*(4.7842
     1           +ALOGT*(-0.35558+ALOGT*0.0133977)))
      DG(14,7)=EXP(-19.5633 + ALOGT*(4.44303
     1           +ALOGT*(-0.321447+ALOGT*0.0122567)))
      DG(14,8)=EXP(-20.9624 + ALOGT*(4.717
     1           +ALOGT*(-0.346959+ALOGT*0.0130276)))
      DG(14,9)=EXP(-20.0735 + ALOGT*(4.42492
     1           +ALOGT*(-0.305193+ALOGT*0.0110513)))
      DG(14,10)=EXP(-20.4438 + ALOGT*(4.28326
     1           +ALOGT*(-0.268961+ALOGT*0.00885829)))
      DG(14,11)=EXP(-19.6486 + ALOGT*(4.42494
     1           +ALOGT*(-0.317253+ALOGT*0.0119985)))
      DG(14,12)=EXP(-21.4332 + ALOGT*(4.67883
     1           +ALOGT*(-0.330789+ALOGT*0.0119146)))
      DG(14,13)=EXP(-19.7559 + ALOGT*(3.78843
     1           +ALOGT*(-0.195327+ALOGT*0.00529695)))
      DG(14,14)=EXP(-20.0509 + ALOGT*(3.84844
     1           +ALOGT*(-0.203991+ALOGT*0.00570241)))
      DG(15,1)=EXP(-10.4071 + ALOGT*(0.910928
     1           +ALOGT*(0.215703+ALOGT*-0.0144218)))
      DG(15,2)=EXP(-20.6798 + ALOGT*(5.70894
     1           +ALOGT*(-0.533884+ALOGT*0.0234347)))
      DG(15,3)=EXP(-19.6322 + ALOGT*(4.63398
     1           +ALOGT*(-0.359003+ALOGT*0.0143476)))
      DG(15,4)=EXP(-20.0507 + ALOGT*(4.59427
     1           +ALOGT*(-0.352858+ALOGT*0.0140803)))
      DG(15,5)=EXP(-18.2101 + ALOGT*(3.47419
     1           +ALOGT*(-0.154162+ALOGT*0.0037198)))
      DG(15,6)=EXP(-20.7836 + ALOGT*(4.57519
     1           +ALOGT*(-0.327154+ALOGT*0.0121037)))
      DG(15,7)=EXP(-19.1557 + ALOGT*(4.19681
     1           +ALOGT*(-0.284544+ALOGT*0.010436)))
      DG(15,8)=EXP(-20.7573 + ALOGT*(4.57004
     1           +ALOGT*(-0.326067+ALOGT*0.012048)))
      DG(15,9)=EXP(-19.7121 + ALOGT*(4.20294
     1           +ALOGT*(-0.27318+ALOGT*0.00953242)))
      DG(15,10)=EXP(-20.4845 + ALOGT*(4.2442
     1           +ALOGT*(-0.264369+ALOGT*0.00869112)))
      DG(15,11)=EXP(-19.1036 + ALOGT*(4.10152
     1           +ALOGT*(-0.268938+ALOGT*0.00963106)))
      DG(15,12)=EXP(-21.4825 + ALOGT*(4.64358
     1           +ALOGT*(-0.326433+ALOGT*0.0117393)))
      DG(15,13)=EXP(-21.0312 + ALOGT*(4.29804
     1           +ALOGT*(-0.269322+ALOGT*0.00879745)))
      DG(15,14)=EXP(-21.2006 + ALOGT*(4.29835
     1           +ALOGT*(-0.268867+ALOGT*0.00875223)))
      DG(15,15)=EXP(-21.4589 + ALOGT*(4.35433
     1           +ALOGT*(-0.277373+ALOGT*0.00916609)))
      DG(16,1)=EXP(-5.90178 + ALOGT*(-1.24593
     1           +ALOGT*(0.533632+ALOGT*-0.0296688)))
      DG(16,2)=EXP(-21.1458 + ALOGT*(5.68469
     1           +ALOGT*(-0.51412+ALOGT*0.0218462)))
      DG(16,3)=EXP(-17.8941 + ALOGT*(3.63345
     1           +ALOGT*(-0.199032+ALOGT*0.006132)))
      DG(16,4)=EXP(-19.0136 + ALOGT*(3.91075
     1           +ALOGT*(-0.241027+ALOGT*0.008271)))
      DG(16,5)=EXP(-15.5949 + ALOGT*(2.19415
     1           +ALOGT*(0.0285285+ALOGT*-0.00472077)))
      DG(16,6)=EXP(-20.0211 + ALOGT*(4.03835
     1           +ALOGT*(-0.242139+ALOGT*0.00787004)))
      DG(16,7)=EXP(-18.1275 + ALOGT*(3.55325
     1           +ALOGT*(-0.182239+ALOGT*0.00526501)))
      DG(16,8)=EXP(-19.9887 + ALOGT*(4.04516
     1           +ALOGT*(-0.243053+ALOGT*0.00791146)))
      DG(16,9)=EXP(-18.6218 + ALOGT*(3.53424
     1           +ALOGT*(-0.169856+ALOGT*0.00446125)))
      DG(16,10)=EXP(-19.4897 + ALOGT*(3.63819
     1           +ALOGT*(-0.174014+ALOGT*0.00441943)))
      DG(16,11)=EXP(-16.5457 + ALOGT*(2.74218
     1           +ALOGT*(-0.0605418+ALOGT*-0.000700205)))
      DG(16,12)=EXP(-20.647 + ALOGT*(4.09066
     1           +ALOGT*(-0.241085+ALOGT*0.00757206)))
      DG(16,13)=EXP(-19.5065 + ALOGT*(3.45496
     1           +ALOGT*(-0.144422+ALOGT*0.00284408)))
      DG(16,14)=EXP(-19.6898 + ALOGT*(3.45788
     1           +ALOGT*(-0.143834+ALOGT*0.00276094)))
      DG(16,15)=EXP(-20.2461 + ALOGT*(3.63179
     1           +ALOGT*(-0.16757+ALOGT*0.00381138)))
      DG(16,16)=EXP(-17.9993 + ALOGT*(2.46208
     1           +ALOGT*(0.00530922+ALOGT*-0.004512)))
      DG(17,1)=EXP(-7.72753 + ALOGT*(-0.342413
     1           +ALOGT*(0.399609+ALOGT*-0.0231933)))
      DG(17,2)=EXP(-20.3031 + ALOGT*(5.42285
     1           +ALOGT*(-0.481938+ALOGT*0.0205386)))
      DG(17,3)=EXP(-18.34 + ALOGT*(3.94063
     1           +ALOGT*(-0.249077+ALOGT*0.00874573)))
      DG(17,4)=EXP(-19.4907 + ALOGT*(4.21155
     1           +ALOGT*(-0.28771+ALOGT*0.0106224)))
      DG(17,5)=EXP(-16.0996 + ALOGT*(2.48073
     1           +ALOGT*(-0.0115551+ALOGT*-0.00290534)))
      DG(17,6)=EXP(-20.2125 + ALOGT*(4.19554
     1           +ALOGT*(-0.26525+ALOGT*0.00896004)))
      DG(17,7)=EXP(-18.5569 + ALOGT*(3.82008
     1           +ALOGT*(-0.2225+ALOGT*0.00723328)))
      DG(17,8)=EXP(-20.1333 + ALOGT*(4.17902
     1           +ALOGT*(-0.262969+ALOGT*0.00885706)))
      DG(17,9)=EXP(-19.2854 + ALOGT*(3.90217
     1           +ALOGT*(-0.223924+ALOGT*0.00704745)))
      DG(17,10)=EXP(-19.5252 + ALOGT*(3.71962
     1           +ALOGT*(-0.18509+ALOGT*0.0048802)))
      DG(17,11)=EXP(-17.7927 + ALOGT*(3.39093
     1           +ALOGT*(-0.15779+ALOGT*0.0040622)))
      DG(17,12)=EXP(-20.7151 + ALOGT*(4.18858
     1           +ALOGT*(-0.255385+ALOGT*0.00823298)))
      DG(17,13)=EXP(-19.4648 + ALOGT*(3.50444
     1           +ALOGT*(-0.151863+ALOGT*0.00319193)))
      DG(17,14)=EXP(-19.6706 + ALOGT*(3.51646
     1           +ALOGT*(-0.152735+ALOGT*0.00318704)))
      DG(17,15)=EXP(-20.3124 + ALOGT*(3.72725
     1           +ALOGT*(-0.181819+ALOGT*0.00449214)))
      DG(17,16)=EXP(-18.1991 + ALOGT*(2.61677
     1           +ALOGT*(-0.0177887+ALOGT*-0.00338826)))
      DG(17,17)=EXP(-18.4166 + ALOGT*(2.77989
     1           +ALOGT*(-0.0419566+ALOGT*-0.0022212)))
C
      DO I=1,17
        DO J=I+1,17
          DG(I, J) = DG(J, I)
        ENDDO
      ENDDO
C
      DO I=1,17
        QG(I) = 0.D0
        DO J=1,17
          QG(I) = QG(I) + XG(J)/DG(I,J)
        ENDDO
      ENDDO
C
      D(  1) = (1.D0-Y(1))/(QG(1)-X(1)/DG(1,1))/PA
      D(  2) = (1.D0-Y(2))/(QG(2)-X(2)/DG(2,2))/PA
      D(  3) = (1.D0-Y(3))/(QG(3)-X(3)/DG(3,3))/PA
      D(  4) = (1.D0-Y(4))/(QG(4)-X(4)/DG(4,4))/PA
      D(  5) = (1.D0-Y(5))/(QG(3)-X(5)/DG(3,3))/PA
      D(  6) = (1.D0-Y(6))/(QG(5)-X(6)/DG(5,5))/PA
      D(  7) = (1.D0-Y(7))/(QG(4)-X(7)/DG(4,4))/PA
      D(  8) = (1.D0-Y(8))/(QG(6)-X(8)/DG(6,6))/PA
      D(  9) = (1.D0-Y(9))/(QG(6)-X(9)/DG(6,6))/PA
      D( 10) = (1.D0-Y(10))/(QG(7)-X(10)/DG(7,7))/PA
      D( 11) = (1.D0-Y(11))/(QG(7)-X(11)/DG(7,7))/PA
      D( 12) = (1.D0-Y(12))/(QG(4)-X(12)/DG(4,4))/PA
      D( 13) = (1.D0-Y(13))/(QG(4)-X(13)/DG(4,4))/PA
      D( 14) = (1.D0-Y(14))/(QG(6)-X(14)/DG(6,6))/PA
      D( 15) = (1.D0-Y(15))/(QG(6)-X(15)/DG(6,6))/PA
      D( 16) = (1.D0-Y(16))/(QG(8)-X(16)/DG(8,8))/PA
      D( 17) = (1.D0-Y(17))/(QG(9)-X(17)/DG(9,9))/PA
      D( 18) = (1.D0-Y(18))/(QG(8)-X(18)/DG(8,8))/PA
      D( 19) = (1.D0-Y(19))/(QG(9)-X(19)/DG(9,9))/PA
      D( 20) = (1.D0-Y(20))/(QG(9)-X(20)/DG(9,9))/PA
      D( 21) = (1.D0-Y(21))/(QG(9)-X(21)/DG(9,9))/PA
      D( 22) = (1.D0-Y(22))/(QG(6)-X(22)/DG(6,6))/PA
      D( 23) = (1.D0-Y(23))/(QG(6)-X(23)/DG(6,6))/PA
      D( 24) = (1.D0-Y(24))/(QG(10)-X(24)/DG(10,10))/PA
      D( 25) = (1.D0-Y(25))/(QG(11)-X(25)/DG(11,11))/PA
      D( 26) = (1.D0-Y(26))/(QG(10)-X(26)/DG(10,10))/PA
      D( 27) = (1.D0-Y(27))/(QG(10)-X(27)/DG(10,10))/PA
      D( 28) = (1.D0-Y(28))/(QG(10)-X(28)/DG(10,10))/PA
      D( 29) = (1.D0-Y(29))/(QG(10)-X(29)/DG(10,10))/PA
      D( 30) = (1.D0-Y(30))/(QG(10)-X(30)/DG(10,10))/PA
      D( 31) = (1.D0-Y(31))/(QG(10)-X(31)/DG(10,10))/PA
      D( 32) = (1.D0-Y(32))/(QG(12)-X(32)/DG(12,12))/PA
      D( 33) = (1.D0-Y(33))/(QG(12)-X(33)/DG(12,12))/PA
      D( 34) = (1.D0-Y(34))/(QG(12)-X(34)/DG(12,12))/PA
      D( 35) = (1.D0-Y(35))/(QG(12)-X(35)/DG(12,12))/PA
      D( 36) = (1.D0-Y(36))/(QG(12)-X(36)/DG(12,12))/PA
      D( 37) = (1.D0-Y(37))/(QG(12)-X(37)/DG(12,12))/PA
      D( 38) = (1.D0-Y(38))/(QG(13)-X(38)/DG(13,13))/PA
      D( 39) = (1.D0-Y(39))/(QG(13)-X(39)/DG(13,13))/PA
      D( 40) = (1.D0-Y(40))/(QG(13)-X(40)/DG(13,13))/PA
      D( 41) = (1.D0-Y(41))/(QG(13)-X(41)/DG(13,13))/PA
      D( 42) = (1.D0-Y(42))/(QG(13)-X(42)/DG(13,13))/PA
      D( 43) = (1.D0-Y(43))/(QG(13)-X(43)/DG(13,13))/PA
      D( 44) = (1.D0-Y(44))/(QG(12)-X(44)/DG(12,12))/PA
      D( 45) = (1.D0-Y(45))/(QG(10)-X(45)/DG(10,10))/PA
      D( 46) = (1.D0-Y(46))/(QG(10)-X(46)/DG(10,10))/PA
      D( 47) = (1.D0-Y(47))/(QG(10)-X(47)/DG(10,10))/PA
      D( 48) = (1.D0-Y(48))/(QG(10)-X(48)/DG(10,10))/PA
      D( 49) = (1.D0-Y(49))/(QG(13)-X(49)/DG(13,13))/PA
      D( 50) = (1.D0-Y(50))/(QG(12)-X(50)/DG(12,12))/PA
      D( 51) = (1.D0-Y(51))/(QG(13)-X(51)/DG(13,13))/PA
      D( 52) = (1.D0-Y(52))/(QG(10)-X(52)/DG(10,10))/PA
      D( 53) = (1.D0-Y(53))/(QG(10)-X(53)/DG(10,10))/PA
      D( 54) = (1.D0-Y(54))/(QG(6)-X(54)/DG(6,6))/PA
      D( 55) = (1.D0-Y(55))/(QG(7)-X(55)/DG(7,7))/PA
      D( 56) = (1.D0-Y(56))/(QG(13)-X(56)/DG(13,13))/PA
      D( 57) = (1.D0-Y(57))/(QG(13)-X(57)/DG(13,13))/PA
      D( 58) = (1.D0-Y(58))/(QG(13)-X(58)/DG(13,13))/PA
      D( 59) = (1.D0-Y(59))/(QG(13)-X(59)/DG(13,13))/PA
      D( 60) = (1.D0-Y(60))/(QG(13)-X(60)/DG(13,13))/PA
      D( 61) = (1.D0-Y(61))/(QG(13)-X(61)/DG(13,13))/PA
      D( 62) = (1.D0-Y(62))/(QG(10)-X(62)/DG(10,10))/PA
      D( 63) = (1.D0-Y(63))/(QG(14)-X(63)/DG(14,14))/PA
      D( 64) = (1.D0-Y(64))/(QG(14)-X(64)/DG(14,14))/PA
      D( 65) = (1.D0-Y(65))/(QG(14)-X(65)/DG(14,14))/PA
      D( 66) = (1.D0-Y(66))/(QG(13)-X(66)/DG(13,13))/PA
      D( 67) = (1.D0-Y(67))/(QG(13)-X(67)/DG(13,13))/PA
      D( 68) = (1.D0-Y(68))/(QG(14)-X(68)/DG(14,14))/PA
      D( 69) = (1.D0-Y(69))/(QG(15)-X(69)/DG(15,15))/PA
      D( 70) = (1.D0-Y(70))/(QG(15)-X(70)/DG(15,15))/PA
      D( 71) = (1.D0-Y(71))/(QG(15)-X(71)/DG(15,15))/PA
      D( 72) = (1.D0-Y(72))/(QG(15)-X(72)/DG(15,15))/PA
      D( 73) = (1.D0-Y(73))/(QG(15)-X(73)/DG(15,15))/PA
      D( 74) = (1.D0-Y(74))/(QG(16)-X(74)/DG(16,16))/PA
      D( 75) = (1.D0-Y(75))/(QG(15)-X(75)/DG(15,15))/PA
      D( 76) = (1.D0-Y(76))/(QG(17)-X(76)/DG(17,17))/PA
      D( 77) = (1.D0-Y(77))/(QG(4)-X(77)/DG(4,4))/PA
C
      RETURN
      END
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCEDIF(T, NO, KK, X, COFD, WT, SMALL, XX, DJK, D)
C
C  START PROLOGUE
C
C  SUBROUTINE MCEDIF(T, NO, KK, X, COFD, WT, SMALL, XX, DJK, D)
C  This subroutine is used internally to compute the mixture
C  diffusion coefficients; normally not called by the package user.
C
C  INPUT
C  T          - Real scalar, temperature.
C                  cgs units, K
C  NO         - Integer scalar, order of fit.
C  KK         - Integer scalar, total species count.
C  X(*)       - Real array, mole fractions of the mixture;
C               dimension at least KK, the total species count.
C  COFD(*,*,*)- Real three-dimensional array, coefficients of the
C               fits for binary diffusion coefficients;
C               dimension at least NO for the first dimension,
C               the fit order, and at least KK, the total species
C               count, for both the second and last dimensions.
C  WT(*)      - Real array, species molecular weights;
C               dimension at least KK, the total species count.
C  SMALL      - Real scalar, a small number added to all mole fractions
C               before computing the mixture diffusion coefficients;
C               this process avoids an undefined situation when a pure
C               species condition is approached.
C  XX(*)      - Real array, mole fractions plus SMALL to avoid the
C               problem of a pure species;
C               dimension at least KK, the total species count.
C  RMCWRK(*)  - Real workspace array; dimension at LENRMC.
C
C  OUTPUT
C  D(*)       - Real array, mixture diffusion coefficients.
C                  cgs units, cm**2/s.
C  DJK(*,*)   - Real matrix, binary diffusion coefficients;
C               dimension at least KK, the total species count, for
C               both dimensions.
C                  cgs units, cm**2/s
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
      PARAMETER (ZERO = 0.0D0)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C      PARAMETER (ZERO = 0.0E0)
C*****END precision > single
C
      DIMENSION X(KK), COFD(NO,KK,KK), WT(KK), XX(KK),
     $          DJK(KK,KK), D(KK)
C
      ALOGT = LOG(T)
C
C Special Case for K = 1 - return the self-diffusion coefficient
C
      IF (KK .EQ. 1) THEN
        CALL MCEVAL (ALOGT, 1, NO, COFD(1,1,1), DJK(1,1))
        D(1) = EXP(DJK(1,1))
        RETURN
      ENDIF
C
C Use the fact that DJK is symmetric to cut the work down by 1/2
C  - also we don't need the self-diffusion coefficient evaluated
C
      IF (NO .EQ. 4) THEN
        DO 90 K = 2, KK
#ifdef VECTORVERSION
C  Added by Mark Fahey, ORNL.
C  Manually inline for Cray. 
           DO J = 1, K-1
             DJK(J,K) = (((COFD(4,J,K) * ALOGT) + COFD(3,J,K)) * ALOGT +
     $                        COFD(2,J,K)) * ALOGT + COFD(1,J,K)
           ENDDO
#else
         CALL MCEVAL4 (ALOGT, K-1, COFD(1,1,K), DJK(1,K) )
#endif
 90     CONTINUE
      ELSE
        DO 100 K = 2, KK
#ifdef VECTORVERSION
C  Added by Mark Fahey, ORNL.
C  Manually inline for Cray. 
          DO J = 1, K-1
            DJK(J,K) = COFD(NO,J,K)
          ENDDO
          DO I = 1, NO-1
            DO J = 1, K-1
              DJK(J,K) = DJK(J,K) * ALOGT + COFD(NO-I,J,K)
            ENDDO
          ENDDO
#else
          CALL MCEVAL (ALOGT, K-1, NO, COFD(1,1,K), DJK(1,K) )
#endif
100     CONTINUE
      ENDIF
C
C Fill in the entire DJK, only after the exponential !
C - actually, evaluate and store the inverse of the
C   binary diffusion coefficient - this is what's needed.
C
#ifdef VECTORVERSION
C Added by Mark Fahey, ORNL.
C If DJK is symmetric, can we do something more clever
      DO K = 1, KK
         DO J = 1, KK
            DJK(J,K) = EXP(-DJK(J,K))
         ENDDO
      ENDDO
      DO K = 1, KK
         DJK(K,K) = ZERO
      ENDDO
#else
      DO 150 K = 1, KK
         DO 140 J = 1, K-1
            DJK(J,K) = EXP(-DJK(J,K))
            DJK(K,J) = DJK(J,K)
  140    CONTINUE
         DJK(K,K) = ZERO
  150 CONTINUE
#endif
C
      WTM = ZERO
      DO 175 K = 1, KK
         WTM = WTM + WT(K)*X(K)
         XX(K) = MAX (X(K), SMALL)
  175 CONTINUE
C
      DO 300 K = 1, KK
C
         SUMXW  = - XX(K) * WT(K)
         SUMXOD = ZERO
C
         DO 200 J = 1, KK
             SUMXW  = SUMXW  + XX(J)*WT(J)
             SUMXOD = SUMXOD + XX(J)*DJK(J,K)
  200    CONTINUE
C
         D(K) = SUMXW/(WTM*SUMXOD)
  300 CONTINUE
C
C     end of SUBROUTINE MCEDIF
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCMDIF (P, T, X, KDIM, IMCWRK, RMCWRK, D)
C
C  START PROLOGUE
C
C  SUBROUTINE MCMDIF (P, T, X, KDIM, IMCWRK, RMCWRK, D)
C  Returns the ordinary multicomponent diffusion coefficients,
C  given pressure, temperature, and mole fractions.
C
C  INPUT
C  P         - Real scalar, pressure.
C                 cgs units, dynes/cm**2
C  T         - Real scalar, temperature.
C                 cgs units, K
C  X(*)      - Real array, mole fractions of the mixture;
C              dimension at least KK, the total species count.
C  KDIM      - Integer scalar, actual first dimension of D(KDIM,KK);
C              KDIM must be at least KK, the total species count.
C  IMCWRK(*) - Integer workspace array; dimension at least LENIMC.
C  RMCWRK(*) - Real    workspace array; dimension at least LENRMC.
C
C  OUTPUT
C  D(*,*)    - Real matrix, ordinary multicomponent diffusion
C              coefficients;
C              dimension at least KK, the total species count, for
C              both the first and second dimensions.
C                 cgs units, cm**2/s
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,
     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST,
     4                NCST, NXL, NR, NWRK, K3
      DIMENSION X(*), IMCWRK(*), RMCWRK(*), D(KDIM,*)
      LOGICAL   KERR
C
      CALL MCORDF (P, T, X, NKK, KDIM, SMALL, RMCWRK(NWT), RMCWRK,
     1             RMCWRK(NXX), RMCWRK(NBIND), RMCWRK(NXL),
     2             RMCWRK(NWRK), IMCWRK(IPVT), D, KERR)
C
C     end of SUBROUTINE MCMDIF
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCMCDT (P, T, X, IMCWRK, RMCWRK, ICKWRK, CKWRK,
     1                   DT, COND)
C
C  START PROLOGUE
C
C  SUBROUTINE MCMCDT (P, T, X, IMCWRK, RMCWRK, ICKWRK, CKWRK,
C                     DT, COND)
C  Returns thermal diffusion coefficients, and mixture thermal
C  conductivities, given pressure, temperature, and mole fraction.
C
C  INPUT
C  P         - Real scalar, pressure.
C                 cgs units, dynes/cm**2
C  T         - Real scalar, temperature.
C                 cgs units, K
C  X(*)      - Real array, mole fractions of the mixture;
C              dimension at least KK, the total species count.
C
C  IMCWRK(*) - Integer TRANSPORT workspace array;
C              dimension at least LENIMC.
C  RMCWRK(*) - Real    TRANSPORT workspace array;
C              dimension at least LENRMC.
C  ICKWRK(*) - Integer CHEMKIN workspace array;
C              dimension at least LENICK.
C  RCKWRK(*) - Real    CHEMKIN workspace array;
C              dimension at least LENRCK.
C
C  OUTPUT
C  DT(*)     - Real array, thermal multicomponent diffusion
C              coefficients;
C              dimension at least KK, the total species count.
C                 cgs units, gm/(cm*sec)
C                  CGS UNITS - GM/(CM*SEC)
C  COND      - Real scalar, mixture thermal conductivity.
C                 cgs units, erg/(cm*K*s)
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,
     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST,
     4                NCST, NXL, NR, NWRK, K3
      DIMENSION X(*), IMCWRK(*), RMCWRK(*), ICKWRK(*), CKWRK(*), DT(*)
      LOGICAL   KERR
C
      CALL MCLMDT (P, T, X, NKK, K3, SMALL, RMCWRK(NWT), RMCWRK(NEOK),
     1             RMCWRK(NZROT), IMCWRK(INLIN), RMCWRK(NEPS),
     2             ICKWRK, CKWRK, RMCWRK, RMCWRK(NXX), RMCWRK(NVIS),
     3             RMCWRK(NAST), RMCWRK(NBST), RMCWRK(NCST),
     4             RMCWRK(NXI),  RMCWRK(NCP), RMCWRK(NCROT),
     5             RMCWRK(NCINT), RMCWRK(NXL), RMCWRK(NR),
     6             RMCWRK(NBIND), IMCWRK(IPVT), DT, COND, KERR)
C
C     end of SUBROUTINE MCMCDT
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCEVAL (TF, KK, NO, COF, VAL)
C
C  START PROLOGUE
C
C  SUBROUTINE MCEVAL (TF, KK, NO, COF, VAL)
C  This subroutine uses Horner's algorithm to evaluate a polynomial
C  fit.  This routine is not normally called by the package user.
C
C  INPUT
C  TF        - Real scalar, independent variable of fit;
C              either temperature or log(temperature).
C  KK        - Integer scalar, total species count.
C  NO        - Integer scalar, order of fit.
C  COF(*,*)  - Real matrix, fit coefficients;
C              dimension exactly NO for the first dimension and at
C              least KK for the second.
C              COF(N,K) is the Nth coefficient of a property fit for
C              species K.
C
C  OUTPUT
C  VAL(*)    - Real array, evaluations of the fit at TF;
C              dimension at least KK, the total species count.
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION COF(NO,KK), VAL(KK)
C
      NOM1 = NO-1
C
      DO 10 K = 1, KK
        VAL(K) = COF(NO,K)
   10 CONTINUE
      DO 200 I = 1, NOM1
        DO 150 K = 1, KK
          VAL(K) = VAL(K) * TF + COF(NO-I,K)
  150   CONTINUE
200   CONTINUE
C
C     end of SUBROUTINE MCEVAL
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCEVAL4 (TF, KK, COF, VAL)
C
C  START PROLOGUE
C
C  SUBROUTINE MCEVAL4 (TF, KK, COF, VAL)
C  This subroutine uses Horner's algorithm to evaluate a polynomial;
C  the polynomial fit is hard-coded for order four polynomials, NO=4.
C  This routine is not normally called by the package user.
C
C  INPUT
C  TF       - Real scalar, independent variable of fit;
C             either temperature or log(temperature).
C  KK       - Integer scalar, total species count.
C  NO       - Integer scalar, order of fit.
C  COF(*,*) - Real matrix, fit coefficients;
C             dimension exactly NO for the first dimension, and at
C             least KK, the total species count, for the second;
C             COF(N,K) is the Nth coefficient of a property fit for
C             species K.
C
C  OUTPUT
C  VAL(*)   - Real array, evaluations of the fit at TF;
C             dimension at least KK, the total species count.
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
      PARAMETER (NO=4)
      DIMENSION COF(NO, KK), VAL(KK)
C
      DO 10 K = 1, KK
        VAL(K) = (((COF(4,K) * TF) + COF(3,K)) * TF + COF(2,K))
     $                                         * TF + COF(1,K)
10    CONTINUE
C
C     end of SUBROUTINE MCEVAL4
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCEPSG (KK, EPS, SIG, DIP, POL, EOK, SGM)
C
C  START PROLOGUE
C
C  SUBROUTINE MCEPSG (KK, EPS, SIG, DIP, POL, EOK, SGM)
C  This subroutine computes the reduced wel depth EOK(I,J) and
C  collision diameter SGM(I,J) for each I,J species pair.  The routine
C  is called only once, by the initialization subroutine MCINIT.
C  This routine is normally not called by the user.
C
C  INPUT
C  KK      - Integer scalar, total species count.
C  EPS(*)  - Real array, Lennard-Jones potential well depths;
C            dimension at least KK, the total species count.
C               cgs units, K
C  SIG(*)  - Real array, Lennardl-Jones collision diameters;
C            dimension at least KK, the total species count.
C               cgs units, Angstrom
C  DIP(*)  - Real array, dipole moments;
C            dimension at least KK, the total species count.
C               cgs units, Debye
C  POL(*)  - Real array, polarizabilities;
C            dimension at least KK, the total species count.
C               cgs units, Angstrom**3
C
C  OUTPUT
C  EOK(*,*)- Real matrix, reduced well depths for species pairs;
C            dimension at least KK, the total species count, for
C            both the first and second dimensions.
C               cgs units, K
C  SGM(*,*)- Real matrix, reduced collision diameters for species
C            species pairs;
C            dimension at least KK, the total species count, for
C            both the first and second dimensions.
C               cgs units, Angstrom
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
      PARAMETER (ONE=1.0D0, FDTCGS=1.0D-18, FATCM=1.0D8,
     1           DIPMIN=1.0D-20, BOLTZ=1.38056D-16)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C      PARAMETER (ONE=1.0, FDTCGS=1.0E-18, FATCM=1.0E8,
C     1           DIPMIN=1.0E-20, BOLTZ-1.38056E-16)
C*****END precision > single
C
      DIMENSION EPS(*), SIG(*), DIP(*), POL(*), EOK(KK,*), SGM(KK,*)
C
C     Compute and store EPS/K and SIGMA for all pairs
C
      DO 1000 J = 1, KK
C
         DO 999 K = 1, J
C
           IF(DIP(J).LT.DIPMIN .AND. DIP(K).GT.DIPMIN) THEN
C             K is polar, J is nonpolar
C
              XI = ONE + 0.25*(POL(J)/SIG(J)**3) *
     1                     (FDTCGS**2*FATCM**3/BOLTZ) *
     2                     (DIP(K)**2/(EPS(K)*SIG(K)**3)) *
     3                      SQRT(EPS(K)/EPS(J))
              SGM(K,J) = 0.5 * (SIG(J)+SIG(K)) * XI**(-ONE/6.0)
              EOK(K,J) = SQRT(EPS(J)*EPS(K)) * XI**2
C
          ELSE IF(DIP(J).GT.DIPMIN .AND. DIP(K).LT.DIPMIN) THEN
C             J is polar, K is nonpolar
C
              XI = ONE + 0.25*(POL(K)/SIG(K)**3) *
     1                     (FDTCGS**2*FATCM**3/BOLTZ) *
     2                     (DIP(J)**2/(EPS(J)*SIG(J)**3)) *
     3                      SQRT(EPS(J)/EPS(K))
              SGM(K,J) = 0.5 * (SIG(J)+SIG(K)) * XI**(-ONE / 6.0)
              EOK(K,J) = SQRT(EPS(J)*EPS(K)) * XI**2
C
          ELSE
C             normal case, either both polar or both nonpolar
C
              SGM(K,J) = 0.5 * (SIG(J) + SIG(K))
              EOK(K,J) = SQRT(EPS(J)*EPS(K))
C
          ENDIF
          SGM(J,K) = SGM(K,J)
          EOK(J,K) = EOK(K,J)
 999    CONTINUE
1000  CONTINUE
C
C     end of SUBROUTINE MCEPSG
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCORDF (P, T, X, KK, KDIM, SMALL, WT, RMCWRK, XX,
     1                   BINDIF, XL0000, WORK, IPVT, D, KERR)
C
C  START PROLOGUE
C
C  SUBROUTINE MCORDF (P, T, X, KK, KDIM, SMALL, WT, RMCWRK, XX,
C 1                   BINDIF, XL0000, WORK, IPVT, D, KERR)
C  This subroutine computes ordinary multicomponent diffusion coeffi-
C  cient matrix; it does so by computing the inverse of the L00,00
C  matrix.
C  This routine is not normally called directly by the user;
C  the user calls MCMDIF, which in turn calls MCORDF.
C
C  INPUT
C  P         - Real scalar, pressure.
C                 cgs units, dynes/cm**2
C  T         - Real scalar, temperature.
C                 cgs units, K
C  X(*)      - Real array, mole fractions of the mixture;
C              dimension at least KK, the total species count.
C  KK        - Integer scalar, total species count.
C  KDIM      - Integer scalar, actual first dimension of D(KDIM,KK);
C              KDIM must be at least KK, the total species count.
C  SMALL     - Real scalar;  the mole fractions used in the transport
C              computation are given by XX(K) = X(K) + SMALL.
C  WT(*    - Real array, species molecular weights;
C              dimension at least KK, the total species count.
C  RMCWRK(*) - Real workspace array; dimension at least LENRMC.
C  XX(*)     - Real array, mole fractions used in transport computa-
C              tion.
C              XX(K) = X(K) + SMALL
C  BINDIF(*,*)-Real matrix, binary diffusion coefficients;
C              dimension at least KK, the total species count,
C              for both the first and second dimensions.
C                 cgs units, cm**2/s
C  XL0000(*,*)-Real matrix L00,00;
C              dimension at least KK, the total species count,
C              for both the first and second dimensions.
C  WORK(*)   - Real workspace array for the inversion of the L00,00
C              matrix by LINPACK routines SGEFA and SGEDI;
C              dimension at least KK, the total species count.
C  IPVT(*)   - Integer array, pivot indices for inversion of the
C              L00,00 matrix by LINPACK routines SGEFA and SGEDI;
C              dimension at least KK, the total species count.
C
C  OUTPUT
C  D(*,*)    - Real matrix, ordinary multicomponent diffusion
C              coefficients;
C              dimension at least KK, the total species count, for
C              both the first and second dimensions.
C                 cgs units, cm**2/s
C  KERR      - Logical flag indicating whether an error was found
C              during matrix manipulations
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
      PARAMETER (ZERO=0.0D0, ONE=1.0D0)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C      PARAMETER (ZERO=0.0, ONE=1.0)
C*****END precision > single
C
      LOGICAL KERR
      CHARACTER*80 MSGSTR
      DIMENSION X(*), WT(*), BINDIF(KK,*), XL0000(KK,*), IPVT(*),
     1          WORK(*), XX(*), RMCWRK(*), D(KDIM,*)
C*****precision > double - linpack
C      DIMENSION DET(2)
C      SAVE      JOB
C      DATA      JOB /1/
C*****END precision > double - linpack
C*****precision > single - linpack
C      DIMENSION DET(2)
C      SAVE      JOB
C      DATA      JOB /1/
C*****END precision > single - linpack
C
C     Set minimum mole fraction to SMALL
C
      DO 50 I = 1, KK
         XX(I) = MAX( X(I) , SMALL)
   50 CONTINUE
C
C     Evaluate the binary diffusion coefficients
C
      CALL MCSDIF (P, T, KK, RMCWRK, BINDIF)
C
C     Assemble L00,00
C
      PFAC = 16.0 * T / (25.0 * P)
      DO 200 I = 1, KK
        SUM = -XX(I) / BINDIF(I,I)
        DO 90 J = 1, KK
          SUM = SUM + XX(J) / BINDIF(I,J)
  90    CONTINUE
        SUM = SUM / WT(I)
        DO 100 J = 1, KK
          XL0000(I,J) = PFAC * XX(J) *
     $                  (WT(J) * SUM + XX(I) / BINDIF(I,J))
 100    CONTINUE
        XL0000(I,I) = ZERO
 200  CONTINUE
C
C     Invert L00,00 using LAPACK or LINPACK
C
C*****precision > double - lapack
      CALL DGETRF (KK, KK, XL0000, KK, IPVT, INFO)
      IF (INFO .NE. 0) THEN
C <error module="tranlib" severity="error">
C <id>12</id>
C <message>An error occurred in subroutine MCORDF
C during LU factorization of the L-matrix, used to calculate
C ordinary diffusion coefficients.  Check that the transport
C properties for the species are correct.
C </message>
C <message level="2">The math Lapack subroutine
C DGETRF returned the value of INFO = %1.
C </message>
C </error>
          KERR = .TRUE.
          IDERR = 13
          MSGSTR = ' '
          WRITE (MSGSTR,'(I5)') INFO
          CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
          WRITE (6,*) ' ERROR IN DGETRF, INFO = ', INFO
          RETURN
CEM         STOP
      ENDIF
      CALL DGETRI(KK, XL0000, KK, IPVT, WORK, KK, INFO)
      IF (INFO .NE. 0) THEN
C <error module="tranlib" severity="error">
C <id>13</id>
C <message>An error occurred in subroutine MCORDF
C during inversion of the L-matrix, used to calculate
C ordinary diffusion coefficients.  Check that the transport
C properties for the species are correct.
C </message>
C <message level="2">The math Lapack
C subroutine DGETRI returned the value of INFO = %1.
C </message>
C </error>
          KERR = .TRUE.
          IDERR = 14
          MSGSTR = ' '
          WRITE (MSGSTR,'(I5)') INFO
          CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
          WRITE (6,*) ' ERROR IN DGETRI, INFO = ', INFO
          RETURN
CEM         STOP
      ENDIF
C*****END precision > double - lapack
C
C*****precision > double - linpack
C      CALL DGEFA (XL0000, KK, KK, IPVT, INFO)
C      IF (INFO .NE. 0) THEN
CC <error module="tranlib" severity="error">
CC <id>14</id>
CC <message>An error occurred in subroutine MCORDF
CC during LU factorization of the L-matrix, used to calculate
CC ordinary diffusion coefficients.  Check that the transport
CC properties for the species are correct.
CC </message>
CC <message level="2">The math Linpack subroutine
CC DGEFA returned the value of INFO = %1.
CC </message>
CC </error>
C        KERR = .TRUE.
C        IDERR = 15
C        MSGSTR = ' '
C        WRITE(MSGSTR,'(I5)') INFO
C        CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
C        WRITE (6, *) ' ERROR IN DGEFA, INFO = ', INFO
C        STOP
C      ENDIF
C      CALL DGEDI (XL0000, KK, KK, IPVT, DET, WORK, JOB)
C*****END precision > double - linpack
C
C*****precision > single - lapack
C      CALL SGETRF (KK, KK, XL0000, KK, IPVT, INFO)
C      IF (INFO .NE. 0) THEN
CC <error module="tranlib" severity="error">
CC <id>15</id>
CC <message>An error occurred in subroutine MCORDF
CC during LU factorization of the L-matrix, used to calculate
CC ordinary diffusion coefficients.  Check that the transport
CC properties for the species are correct.
CC </message>
CC <message level="2">The math Lapack subroutine
CC SGETRF returned the value of INFO = %1.
CC </message>
CC </error>
C          KERR = .TRUE.
C          IDERR = 16
C          MSGSTR = ' '
C          WRITE (MSGSTR,'(I5)') INFO
C          CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
C          WRITE (6,*) ' ERROR IN SGETRF, INFO = ', INFO
C          STOP
C      ENDIF
C      CALL SGETRI(KK, XL0000, KK, IPVT, WORK, KK, INFO)
CC <error module="tranlib" severity="error">
CC <id>16</id>
CC <message>An error occurred in subroutine MCORDF
CC during inversion of the L-matrix, used to calculate
CC ordinary diffusion coefficients.  Check that the transport
CC properties for the species are correct.
CC </message>
CC <message level="2">The math Lapack
CC subroutine SGETRI returned the value of INFO = %1.
CC </message>
CC </error>
C      KERR = .TRUE.
C      IDERR = 17
C      MSGSTR = ' '
C      WRITE (MSGSTR,'(I5)') INFO
C      CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
C      IF (INFO .NE. 0) THEN
C          WRITE (6,*) ' ERROR IN SGETRI, INFO = ', INFO
C          STOP
C      ENDIF
C*****END precision > single - lapack
C
C*****precision > single - linpack
C      CALL SGEFA (XL0000, KK, KK, IPVT, INFO)
C      IF (INFO .NE. 0) THEN
CC <error module="tranlib" severity="error">
CC <id>17</id>
CC <message>An error occurred in subroutine MCORDF
CC during LU factorization of the L-matrix, used to calculate
CC ordinary diffusion coefficients.  Check that the transport
CC properties for the species are correct.
CC </message>
CC <message level="2">The math Linpack subroutine
CC SGEFA returned the value of INFO = %1.
CC </message>
CC </error>
C         KERR = .TRUE.
C         IDERR = 18
C         MSGSTR = ' '
C         WRITE (MSGSTR,'(I5)') INFO
C         CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
C        WRITE (6, *) ' ERROR IN SGEFA, INFO = ', INFO
C        STOP
C      ENDIF
C      CALL SGEDI (XL0000, KK, KK, IPVT, DET, WORK, JOB)
C*****END precision > single - linpack
C
C     Compute the ordinary multicomponent diffusion coefficients
C
      SUM = ZERO
      DO 400 I = 1, KK
        SUM = SUM + WT(I) * X(I)
  400 CONTINUE
      PFAC = PFAC * SUM
C
      DO 500 J = 1, KK
         PFAC_J = PFAC / WT(J)
         DO 450 I = 1, KK
            D(I,J) = PFAC_J * XX(I) * (XL0000(I,J)-XL0000(I,I))
  450    CONTINUE
  500 CONTINUE
C
C     end of SUBROUTINE MCORDF
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCLMDT (P, T, X, KK, KK3, SMALL, WT, EOK, ZROT, LIN,
     1                   EPS, ICKWRK, CKWRK, RMCWRK, XX, VIS, ASTAR,
     2                   BSTAR, CSTAR, XI, CPOR, CROTOR, CINTOR, XL,
     3                   R, BINDIF, IPVT, DT, COND, KERR)
C
C  START PROLOGUE
C
C  SUBROUTINE MCLMDT (P, T, X, KK, KK3, SMALL, WT, EOK, ZROT, LIN,
C 1                   EPS, ICKWRK, CKWRK, RMCWRK, XX, VIS, ASTAR,
C 2                   BSTAR, CSTAR, XI, CPOR, CROTOR, CINTOR, XL,
C 3                   R, BINDIF, IPVT, DT, COND, KERR)
C  This subroutine computes the thermal conductivity, and the thermal
C  diffusion coefficient array; it does so by first forming the L
C  matrix, and then solving Eq. 24A.
C  This routine is not normally called directly by the user;
C  the user calls MCMCDT, which in turn calls MCLMDT.
C
C  INPUT
C  P        - Real scalar, pressure.
C                cgs units, dynes/cm**2
C  T        - Real scalar, temperature.
C                cgs units, K
C  X(*)     - Real array, mole fractions of the mixture;
C             dimension at least KK, the total species count.
C  KK       - Integer scalar, total species count.
C  KK3      - Integer scalar, three times the species count;
C             the size of the L matrix is KK3 * KK3.
C  SMALL    - Real scalar; the mole fractions used in the transport
C             computation are given by XX(K) = X(K) + SMALL.
C  WT(*)    - Real array, species molecular weights;
C             dimension at least KK, the total species count.
C  EOK(*,*) - Real matrix, reduced well depths for species pairs;
C             dimension at least KK, the total species count, for
C             both the first and second dimensions.
C                cgs units, K
C  ZROT(*)  - Real array, rotational collision numbers evaluated at
C             298K;
C             dimension at least KK, the total species count.
C  LIN(*)   - Integer array; flags indicating linearity of species;
C             dimension at least KK, the total species count.
C             NLIN=0, single atom,
C             NLIN=1, linear molecule,
C             NLIN=2, nonlinear molecule.
C  EPS(*)   - Real array, Lennard-Jones potential well depths;
C             dimension at least KK, the total species count.
C                cgs units, K
C  ICKWRK(*)- Integer CHEMKIN workspace array;
C             dimension at least LENICK.
C  RCKWRK(*)- Real   CHEMKIN workspace array;
C             dimension at least LENRCK.
C  RMCWRK(*)- Real   TRANSPORT workspace array;
C             dimension at least LENRMC.
C  XX(*)    - Real array, species mole fractions used in the
C             transport computation;
C             dimension at least KK, the total species count.
C             XX(K) = X(K) + SMALL.
C  VIS(*)   - Real array, species viscosities evaluated from MCSVIS;
C             dimension at least KK, the total species count.
C                cgs units, cm/cm-s
C  ASTAR(*,*) Real matrix, collision integrals A* for species pairs;
C             dimension at least KK, the total species count, for
C             both the first and second dimensions.
C  BSTAR(*,*) Real matrix, collision integrals B* for species pairs;
C             dimension at least KK, the total species count, for
C             both the first and second dimensions.
C  CSTAR(*,*) Real matrix, collision integrals C* for species pairs;
c             dimension at least KK, the total species count, for
C             both the first and second dimensions.
C  XI(*)    - Real array, collision numbers for the transfer of
C             rotational energy of species I into translational
C             energy of species J (Eq. 42), assuming that all
C             XI(I,J) = XI(I,I) (see p.132 for discussion);
C             dimension at least KK, the total species count.
C  CPOR(*)  - Real array, dimensionless specific heats CP/R for the
C             species;
C             dimension at least KK, the total species count.
C  CROT(*)  - Real array, dimensionless rotational contributions to
C             the specific heats of the species;
C             cimension at least KK, the total species count.
C  CINT(*)  - Real array, dimensionless internal contributions to
C             the specific heats of the species;
C             dimension at least KK, the total species count.
C  XL(*,*)  - Real matrix, the L matrix, Eq. 43 and 49;
C             dimension at least 3*KK, where KK is the total species
C             count, for both the first and second dimensions.
C  R(*)     - Real array, the right-hand sides of Eq. 24A;
C             dimension at least 3*KK, where KK is the total species
C             count.
C  BINDIF(*,*)-Real matrix, binary diffusion coefficients;
C             dimension at least KK, the total species count, for
C             both the first and second dimensions.
C                cgs units, cm**2/s
C  IPVT(*)  - Integer array, pivot indices for inversion of the XL
C             matrix by LINPACK routines SGEFA and SGEDI;
C             dimension at least 3*KK, where KK is the total species
C             count.
C
C  OUTPUT
C  DT(*)    - Real array, thermal multicomponent diffusion
C             coefficients;
C             dimension at least KK, the total species count.
C                cgs units, gm/(cm*sec)
C  COND     - Real scalar, mixture thermal conductivity.
C                cgs units, erg/(cm*K*s)
C  KERR     - Logical flag indicating whether an error was found
C             during matrix manipulations
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
      PARAMETER       (ONE = 1.0D0, ZERO = 0.0D0)
      PARAMETER (RU=8.314510D+07, PI= 3.1415926535897932D+00,
     $                        PI32O2= 2.7841639984158539D+00,
     $                        P2O4P2= 4.4674011002723397D+00,
     $                          PI32= 5.5683279968317078D+00)
C*****END precision > double
C
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C      PARAMETER (ONE = 1.0E0, ZERO = 0.0E0)
C      PARAMETER (RU=8.314510E+07, PI= 3.1415926535897932E+00,
C     $                        PI32O2= 2.7841639984158539E+00,
C     $                        P2O4P2= 4.4674011002723397E+00,
C     $                          PI32= 5.5683279968317078E+00)
C*****END precision > single
C
      DIMENSION X(*), ICKWRK(*), CKWRK(*), RMCWRK(*), WT(*), XX(*),
     1          VIS(*), EOK(KK,*), ZROT(*), LIN(*), EPS(*),
     2          ASTAR(KK,*), BSTAR(KK,*), CSTAR(KK,*), XI(*), CPOR(*),
     3          CROTOR(*), CINTOR(*), XL(KK3,*), R(*), BINDIF(KK,*),
     4          IPVT(*), DT(*), FITAST(7), FITBST(7), FITCST(7)
      LOGICAL   KERR
      CHARACTER*80 MSGSTR
      SAVE      FITAST, FITBST, FITCST
C
C     Fits of A*, B*, and C* as functions of LN(T*)
C
      DATA FITAST / .1106910525E+01, -.7065517161E-02,
     1             -.1671975393E-01,  .1188708609E-01,
     2              .7569367323E-03, -.1313998345E-02,
     3              .1720853282E-03/
C
      DATA FITBST / .1199673577E+01, -.1140928763E+00,
     1             -.2147636665E-02,  .2512965407E-01,
     2             -.3030372973E-02, -.1445009039E-02,
     3              .2492954809E-03/
C
      DATA FITCST / .8386993788E+00,  .4748325276E-01,
     1              .3250097527E-01, -.1625859588E-01,
     2             -.2260153363E-02,  .1844922811E-02,
     3             -.2115417788E-03/
C
C     Set minimum mole fraction to SMALL
C     (Note, the possibility of negative mole fractions
C     necessitates the use of the MAX function ).
C
      DO 50 K = 1, KK
        XX(K) = MAX( X(K) ,  SMALL)
   50 CONTINUE
C
C     Determine A*, B*, and C* for each species
C     Note, these are symmetric matrices because EOK(I,J)
C     is symmetric
C
C      TLOG = LOG(T)
      DO 100 J = 1, KK
         DO 90 I = 1, J
C
         TSLOG = LOG ( T/EOK(I,J) )
C         TSLOG = TLOG - LOG(EOK(I,J))

         T1 = TSLOG
         T2 = TSLOG*T1
         T3 = TSLOG*T2
         T4 = TSLOG*T3
         T5 = TSLOG*T4
         T6 = TSLOG*T5
         ASTAR(I,J) = FITAST(1)    + FITAST(2)*T1 + FITAST(3)*T2 +
     1                FITAST(4)*T3 + FITAST(5)*T4 + FITAST(6)*T5 +
     2                FITAST(7)*T6
         ASTAR(J,I) = ASTAR(I,J)
         BSTAR(I,J) = FITBST(1)    + FITBST(2)*T1 + FITBST(3)*T2 +
     1                FITBST(4)*T3 + FITBST(5)*T4 + FITBST(6)*T5 +
     2                FITBST(7)*T6
         BSTAR(J,I) = BSTAR(I,J)
         CSTAR(I,J) = FITCST(1)    + FITCST(2)*T1 + FITCST(3)*T2 +
     1                FITCST(4)*T3 + FITCST(5)*T4 + FITCST(6)*T5 +
     2                FITCST(7)*T6
         CSTAR(J,I) = CSTAR(I,J)
   90    CONTINUE
  100 CONTINUE
C
C     Evaluate the binary diffusion coefficients and viscosity
C
      CALL MCSDIF (P, T, KK, RMCWRK, BINDIF)
      CALL MCSVIS (T, RMCWRK, VIS)
C
      PFAC = 1.2 * RU * T / P
      DO 150 K = 1, KK
C
C        Evaluate binary self-diffusion coefficients from viscosity
C
         BINDIF(K,K) = PFAC * ASTAR(K,K) * VIS(K) / WT(K)
C
C        Compute Parker correction for ZROT
C
         DD = EPS(K) / T
         DR = EPS(K) / 298.0
         SQRTDD = SQRT(DD)
         SQRTDR = SQRT(DR)
         DD32 = SQRTDD*DD
         DR32 = SQRTDR*DR
         XI(K) = ( (ONE + PI32O2*SQRTDR + P2O4P2*DR + PI32*DR32) /
     1             (ONE + PI32O2*SQRTDD + P2O4P2*DD + PI32*DD32)  )
     2            * MAX(ONE, ZROT(K))
  150 CONTINUE
C
C     Rotational and internal parts of specific heat
C
      CALL CKCPOR (T, ICKWRK, CKWRK, CPOR)
      DO 400 K = 1, KK
         IF (LIN(K) .EQ. 0) THEN
            CROTOR(K) = ZERO
            CINTOR(K) = ZERO
         ELSEIF (LIN(K) .EQ. 1) THEN
            CROTOR(K) = ONE
            CINTOR(K) = CPOR(K) - 2.5
         ELSEIF (LIN(K) .EQ. 2) THEN
            CROTOR(K) = 1.5
            CINTOR(K) = CPOR(K) - 2.5
         ENDIF
  400 CONTINUE
C
C     Assemble L00,00
C
      PFAC = 16.0 * T / (25.0 * P)
      DO 600 I = 1, KK
         SUM = - XX(I) / BINDIF(I,I)
         DO 450 K = 1, KK
           SUM = SUM + XX(K) / BINDIF(I,K)
  450    CONTINUE
         SUM = SUM / WT(I)
         DO 500 J = 1, KK
           XL(I,J) =   PFAC * XX(J) *
     $                        (WT(J) * SUM + XX(I) / BINDIF(J,I))
500      CONTINUE
         XL(I,I) = ZERO
  600 CONTINUE
C
C     Assemble L00,10 and L10,00
C
      PFAC = 8.0 * T / (5.0 * P)
      DO 1200 J = 1, KK
         WTJ_TMP = WT(J)
         XJ_TMP  = X(J)
         SUM     = ZERO
         DO 1150 I = 1, KK
            XL(I, J+KK) = -  PFAC * XX(I) * XJ_TMP * WT(I)
     1                       * (1.2*CSTAR(J,I) - ONE) /
     2                         ((WTJ_TMP + WT(I)) * BINDIF(J,I))
            XL(J+KK, I) = XL(I, J+KK)
            SUM = SUM   + XL(I, J+KK)
 1150    CONTINUE
         XL(J, J+KK) = XL(J, J+KK) - SUM
         XL(J+KK, J) = XL(J, J+KK)
 1200 CONTINUE
C
C     Assemble L01,00 and L00,01
C
      DO 1400 J = 1, KK
         DO 1300 I = 1, KK
            XL(2*KK+I, J) = ZERO
            XL(I, 2*KK+J) = ZERO
 1300    CONTINUE
 1400 CONTINUE
C
C     Assemble diagonal and off-diagonal elements of L10,10
C
      PFAC = 16.0D0 * T / (25.0 * P)
      PIFAC = 5.0 / (3.0*PI)
      DO 1600 J = 1, KK
        WTJ_TMP = WT(J)
        CROT_J  = CROTOR(J) / XI(J)
        PFAC_J  = PFAC * XX(J) * WTJ_TMP
        SUM     = ZERO
        DO 1550 I = 1, KK
          FAC_1 = XX(I) / ((WT(I) + WTJ_TMP)**2 * BINDIF(I,J))
          FAC_2 = 4.0*ASTAR(I,J)*
     $              (ONE + PIFAC*(CROTOR(I)/XI(I) + CROT_J))
          XL(I+KK, J+KK) = PFAC_J * WT(I) * FAC_1
     $                    * ( 13.75 - 3.0*BSTAR(I,J) - FAC_2 )
          SUM = SUM + FAC_1
     $              * (   7.5*WTJ_TMP**2
     $                  + WT(I)*(  WT(I)*(6.25 - 3.0*BSTAR(J,I))
     $                           + WTJ_TMP * FAC_2 )
     $                )
 1550   CONTINUE
        XL(J+KK, J+KK) = XL(J+KK, J+KK) - PFAC*XX(J)*SUM
 1600 CONTINUE
C
C     Assemble L10,01 and L01,10, both the off-diagonal entries
C     and the on-diagonal entries.
C
      NN = 2*KK
      PFAC = 32.0 * T / (5.0 * PI * P)
      DO 1850 J = 1, KK
         IF (LIN(J) .NE. 0) THEN
            NN = NN + 1
            SUM = ZERO
            WTJ_TMP = WT(J)
            PFAC_J =   ( PFAC * WTJ_TMP * XX(J) * CROTOR(J) )
     $               / ( CINTOR(J) * XI(J) )
            DO 1800 I = 1, KK
C                             The L10,01 term:
              XL(I+KK, NN) = ( PFAC_J * ASTAR(J,I) * XX(I)     )
     $                     / ( (WTJ_TMP + WT(I)) * BINDIF(J,I) )
C                             The L01,10 term:
              XL(NN, I+KK) =  XL(I+KK, NN)
C                             The extra term that get's stuck
C                             on the diagonal:
              SUM    = SUM +  XL(I+KK, NN)
 1800       CONTINUE
C
C           Extra diagonal entries:
C               (These use the viscosity, eq. 49, in their formulation,
C                because the self-diffusion coefficient has been
C                reevaluated to be consistent with the viscosity.)
C
            XL(J+KK, NN) = XL(J+KK, NN) + SUM
            XL(NN, J+KK) = XL(NN, J+KK) + SUM
         ENDIF
 1850 CONTINUE
C
C     Assemble L01,01 using viscosity Eq. (49).
C
      DO 2000 J = 1, KK
         DO 1900 I = 1, KK
            XL(2*KK+I, 2*KK+J) = ZERO
 1900    CONTINUE
 2000 CONTINUE
C
      NN = 2*KK
      PFAC  = 4.0 * T / P
      PIFAC = 12.0 / (5.0 * PI)
      PIRU  = - 8.0 / (PI * RU)
      DO 2200 I = 1, KK
         IF (LIN(I) .NE. 0) THEN
            NN = NN + 1
            SUM = ZERO
            FAC_1 = ( PIFAC * WT(I) * CROTOR(I) )
     $            / ( CINTOR(I) * XI(I)         )
            DO 2100 K = 1, KK
               FAC_2 = XX(K) / BINDIF(I,K)
               SUM = SUM + FAC_2
               IF (I .NE. K) THEN
                  SUM = SUM + (FAC_1 * FAC_2 * ASTAR(I,K)) / WT(K)
               ENDIF
 2100       CONTINUE
            FAC_2 = XX(I) / CINTOR(I)
            XL(NN, NN) =     ( PIRU * WT(I) * FAC_2 * CROTOR(I) )
     1                     / ( VIS(I) * XI(I)                   )
     2                   - PFAC * SUM
            XL(NN, NN) = FAC_2 * XL(NN, NN)
         ENDIF
 2200 CONTINUE
C
C     Assemble the right-hand side for solving Eq. (24)
C
      NN = 2*KK
      DO 3300 I = 1, KK
         R(I)    = ZERO
         R(I+KK) = XX(I)
         IF (LIN(I) .NE. 0) THEN
            NN  = NN + 1
            R(NN) = XX(I)
         ENDIF
 3300 CONTINUE
C
C     Factor and solve Eq. (24).
C
C*****precision > double - lapack
      CALL DGETRF (NN, NN, XL, KK3, IPVT, INFO)
      IF (INFO .NE. 0) THEN
C <error module="tranlib" severity="error">
C <id>18</id>
C <message>An error occurred in subroutine MCLMDT
C during LU factorization of the L-matrix, used to calculate
C thermal diffusion coefficients.  Check that the transport
C properties for the species are correct.
C </message>
C <message level="2">The math Lapack subroutine
C DGETRF returned the value of INFO = %1.
C </message>
C </error>
          IDERR = 19
          MSGSTR = ' '
          WRITE (MSGSTR,'(I5)') INFO
          CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
          KERR = .TRUE.
          WRITE (6,*) ' ERROR IN DGETRF, INFO = ', INFO
          RETURN
CEM          STOP
      ENDIF
      CALL DGETRS('N', NN, 1, XL, KK3, IPVT, R, NN, INFO)
      IF (INFO .NE. 0) THEN
C <error module="tranlib" severity="error">
C <id>19</id>
C <message>An error occurred in subroutine MCLMDT
C during solution of the L-matrix equation for determination of
C thermal diffusion coefficients.  Check that the transport
C properties for the species are correct.
C </message>
C <message level="2">The math Lapack subroutine
C DGETRS returned the value of INFO = %1.
C </message>
C </error>
          IDERR = 20
          MSGSTR = ' '
          WRITE (MSGSTR,'(I5)') INFO
          CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
          KERR = .TRUE.
          WRITE (6,*) ' ERROR IN DGETRS, INFO = ', INFO
          RETURN
CEM          STOP
      ENDIF
C*****END precision > double - lapack
C
C*****precision > double - linpack
C      CALL DGEFA (XL, KK3, NN, IPVT, INFO)
C      IF (INFO .NE. 0) THEN
CC <error module="tranlib" severity="error">
CC <id>20</id>
CC <message>An error occurred in subroutine MCLMDT
CC during LU factorization of the L-matrix, used to calculate
CC thermal diffusion coefficients.  Check that the transport
CC properties for the species are correct.
CC </message>
CC <message level="2">The math Linpack subroutine
CC DGEFA returned the value of INFO = %1.
CC </message>
CC </error>
C          IDERR = 21
C          MSGSTR = ' '
C          WRITE (MSGSTR,'(I5)') INFO
C          CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
C          WRITE (6,*) ' ERROR IN DGEFA, INFO = ', INFO
C          STOP
C      ENDIF
C      CALL DGESL (XL, KK3, NN, IPVT, R, 0)
C*****END precision > double - linpack
C
C*****precision > single - lapack
C      CALL SGETRF (NN, NN, XL, KK3, IPVT, INFO)
C      IF (INFO .NE. 0) THEN
CC <error module="tranlib" severity="error">
CC <id>21</id>
CC <message>An error occurred in subroutine MCLMDT
CC during LU factorization of the L-matrix, used to calculate
CC thermal diffusion coefficients.  Check that the transport
CC properties for the species are correct.
CC </message>
CC <message level="2">The math Lapack subroutine
CC SGETRF returned the value of INFO = %1.
CC </message>
CC </error>
C          IDERR = 22
C          MSGSTR = ' '
C          WRITE (MSGSTR,'(I5)') INFO
C          CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
C          WRITE (6,*) ' ERROR IN SGETRF, INFO = ', INFO
C          STOP
C      ENDIF
C      CALL SGETRS('N', NN, 1, XL, KK3, IPVT, R, NN, INFO)
C      IF (INFO .NE. 0) THEN
CC <error module="tranlib" severity="error">
CC <id>22</id>
CC <message>An error occurred in subroutine MCLMDT
CC during solution of the L-matrix equation for determination of
CC thermal diffusion coefficients.  Check that the transport
CC properties for the species are correct.
CC </message>
CC <message level="2">The math Lapack subroutine
CC SGETRS returned the value of INFO = %1.
CC </message>
CC </error>
C          IDERR = 23
C          MSGSTR = ' '
C          WRITE (MSGSTR,'(I5)') INFO
C          CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
C          WRITE (6,*) ' ERROR IN SGETRS, INFO = ', INFO
C          STOP
C      ENDIF
C*****END precision > single - lapack
C
C*****precision > single - linpack
C      CALL SGEFA (XL, KK3, NN, IPVT, INFO)
C      IF (INFO .NE. 0) THEN
CC <error module="tranlib" severity="error">
CC <id>23</id>
CC <message>An error occurred in subroutine MCLMDT
CC during LU factorization of the L-matrix, used to calculate
CC thermal diffusion coefficients.  Check that the transport
CC properties for the species are correct.
CC </message>
CC <message level="2">The math Linpack subroutine
CC SGEFA returned the value of INFO = %1.
CC </message>
CC </error>
C          IDERR = 24
C          MSGSTR = ' '
C          WRITE (MSGSTR,'(I5)') INFO
C          CALL ERSET( 'tranlib', IDERR, 3, MSGSTR )
C          WRITE (6,*) ' ERROR IN SGEFA, INFO = ', INFO
C          STOP
C      ENDIF
C      CALL SGESL (XL, KK3, NN, IPVT, R, 0)
C*****END precision > single - linpack
C
C     Form thermal diffusion coefficients
C
      PFAC = 1.6 / RU
      DO 4000 K = 1, KK
         DT(K)  = PFAC * WT(K) * XX(K) * R(K)
 4000 CONTINUE
C
C     Form the thermal conductivity
C
      CONDTR = ZERO
      DO 4100 K = 1, KK
         CONDTR = CONDTR + X(K) * R(KK+K)
 4100 CONTINUE
C
      NN = 2*KK
      CONDIN = ZERO
      DO 4200 K = 1, KK
         IF (LIN(K) .NE. 0) THEN
            NN = NN + 1
            CONDIN = CONDIN + X(K) * R(NN)
         ENDIF
 4200 CONTINUE
C
      COND = -4.0 * (CONDTR + CONDIN)
C
C     end of SUBROUTINE MCLMDT
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCATDR (T, X, IMCWRK, RMCWRK, TDR)
C
C  START PROLOGUE
C
C  SUBROUTINE MCATDR (T, X, IMCWRK, RMCWRK, TDR)
C  This subroutine computes the thermal diffusion ratios for the light
C  species into the mixture.
C
C  INPUT
C  T         - Real scalar, temperature.
C                 cgs units, K
C  X(*)      - Real array, mole fractions of the mixture;
C              dimension at least KK, the total species count.
C  IMCWRK(*) - Integer workspace array; dimension at least LENIMC.
C  RMCWRK(*) - Real    workspace array; dimension at least LENRMC.
C
C  OUTPUT
C  TDR(*)    - Real array, thermal diffusion ratios for the species;
C              dimension at least KK, the total species count.
C              TDR(K) = 0 for any species with molecular weight less
C              than 5.
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
      PARAMETER (ZERO = 0.0D0)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C      PARAMETER (ZERO = 0.0)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,
     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST,
     4                NCST, NXL, NR, NWRK, K3
      DIMENSION X(*), IMCWRK(*), RMCWRK(*), TDR(*)
C
C     In this subroutine, temporary storage is assigned as:
C     a vector of the "fitted" parts of the thermal diffusion ratios
C     are stored in RMCWRK(NXI), specifically, the vector represents
C     the J components of TDR(J,K), where K is the light species
C
      DO 100 K = 1, NKK
         TDR(K) = ZERO
  100 CONTINUE
C
      IF (NO .EQ. 4) THEN
        DO 400 L = 1, NLITE
           K = IMCWRK(IKTDIF+L-1)
           ISTRT = NTDIF + (L-1)*NO*NKK
           CALL MCEVAL4 (T, NKK, RMCWRK(ISTRT), RMCWRK(NXI))
           DO 350 J = 1, NKK
              TDR(K) = TDR(K) + RMCWRK(NXI+J-1)*X(K)*X(J)
  350      CONTINUE
  400   CONTINUE
        RETURN
      ENDIF
C
      DO 500 L = 1, NLITE
         K = IMCWRK(IKTDIF+L-1)
         ISTRT = NTDIF + (L-1)*NO*NKK
         CALL MCEVAL (T, NKK, NO, RMCWRK(ISTRT), RMCWRK(NXI))
         DO 450 J = 1, NKK
            TDR(K) = TDR(K) + RMCWRK(NXI+J-1)*X(K)*X(J)
  450    CONTINUE
  500 CONTINUE
C
C     end of SUBROUTINE MCATDR
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCCCEX (K, KDIM, RMCWRK, COFCON)
C
C  START PROLOGUE
C
C  SUBROUTINE MCCCEX (K, KDIM, RCKWRK, COFCON)
C  Gets or puts values of the fitting coefficients for the
C  polynomial fits to species conductivity.
C
C  INPUT
C  K         - Integer scalar, species index.
C              K > 0 gets coefficients from RMCWRK
C              K < 0 puts coefficients into RMCWRK
C  KDIM      - Dimension for COFCON - the total number of species
C  RMCWRK(*) - Real workspace array; dimension at least LENRMC.
C
C  If K < 1:
C  COFCON    - Real vector of polynomial coefficients for
C              the species' conductivity; dimension at least NO,
C              usually 4.
C
C  OUTPUT
C  If K > 1:
C  COFCON    - Real vector of polynomial coefficients for
C              the species' conductivity; dimension at least NO,
C              usually 4.
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,
     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST,
     4                NCST, NXL, NR, NWRK, K3
      DIMENSION RMCWRK(*), COFCON(*)
C
      NK = IABS(K)
      IF (K .GT. 0) THEN
C
C GET the data
C
         DO 200 N = 1, NO
            COFCON(N) = RMCWRK(NLAM + (NK-1)*NO + N - 1 )
200      CONTINUE
      ELSE IF (K .LT. 0) THEN
C
C PUT the data
C
         DO 400 N = 1, NO
            RMCWRK(NLAM + (NK-1)*NO + N - 1) = COFCON(N)
400      CONTINUE
      ENDIF
C
C     end of SUBROUTINE MCCCEX
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCCDEX (K, KDIM, RMCWRK, COFDIF)
C
C  START PROLOGUE
C
C  SUBROUTINE MCCDEX (K, KDIM, RCKWRK, COFDIF)
C  Gets or puts values of the fitting coefficients for the
C  polynomial fits to species binary diffusion coefficients.
C
C  INPUT
C  K         - Integer scalar, species index.
C              K > 0 gets coefficients from RMCWRK
C              K < 0 puts coefficients into RMCWRK
C  KDIM      - Dimension for COFDIF - the total number of species
C  RMCWRK(*) - Real workspace array; dimension at least LENRMC.
C
C  If K < 1:
C  COFDIF    - Real matrix of polynomial coefficients for
C              the species' binary diffusion coefficient with all
C              other species; The first dimension should be KK;
C              the second dimension should be NO, usually 4;
C
C  OUTPUT
C  If K > 1:
C  COFDIF    - Real matrix of polynomial coefficients for
C              the species' binary diffusion coefficient with all
C              other species; first dimension should be NO, usually 4;
C              The second dimension should be NKK
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,
     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST,
     4                NCST, NXL, NR, NWRK, K3
      DIMENSION RMCWRK(*), COFDIF(KDIM,*)
C
      NK = IABS(K)
      IF (K .GT. 0) THEN
C
C GET the data
C
         DO 200 N = 1, NO
C
C    get diffusion coefficients
C
            DO 150 J = 1, NKK
               COFDIF(J,N) =
     &           RMCWRK(NDIF + (NK-1)*NO*NKK + (J-1)*NO + N-1)
150         CONTINUE
200      CONTINUE
      ELSE IF (K .LT. 0) THEN
C
C PUT the data
C
         DO 400 N = 1, NO
C
C    put diffusion coeffs, keep binary diffusion matrix symmetric:
C
            DO 350 J = 1, NKK
               RMCWRK(NDIF + (NK-1)*NO*NKK + (J-1)*NO + N-1)
     &           = COFDIF(J,N)
               RMCWRK(NDIF + (J-1)*NO*NKK + (NK-1)*NO + N-1)
     &             = COFDIF(J,N)
350         CONTINUE
400      CONTINUE
      ENDIF
C
C     end of SUBROUTINE MCCDEX
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE MCCVEX (K, KDIM, RMCWRK, COFVIS)
C
C  START PROLOGUE
C
C  SUBROUTINE MCCVEX (K, KDIM, RCKWRK, COFVIS)
C  Gets or puts values of the fitting coefficients for the
C  polynomial fits to species viscosity.
C
C  INPUT
C  K         - Integer scalar, species index.
C              K > 0 gets coefficients from RMCWRK
C              K < 0 puts coefficients into RMCWRK
C  KDIM      - Dimension for COFVIS - the total number of species
C  RMCWRK(*) - Real workspace array; dimension at least LENRMC.
C
C  If K < 1:
C  COFVIS    - Real vector of polynomial coefficients for
C              the species' viscosity; dimension at least NO, usually 4
C
C  OUTPUT
C  If K > 1:
C  COFVIS    - Real vector of polynomial coefficients; dimension
C              at least NO, usually = 4
C
C  END PROLOGUE
C
C*****precision > double
      IMPLICIT real (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H,O-Z), INTEGER (I-N)
C*****END precision > single
C
      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,
     1                IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,
     2                NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,
     3                NCINT, NBIND, NEOK, NSGM, NAST, NBST,
     4                NCST, NXL, NR, NWRK, K3
      DIMENSION RMCWRK(*), COFVIS(*)
C
      NK = IABS(K)
      IF (K .GT. 0) THEN
C
C GET the data
C
         DO 200 N = 1, NO
            COFVIS(N) = RMCWRK(NETA + (NK-1)*NO + N - 1 )
200      CONTINUE
      ELSE IF (K .LT. 0) THEN
C
C PUT the data
C
         DO 400 N = 1, NO
            RMCWRK(NETA + (NK-1)*NO + N - 1) = COFVIS(N)
400      CONTINUE
         ENDIF
C
C     end of SUBROUTINE MCCVEX
      RETURN
      END

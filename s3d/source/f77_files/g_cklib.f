C                           DISCLAIMER
C
C   This file was generated on  by the version of
C   ADIFOR compiled on June, 1998.
C
C   ADIFOR was prepared as an account of work sponsored by an
C   agency of the United States Government, Rice University, and
C   the University of Chicago.  NEITHER THE AUTHOR(S), THE UNITED
C   STATES GOVERNMENT NOR ANY AGENCY THEREOF, NOR RICE UNIVERSITY,
C   NOR THE UNIVERSITY OF CHICAGO, INCLUDING ANY OF THEIR EMPLOYEES
C   OR OFFICERS, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES
C   ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETE-
C   NESS, OR USEFULNESS OF ANY INFORMATION OR PROCESS DISCLOSED, OR
C   REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
C
C    CVS $Revision: 1.1.1.1 $  created $Date: 2005/01/11 00:27:29 $
C      SUBROUTINE CKABS
C
C  SUBROUTINE CKABS is the comment and upgrades information for the
C  CHEMKIN-III subroutine library.
C
C///////////////////////////////////////////////////////////////////////
C
C  V.5.36 2000/12/15 F. Rupley
C  1) add extra checks for READ values, CKLEN, CKLEN2, CKINIT, CKPNT
C  V.5.35 2000/12/12 F. Rupley
C  1) add END=100 for READs, CKLEN, CKLEN2, CKINIT
C  V.5.34 2000/12/05 F. Rupley
C  1) ckstrt.h pointers to constants -
C     NcPI (PI), NcAVO (Avogadro's number), NcBOL (Boltzmann's constant),
C     NcPER (Permittivity constant), NcECH (Electron charge)
C  2) CKINIT initializes the constant values
C  3) subroutines which previously set PARAMETER values for above
C     now obtain them from RCKWRK(*)
C  V.5.33 2000/11/17 F. Rupley
C  1) modify CKXTND DO 75, needs to start at N+1 (not 2)
C  V.5.32 2000/10/12 F. Rupley
C  1) modify CKSUBS to ignore '!'when adjacent to text (Defect ID#381).
C  V.5.31 2000/10/02 F. Rupley
C  1) Correct CKINIT, which needs to read SUM of real coefficients
C     for the NRNU reactions.
C  V.5.30 2000/09/11 F. Rupley
C  1) Check NKKI>0 before proceeding, 
C     CKISIG, CKICON, CKIVIS, CKIDIF, CKECON, CKESIG, CKEVIS, CKQNUI
C  2) Check KEL>0 before proceeding, CKQNUE
C  V.5.29 2000/08/15 E. Meeks
C  1) Commented out function S2 that is not being used that gives
C     a warning during compile.
C  2) Added logical LDEMO in licensing block.
C  3) Added extra call CHKOUT with indication to try FLEXLOCK
C     option, given by the feature ID set to 99.
C  4) Added new license change block after reading the first few
C     records of the linking file, to check if the size of the
C     problem is not too big for the demo.  If it is too big,
C     we return with the same error that we would have had with a
C     licensing error.  Otherwise we proceed and set the ICKWRK(1)
C     integer to 99, so that the apps will know we are in demo mode.
C  V.5.28 2000/08/06 E. Meeks
C  1. Fixed problem with NORD reactions in CKRATX; incorrect index K
C  2. Fixed logic for case when ORD < -1 for NORD reactions
C  V.5.27 2000/07/27 F. Rupley
C  1. return from CKQNUE if NMOM<=0
C  2. return from CKQNUI if NMOM<=0
C  V.5.26 2000/07/26 F. Rupley per E. Meeks
C  1. a different initialization of BLOG in CKRATT
C  V.5.25 2000/07/26 F. Rupley
C  1. fix uninitialized BLOG in CKRATT
C  V.5.24 2000/07/24 F. Rupley per E. Meeks
C  1. Protect against overflow in CKRATT,
C     RKFT(I) for Fit#1 reactions
C  V.5.23 2000/07/02 F. Rupley
C  1. Protect against overflow in CKRATT
C     for change-order reactions by checking
C     range of concentrations and orders
C  V.5.22 2000/06/18 F. Rupley
C  1. Protect against overflow in CKRATT, CKEQ
C     by checking RKFT(I) against BIG
C  2. Correct comment in CKABS from SKINIT to CKINIT
C  3. move start of PROLOGUE below version comments so that they don't
C     print to listings.
C  4. Correct mis-spellings, and modify some spacing in order to
C     produce prologue printout for manual.
C  V.5.21 2000/03/30 F. Rupley
C  1. SUBROUTINE CKXTND modifies a function array to add or replace
C     end values.
C  V.5.20 2000/03/12 F. Rupley
C  1. set RCKWRK(1)=0 if no FLEXLM
C  V.5.19 2000/01/29 F. Rupley
C  1. Add pointer IcNSU and workspace over II for sum of coefficients,
C         pointer NcRSU and workspace over NRNU for sum of coefficients.
C  2. Add NUSUMK, RNUSUM arrays to argument lists, CKRAT, CKRATT, CKEQ
C  3. SUBROUTINE CKDONE -
C     opposite of CKINIT, to CHKIN license
C  V.5.18 1999/03/28 E. Meeks
C  1. Added subroutine CKREWR to write a new (modified)
C     Linking File from the work arrays.
C  V.5.17 1999/01/20 E. Meeks
C  1. Added subroutine CKLEN2 to read from the linking file and
C     return all information that may be needed to allocate work
C     array sizes in applications that call the chemkin library.
C  V.5.16 1998/10/16 E. Meeks
C  1. Correct all mispellings of AVAG to AVOG for Avogadro's Number
C  V 5.15 98/07/26 E. Meeks
C  1. Action#0185:  Corrected indexing in CKIORD; In IF statement when
C     KSPEC < 0, 'FORD(KSPEC,NFORD)' should be ' FORD(-KSPEC,NFORD)'
C  2. Action#0185: Changed call list, routine description and
C     function of CKIORD so that it just returns available information
C     about the NORD reactions: reaction indices and species orders,
C     rather than information about forward vs. reverse order
C     specifications, which is not available on the linking file.
C  V.5.14 98/04/08 E. Meeks
C  1. Action#0166:  Added subroutine CKIREV, to return explicitly
C     defined reverse reaction-rate coefficients A,B, and E
C  V.5.13 98/04/01 E. Meeks
C  1. Fix bug#0163: Correct pointer definition I_NK in soubroutine
C     CKIRNU:  I_NK = IcNK + (I-1)*MXSP.
C  V.5.12 97/10/28 E. Meeks
C  1. Fix bug#054b: improve overflow handling for FIT1 option in
C     CKRATT.
C  V.5.11 97/08/28 F. Rupley
C  1. Fix bug#005a: in CKRATX ensure non-negative concentrations to
C     calculate rates for real stoichiometric coefficients (DO 160),
C     and for changed orders (DO 200)
C  1. Fix bug#104: remove unused CKFRCH from EXTERNAL and INTEGER
C     statements in CKPARR.
C  V.5.10 97/07/23 (F. Rupley)
C  1. Fix bug#012: removed spacing from logical operators for f77 stnd.
C     Spaces found in CKEQ, line 3658 and CKRATT, line 9742.
C  V.5.9 97/04/15
C  1. move SAVE statements before DATA statements
C  V.5.8 97/03/01
C  1. fix NTR=1 case;
C     "final" version for Reaction Design
C  V.5.7, 96/12/24
C  1. fix indexing error, CKPY
C  V.5.6, 96/12/11
C  1. several bugfixes
C  V.5.5, 96/11/11
C  1. new time-saving logic in most thermodynamic and rate routines
C  V.5.4, 96/09/04
C  1. add INTEGER FUNCTION CKLKUP, returns index of an integer if
C     present in an integer array
C  V.5.3, 96/08/19
C  1. in CKRATT, set default EQK(I)=1.0 to cover SUMSMH=0.0.
C  VERSION 5.2, 96/05/24
C  1. initial sccs version
C  VERSION 5.1 (F. Rupley, April 26, 1996)
C  CAUTION...THIS WORK IS STILL IN PROGRESS:
C  1. CKRATX - IF (KFAL(N) .EQ. 0) should be IF (KFAL(N) .GT. 0)
C  2. delete CKRATN and CKQNCK (per E. Meeks)
C  3. reworked CKSYMR
C  CHANGES FOR VERSION 5.0 (CHEMKIN-III initial version,
C                           F. Rupley, March 1 1996)
C  1. add, modify comments sections throughout in preparation for
C     updated documentation.
C  2. add ascii linkfile options.
C  3. VERS=5.0 is linkfile version number, not to be changed unless
C     linkfile changes.
C  4. additional IFLO (new pointer IiFLO) for pressure-dependent
C     reactions, and additional logic for high- vs. low-pressure in
C     CKRATX.
C  5. eliminate some excess arguments from CKRATT list?
C  6. since many more linkfile records, in case of READ error,
C     set IFLAG=NREC, the index number of the record
C  CHANGES VERSION 4.8 (F. Rupley 11/27/95)
C  1. added or modified comments for CKSTRT, CKABS, and other subroutine
C     prolog sections (per E. Meeks) for upgrading documentation
C  2. reversed chronological or of VERSION change comments.
C  CHANGES FOR VERSION 4.7 (8/14.95 E. Meeks)
C  1.  Change maximum number of species in a reaction to 12
C      with corrections per R. Larson
C  2.  Added subroutine CKKTFL to initialize species temperature
C      array flag for nonthermal systems.  CKINIT now initializes
C      this array to ones for thermal systems.  The array only gets
C      set otherwise if CKKTFL is called by the application code.
C  3.  Generalized all temperature-dependent subroutines calculations
C      to allow for multiple species temperatures.  The default
C      temperature is T(1) for background gas temperature in plasma
C      cases or the temperature in thermal systems.
C  4.  Added subroutines for determining electron and ion transport
C      properties from reaction rate specifications for momentum-
C      transfer collisions.
C     CHANGES FOR VERSION 4.6 (2/27/95 F. Rupley)
C     1.  Change character index "(:" to "(1:"
C     CHANGES for VERSION 4.5 (1/19/95 per M. Coltrin)
C     1.  Add integer flag IFLAG to CKLEN and CKINIT and allow
C         RETURN instead of STOP for IFLAG error conditions.
C     CHANGES for VERSION 4.4 (11/17/94 per R. Steeper)
C     1.  Simplify CKRHOC,PKRHOC (superflous calculation for RHO)
C     2.  Simplify CKPC   (superflous calculations)
C     CHANGES for VERSION 4.3 (10/3/94 F. Rupley per E. Meeks)
C     1.  Correct calculation of RHO in PKRHOX.
C     CHANGES for VERSION 4.23 (8/26/94)
C     1.  Correct value of RUC (RCKWRK(NcRC)) in CKINIT.
C     CHANGES for VERSION 4.22 (8/15/94)
C     1.  Remove NSPEC(*) from CKRATX call list (ICKWRK(IcNS))
C     CHANGES for VERSION 4.21 (8/10/94)
C     1.  Accepts version 3.9 linkfile
C     CHANGES for VERSION 4.2 (6/13/94 F. Rupley, per E. Meeks)
C     1.  Modify CKRATT for plasma options.
C     2.  Add SUBROUTINES CKHRX, CKIEIM, CKIEXC
C     CHANGES for VERSION 4.10c (6/3/94 F. Rupley)
C     1.  Accept linkfile 3.6c (bugfixes per H. Moffat)
C     CHANGES for VERSION 4.10b (5/20/94 F. Rupley per E. Meeks)
C     1.  Incorporate plasma options (linkfile 3.6b)
C     CHANGES for VERSION 4.10 (4/28/94 F. Rupley, per M. Coltrin)
C     1.  New subroutines CKINU, CKIRNU, and CKIORD for real
C         stoichiometric coefficients and change of order reactions.
C     2.  Recognize linkfile
C     CHANGES FOR VERSION 4.9 (4/20/94 F. Rupley)
C     1.  Accept binary file V.3.6 (correction to CKUNIT)
C     CHANGES FOR VERSION 4.8 (4/19/94 F. Rupley)
C     1.  Accept binary file V.3.5 (correction to CKBAL, CKRBAL)
C     CHANGES FOR VERSION 4.7 (4/14/94 F. Rupley, suggested by E. Meeks)
C     1.  use INCLUDE 'ckstrt.h' instead of having the CKSTRT common
C         block in every subroutine.
C     CHANGES FOR VERSION 4.6 (3/15/94 F. Rupley)
C     1.  DOS/PC compatibility effort includes adding file names to
C         OPEN statements (updating interpreter), removing
C         unusued but possibly initialized variables.
C     CHANGES FOR V.4.5 (1/26/94 F. Rupley per R. Kee)
C     1. Implement real stoichometric coefficients; binary file V.3.3.
C     CHANGES FOR V.4.4 (11/10/93 F. Rupley)
C     1. Accept binary file V.3.2 (correction to CKUNIT)
C     CHANGES FOR V.4.3 (11/9/93 F. Rupley per E. Meeks)
C     1. Min/max single-precision exponent in CKR2CH should be
C        be 30 instead of 38.
C     CHANGES FOR V.4.2 (9/14/93 F. Rupley)
C     1. Move perturbation factoring from CKRATT to CKRATX
C     CHANGES FOR V.4.1 (2/24/93 F. Rupley)
C     1. Accept binary file V.3.1 (correction to CKREAC)
C     CHANGES FOR V.4.0 (10/1/92 F. Rupley per M. Coltrin)
C     1. COMMON /CKCONS/ VERS, PREC, KERR, LENI, LENR, LENC
C        eliminates need for LINKCK in argument list of CKSAVE
C     CHANGES FOR V.3.9 (4/17/92 F. Rupley)
C     1. Bugfix in CKSAVE (did not write new pointers NcKF,NcKR)
C     CHANGES FOR V.3.8 (4/15/92 F. Rupley)
C     1. Accept binary file V.3.0 (correction to CKDUP)
C     CHANGES FOR VERSION 3.7 (3/10/92 F. Rupley per Kee/Grcar)
C     1. Calls to CKRAT replaced by calls to CKRATT and CKRATX.
C     2. New subroutine CKKFRT returns the forward and reverse
C        rates (RKFT, RKRT) calculated by CKRATT (does not consider
C        pressure dependencies).
C     3. New subroutine CKWYPK returns the rates of production
C        given the RKFT and RKRT from (2).
C     CHANGES FOR VERSION 3.6 (2/24/92 F. Rupley per E. Meeks)
C     1. Accept binary file V.2.9 (additional error checking for
C        reverse T-L reactions, 2*II additional real work space)
C     2. Correct calculation for reverse T-L reaction rates
C     3. New subroutines CKRATT, CKRATX (subsets of CKRAT)
C     4. New pointers NcKF,NcKR to store intermediate temperature-
C        dependent rates.
CC     CHANGES FOR VERSION 3.4 (2/19/92 F. Rupley)
C     1. Correct error in CKITR (IcNR should be IcNS)
C     CHANGES FOR VERSION 3.3 (6/27/91 F. Rupley)
C     1. Accept binary file V.2.8 (modified interpreter output to
C        print all 16 characters of species names)
C     CHANGES FOR VERSION 3.2 (6/10/91 H. Moffat)
C     1. Added Subroutine CKFAL, which returns the pressure-dependency
C        parameters for the mechanism.
C     2. Added Subroutine CKNUF, which returns the reactant
C        stoichiometric coefficients.
C     3. Fixed an error in CKSNUM, which caused an error condition when
C        the input string did not have any blanks inbetween words.
C     4. Fixed two errors in CKTHB. The default third body efficiency
C        should be equal to 1.
C     CHANGES FOR VERSION 3.1 (5/9/91 F. Rupley)
C     1. Add Subroutine CKMXTP to return number of temperatures used
C        in thermodynamic fits.
C     CHANGES FOR VERSION 3.0 (4/1/91 F. Rupley)
C     1. Accept binary file V.2.7 (modification of CKDUP)
C        coefficient a6.
C    CHANGES TO VERSION 2.9 (2/15/91, F. Rupley per R. Kee)
C     2. Subroutine CKRHEX allows perturbation of thermodynamic
C     1. Add a fourth parameter to the array of Arhennius coefficients
C        for the II reactions;
C        increase the value of NPAR in COMMON /CKSTRT/ by one (this
C        also increases the length of the array of reverse Arhennius
C        parameters);
C        initialize the value of the fourth parameter to 1.0 in
C        CKINIT;
C        use this value as a "perturbation factor" for the forward
C        rates in CKRAT;
C        add SUBROUTINE CKRDEX to allow applications codes to change
C        the perturbation factor RD(I) in sensitivity calculations.
C     2. Accept binary file V.2.6 (LENRCK was increased by II+NREV to
C        reflect above changes in RCKWRK array.
C    CHANGES TO VERSION 2.8 (1/18/91, F. Rupley)
C     1. Accept binary file V.2.5
C    CHANGES TO VERSION 2.7 (12/20/90, F. Rupley)
C     1. Accept binary file V.2.4
C    CHANGES TO VERSION 2.6 (12/15/90, F. Rupley)
C     1. Accept binary file V.2.3
C    CHANGES TO VERSION 2.5 (11/15/90, F. Rupley)
C     1. Accept binary file V.2.2
C    CHANGES TO VERSION 2.4
C     1. Accept binary  file V.2.1
C    CHANGES TO VERSION 2.3
C     1. Accept binary file V.2.0
C    CHANGES TO VERSION 2.2
C     1. Bugfix in CKABML
C     2. In CKXNUM (and CKSNUM), if NEXP is negative, it is not an
C        error to find fewer values.
C    CHANGES TO VERSION 2.1
C     1. New binary file has an additional record to indicate its
C        version, machine precision, and error status
C     2. SUBROUTINE CKPNT reads a binary file to get COMMON /CKSTRT/
C        pointers.
C     3. SUBROUTINE CKSAVE writes pointers and work arrays to a
C        binary file.
C     4. Add COMMON /MACH/ and initialization of BIG,SMALL,EXPARG to
C        SUBROUTINE CKPNT
C     5. Change LOG(*) to LOG(MAX(*,SMALL)) in several subroutines.
C    CHANGES TO VERSION 2.0
C     1. Subroutine CKLEN to provide necessary lengths of work arrays.
C     2. Subroutine CKKFKR provides arrays of forward and reverse
C        reaction rates.
C    CHANGES FROM VERSION 1.8
C     1. vax/cray change blocks for machine constants changed to
C        smallexp, bigexp change blocks
C     2. add ERR= to first read statement in CKINIT
C    CHANGES FROM VERSION 1.7
C     1. Get rid of non-standard comment statements.
C    CHANGES FROM VERSION 1.6
C     1. Added error checking and additional arguments to character
C        manipulation subroutines.
C     2. Fixed an error with IFIRCH(LINE) in IPPLEN
C    CHANGES FROM VERSION 1.5
C     1. Implement Landau-Teller rate expression
C     2. Add utility routine CKR2CH
C    CHANGES FROM VERSION 1.4
C     1. New versions of CKABML/CKABMS, CKGBML/CKGBMS, CKSBML/CKSBMS
C        for mixture-averaging require additional argument P
C     2. Replace SDOT's and DDOT's by loops
C     3. Reaction strings now have "=>" in irreversible reactions,
C                                 "<=>" in reversible reactions.
C    CHANGES FROM VERSION 1.3
C     1. added PROLOGUES
C     2. binary file now includes minimum length of arrays
C    CHANGES FROM VERSION 1.2
C     1. change SCOPY for integer arrays to DO loops
C    CHANGES FROM VERSION 1.1
C     1. add change block to CKCHRG
C     2. correct change block in CKHORT
C    CHANGES FROM VERSION 1.0
C     1. REPLACE "REAL*8" WITH "real"
C
C//////////////////////////////////////////////////////////////////////C
C
CInactive procedure or block data 'ckabs' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckabe' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckabml' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckabms' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckaml' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckams' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
C*****precision > double
CInactive procedure or block data 'ckatom' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckathm' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckawt' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
C*****precision > double
CInactive procedure or block data 'ckbsec' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckxtnd' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckavg' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcdc' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcopy' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckdot' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcdxp' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcdxr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcdyp' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcdyr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckchrg' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcomp' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckncmp' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcont' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcpbl' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcpbs' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcpml' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcpms' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcpor' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcray' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckctc' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckctx' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckctxp' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckctxr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcty' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckctyp' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckctyr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcvbl' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcvbs' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcvml' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckcvms' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckecon' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckedif' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckeq' excluded from output
C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckeqc' excluded from output
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckeqxp' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckeqxr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckeqyp' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckeqyr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckesig' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckevis' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckfal' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckfalp' excluded from output
C
C----------------------------------------------------------------------C
C
CInactive procedure or block data 'ckfal1' excluded from output
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckgbml' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckgbms' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      subroutine g_ckgml(g_p_, t, g_t, ldg_t, ickwrk, rckwrk, g_rckwrk, 
     *ldg_rckwrk, gml, g_gml, ldg_gml)
C
C  START PROLOGUE
C
C  SUBROUTINE CKGML  (T, ICKWRK, RCKWRK, GML)
C  Returns the standard state Gibbs free energies in molar units;
C  see Eq. (24).
C
C  INPUT
C  T(*)      - Real array, temperature(s); dimension is determined by
C              the application program to be the total number of
C              species temperatures, nominally 1.
C                 cgs units, K
C  ICKWRK(*) - Integer workspace array; dimension at least LENICK.
C  RCKWRK(*) - Real    workspace array; dimension at least LENRCK.
C
C  OUTPUT
C  GML(*)    - Real array, standard state Gibbs free energies for
C              the species;
C              dimension at least KK, the total species count.
C                 cgs units, ergs/mole
C
C  END PROLOGUE
C*****precision > double
        implicit real (a-h, o-z), integer (i-n)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
C       Start of include file D:/DNS/ARK_Release/AUTOGETRATES/run/ckstrt.h
C       CVS Revision:$Revision: 1.1.1.1 $  created $Date: 2005/01/11 00:27:29 $
C     this is CHEMKIN-III file ckstrt.h V.3.1 December 2000;
C     it contains pointers for the gas-phase kinetics
C       subroutines' data storage arrays
C
C
C     include file for CHEMKIN-III cklib.f, dated: December 8, 2000
C
        integer nmm, nkk, nii, mxsp, mxtb, mxtp, ncp, ncp1, ncp2, ncp2t,
     * npar, nlar, nfar, nlan, nfal, nrev, nthb, nrlt, nwl, neim, njan, 
     *njar, nft1, nf1r, nexc, nmom, nxsm, ntde, nrnu, nord, mxord, kel, 
     *nkki, icmm, ickk, icnc, icph, icch, icnt, icnu, icnk, icnsu, icns,
     * icnr, iclt, icrl, icrv, icwl, icfl, icfo, icft, ickf, ictb, ickn,
     * ickt, icei, icet, icjn, icf1, icex, icmo, icmk, icxs, icxi, icxk,
     * ictd, ictk, icrnu, icord, ickor, icki, icktf, ick1, ick2, ncaw, n
     *cwt, nctt, ncaa, ncco, ncrv, nclt, ncrl, ncfl, nckt, ncwl, ncjn, n
     *cf1, ncex, ncru, ncrc, ncpa, ncavo, ncper, ncech, ncbol, ncpi, nck
     *f, nckr, ncrnu, ncrsu, nckor, nck1, nck2, nck3, nck4, nci1, nci2, 
     *nci3, nci4
C
C
        common /ckstrt/ nmm, nkk, nii, mxsp, mxtb, mxtp, ncp, ncp1, ncp2
     *, ncp2t, npar, nlar, nfar, nlan, nfal, nrev, nthb, nrlt, nwl, neim
     *, njan, njar, nft1, nf1r, nexc, nmom, nxsm, ntde, nrnu, nord, mxor
     *d, kel, nkki, icmm, ickk, icnc, icph, icch, icnt, icnu, icnk, icns
     *u, icns, icnr, iclt, icrl, icrv, icwl, icfl, icfo, icft, ickf, ict
     *b, ickn, ickt, icei, icet, icjn, icf1, icex, icmo, icmk, icxs, icx
     *i, icxk, ictd, ictk, icrnu, icord, ickor, icki, icktf, ick1, ick2,
     * ncaw, ncwt, nctt, ncaa, ncco, ncrv, nclt, ncrl, ncfl, nckt, ncwl,
     * ncjn, ncf1, ncex, ncru, ncrc, ncpa, ncavo, ncper, ncech, ncbol, n
     *cpi, nckf, nckr, ncrnu, ncrsu, nckor, nck1, nck2, nck3, nck4, nci1
     *, nci2, nci3, nci4
C
C     Integer constants
C
C        0     1     2     3     4     5     6     7     8     9
C        10    11    12    13    14    15    16    17    18    19   
C        20    21    22    23    24    25    26    27    28    29
C        30     31    32
C
C     Integer pointers to character arrays in CCKWRK
C
C        33    34
C
C     Integer pointers to integer arrays in ICKWRK
C
C        35    36    37    38    39    40    41    42    43    44     
C        45    46    47    48    49    50    51    52    53    54
C        55    56    57    58    59    60    61    62    63    64    
C        65    66    67    68    69    70    71    72    73   
C
C     Integer pointers to real variables and arrays in RCKWRK
C
C        74    75    76    77    78    79    80    81    82    83    
C        84    85    86    87    88    89    90    91     92
C        93     94     95    96    97    98 
C        99    100   101   102   103   104   105   106   107   108
C
C
C Logical global variables
C
C      NPERT = TRUE if any of the perturbation factors for the rate
C              constants have been changed from the value of one.
C
        logical lpert
        common /ckglbl/ lpert
C
C     END include file for cklib.f
C
        dimension t(*), ickwrk(*), rckwrk(*), gml(*)
C
        integer g_pmax_
        parameter (g_pmax_ = 4)
        integer g_i_, g_p_, ldg_t, ldg_gml, ldg_rckwrk
        real g_tk(g_pmax_), g_t(ldg_t, *), g_gml(ldg_gml, *), g_rckwrk(l
     *dg_rckwrk, *)
        integer g_ehfid
        save g_tk
        external g_cksml
        external g_ckhml
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'ckgml','g_cklib.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        call g_ckhml(g_p_, t, g_t, ldg_t, ickwrk, rckwrk, g_rckwrk, ldg_
     *rckwrk, rckwrk(nck1), g_rckwrk(1, nck1), ldg_rckwrk)
        call g_cksml(g_p_, t, g_t, ldg_t, ickwrk, rckwrk, g_rckwrk, ldg_
     *rckwrk, rckwrk(nck2), g_rckwrk(1, nck2), ldg_rckwrk)
        nkm1 = nkk - 1
        do 99932 k = 0, nkm1
          do g_i_ = 1, g_p_
            g_tk(g_i_) = g_t(g_i_, ickwrk(icktf + k))
          enddo
          tk = t(ickwrk(icktf + k))
C--------
          do g_i_ = 1, g_p_
            g_gml(g_i_, k + 1) = g_rckwrk(g_i_, nck1 + k) + (-rckwrk(nck
     *2 + k)) * g_tk(g_i_) + (-tk) * g_rckwrk(g_i_, nck2 + k)
          enddo
          gml(k + 1) = rckwrk(nck1 + k) - tk * rckwrk(nck2 + k)
C--------
150       continue
99932   continue
C
C     end of SUBROUTINE CKGML
        return
      end
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckgms' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckhbml' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckhbms' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      subroutine g_ckhml(g_p_, t, g_t, ldg_t, ickwrk, rckwrk, g_rckwrk, 
     *ldg_rckwrk, hml, g_hml, ldg_hml)
C
C  START PROLOGUE
C
C  SUBROUTINE CKHML  (T, ICKWRK, RCKWRK, HML)
C  Returns the enthalpies in molar units.
C
C  INPUT
C  T(*)      - Real array, temperature(s); dimension is determined by
C              the application program to be the total number of
C              species temperatures, nominally 1.
C                 cgs units, K
C  ICKWRK(*) - Integer workspace array; dimension at least LENICK.
C  RCKWRK(*) - Real    workspace array; dimension at least LENRCK.
C
C  OUTPUT
C  HML(*)    - Real array, enthalpies for species;
C              dimension at least KK, the total species count.
C                 cgs units, ergs/mole
C
C  END PROLOGUE
C*****precision > double
        implicit real (a-h, o-z), integer (i-n)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
C       Start of include file D:/DNS/ARK_Release/AUTOGETRATES/run/ckstrt.h
C       CVS Revision:$Revision: 1.1.1.1 $  created $Date: 2005/01/11 00:27:29 $
C     this is CHEMKIN-III file ckstrt.h V.3.1 December 2000;
C     it contains pointers for the gas-phase kinetics
C       subroutines' data storage arrays
C
C
C     include file for CHEMKIN-III cklib.f, dated: December 8, 2000
C
        integer nmm, nkk, nii, mxsp, mxtb, mxtp, ncp, ncp1, ncp2, ncp2t,
     * npar, nlar, nfar, nlan, nfal, nrev, nthb, nrlt, nwl, neim, njan, 
     *njar, nft1, nf1r, nexc, nmom, nxsm, ntde, nrnu, nord, mxord, kel, 
     *nkki, icmm, ickk, icnc, icph, icch, icnt, icnu, icnk, icnsu, icns,
     * icnr, iclt, icrl, icrv, icwl, icfl, icfo, icft, ickf, ictb, ickn,
     * ickt, icei, icet, icjn, icf1, icex, icmo, icmk, icxs, icxi, icxk,
     * ictd, ictk, icrnu, icord, ickor, icki, icktf, ick1, ick2, ncaw, n
     *cwt, nctt, ncaa, ncco, ncrv, nclt, ncrl, ncfl, nckt, ncwl, ncjn, n
     *cf1, ncex, ncru, ncrc, ncpa, ncavo, ncper, ncech, ncbol, ncpi, nck
     *f, nckr, ncrnu, ncrsu, nckor, nck1, nck2, nck3, nck4, nci1, nci2, 
     *nci3, nci4
C
C
        common /ckstrt/ nmm, nkk, nii, mxsp, mxtb, mxtp, ncp, ncp1, ncp2
     *, ncp2t, npar, nlar, nfar, nlan, nfal, nrev, nthb, nrlt, nwl, neim
     *, njan, njar, nft1, nf1r, nexc, nmom, nxsm, ntde, nrnu, nord, mxor
     *d, kel, nkki, icmm, ickk, icnc, icph, icch, icnt, icnu, icnk, icns
     *u, icns, icnr, iclt, icrl, icrv, icwl, icfl, icfo, icft, ickf, ict
     *b, ickn, ickt, icei, icet, icjn, icf1, icex, icmo, icmk, icxs, icx
     *i, icxk, ictd, ictk, icrnu, icord, ickor, icki, icktf, ick1, ick2,
     * ncaw, ncwt, nctt, ncaa, ncco, ncrv, nclt, ncrl, ncfl, nckt, ncwl,
     * ncjn, ncf1, ncex, ncru, ncrc, ncpa, ncavo, ncper, ncech, ncbol, n
     *cpi, nckf, nckr, ncrnu, ncrsu, nckor, nck1, nck2, nck3, nck4, nci1
     *, nci2, nci3, nci4
C
C     Integer constants
C
C        0     1     2     3     4     5     6     7     8     9
C        10    11    12    13    14    15    16    17    18    19   
C        20    21    22    23    24    25    26    27    28    29
C        30     31    32
C
C     Integer pointers to character arrays in CCKWRK
C
C        33    34
C
C     Integer pointers to integer arrays in ICKWRK
C
C        35    36    37    38    39    40    41    42    43    44     
C        45    46    47    48    49    50    51    52    53    54
C        55    56    57    58    59    60    61    62    63    64    
C        65    66    67    68    69    70    71    72    73   
C
C     Integer pointers to real variables and arrays in RCKWRK
C
C        74    75    76    77    78    79    80    81    82    83    
C        84    85    86    87    88    89    90    91     92
C        93     94     95    96    97    98 
C        99    100   101   102   103   104   105   106   107   108
C
C
C Logical global variables
C
C      NPERT = TRUE if any of the perturbation factors for the rate
C              constants have been changed from the value of one.
C
        logical lpert
        common /ckglbl/ lpert
C
C     END include file for cklib.f
C
        dimension t(*), ickwrk(*), rckwrk(*), hml(*)
        save tn1, tn2, tn3, tn4, tn5
        integer g_pmax_
        parameter (g_pmax_ = 4)
        integer g_i_, g_p_, ldg_t, ldg_hml, ldg_rckwrk
        real r23_b, r22_b, r21_b, r20_b, r12_b, r8_b, r17_b, r16_b, r22_
     *v, r9_b
        real r2_b, r13_b, g_tn1(g_pmax_), g_t(ldg_t, *), g_tn2(g_pmax_),
     * g_tn3(g_pmax_), g_tn4(g_pmax_), g_tn5(g_pmax_), g_tk1(g_pmax_), g
     *_tk2(g_pmax_)
        real g_tk3(g_pmax_), g_tk4(g_pmax_), g_tk5(g_pmax_), g_hml(ldg_h
     *ml, *), g_rckwrk(ldg_rckwrk, *)
        integer g_ehfid
        save g_tn1, g_tn2, g_tn3, g_tn4, g_tn5, g_tk1, g_tk2, g_tk3, g_t
     *k4, g_tk5
        intrinsic real
        data tn1 /1.0/
C
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'ckhml','g_cklib.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        if (t(1) .ne. tn1) then
C        FIRST of the species-specific temperature array (default)
          do g_i_ = 1, g_p_
            g_tn1(g_i_) = g_t(g_i_, 1)
          enddo
          tn1 = t(1)
C--------
          r2_b = tn1 + tn1
          do g_i_ = 1, g_p_
            g_tn2(g_i_) = r2_b * g_tn1(g_i_)
          enddo
          tn2 = tn1 * tn1
C--------
          do g_i_ = 1, g_p_
            g_tn3(g_i_) = tn2 * g_tn1(g_i_) + tn1 * g_tn2(g_i_)
          enddo
          tn3 = tn1 * tn2
C--------
          do g_i_ = 1, g_p_
            g_tn4(g_i_) = tn3 * g_tn1(g_i_) + tn1 * g_tn3(g_i_)
          enddo
          tn4 = tn1 * tn3
C--------
          do g_i_ = 1, g_p_
            g_tn5(g_i_) = tn4 * g_tn1(g_i_) + tn1 * g_tn4(g_i_)
          enddo
          tn5 = tn1 * tn4
C--------
          r2_b = 1.0 / real(2)
          do g_i_ = 1, g_p_
            g_tn2(g_i_) = r2_b * g_tn2(g_i_)
          enddo
          tn2 = tn2 / real(2)
C--------
          r2_b = 1.0 / real(3)
          do g_i_ = 1, g_p_
            g_tn3(g_i_) = r2_b * g_tn3(g_i_)
          enddo
          tn3 = tn3 / real(3)
C--------
          r2_b = 1.0 / real(4)
          do g_i_ = 1, g_p_
            g_tn4(g_i_) = r2_b * g_tn4(g_i_)
          enddo
          tn4 = tn4 / real(4)
C--------
          r2_b = 1.0 / real(5)
          do g_i_ = 1, g_p_
            g_tn5(g_i_) = r2_b * g_tn5(g_i_)
          enddo
          tn5 = tn5 / real(5)
C--------
        endif
        nkm1 = nkk - 1
        do 99928 k = 0, nkm1
          if (ickwrk(icktf + k) .ne. 1) then
C           different temperature required by this species
            do g_i_ = 1, g_p_
              g_tk1(g_i_) = g_t(g_i_, ickwrk(icktf + k))
            enddo
            tk1 = t(ickwrk(icktf + k))
C--------
            r2_b = tk1 + tk1
            do g_i_ = 1, g_p_
              g_tk2(g_i_) = r2_b * g_tk1(g_i_)
            enddo
            tk2 = tk1 * tk1
C--------
            do g_i_ = 1, g_p_
              g_tk3(g_i_) = tk2 * g_tk1(g_i_) + tk1 * g_tk2(g_i_)
            enddo
            tk3 = tk1 * tk2
C--------
            do g_i_ = 1, g_p_
              g_tk4(g_i_) = tk3 * g_tk1(g_i_) + tk1 * g_tk3(g_i_)
            enddo
            tk4 = tk1 * tk3
C--------
            do g_i_ = 1, g_p_
              g_tk5(g_i_) = tk4 * g_tk1(g_i_) + tk1 * g_tk4(g_i_)
            enddo
            tk5 = tk1 * tk4
C--------
            r2_b = 1.0 / real(2)
            do g_i_ = 1, g_p_
              g_tk2(g_i_) = r2_b * g_tk2(g_i_)
            enddo
            tk2 = tk2 / real(2)
C--------
            r2_b = 1.0 / real(3)
            do g_i_ = 1, g_p_
              g_tk3(g_i_) = r2_b * g_tk3(g_i_)
            enddo
            tk3 = tk3 / real(3)
C--------
            r2_b = 1.0 / real(4)
            do g_i_ = 1, g_p_
              g_tk4(g_i_) = r2_b * g_tk4(g_i_)
            enddo
            tk4 = tk4 / real(4)
C--------
            r2_b = 1.0 / real(5)
            do g_i_ = 1, g_p_
              g_tk5(g_i_) = r2_b * g_tk5(g_i_)
            enddo
            tk5 = tk5 / real(5)
C--------
          else
            do g_i_ = 1, g_p_
              g_tk1(g_i_) = g_tn1(g_i_)
            enddo
            tk1 = tn1
C--------
            do g_i_ = 1, g_p_
              g_tk2(g_i_) = g_tn2(g_i_)
            enddo
            tk2 = tn2
C--------
            do g_i_ = 1, g_p_
              g_tk3(g_i_) = g_tn3(g_i_)
            enddo
            tk3 = tn3
C--------
            do g_i_ = 1, g_p_
              g_tk4(g_i_) = g_tn4(g_i_)
            enddo
            tk4 = tn4
C--------
            do g_i_ = 1, g_p_
              g_tk5(g_i_) = g_tn5(g_i_)
            enddo
            tk5 = tn5
C--------
          endif
C
C        number of temperature ranges for this species
          ntr = ickwrk(icnt + k) - 1
C        location of FIRST set of thermodynamic fit coefficients
          na1 = ncaa + k * ncp2t
C        location of upper limit of FIRST temperature range
          ktemp = nctt + k * mxtp + 1
C
200       continue
          if (ntr .gt. 1 .and. tk1 .gt. rckwrk(ktemp)) then
C           Remaining number of temperature ranges
            ntr = ntr - 1
C           Location of next set of fit coefficients
            na1 = na1 + ncp2
            ktemp = ktemp + 1
C           Check against next temperature, unless last
            if (ntr .gt. 1) then
              goto 200
            endif
          endif
C
          r22_v = rckwrk(na1) * tk1 + rckwrk(na1 + 1) * tk2 + rckwrk(na1
     * + 2) * tk3 + rckwrk(na1 + 3) * tk4 + rckwrk(na1 + 4) * tk5 + rckw
     *rk(na1 + ncp1 - 1)
          r8_b = rckwrk(ncru) * tk5
          r9_b = rckwrk(ncru) * rckwrk(na1 + 4)
          r12_b = rckwrk(ncru) * tk4
          r13_b = rckwrk(ncru) * rckwrk(na1 + 3)
          r16_b = rckwrk(ncru) * tk3
          r17_b = rckwrk(ncru) * rckwrk(na1 + 2)
          r20_b = rckwrk(ncru) * tk2
          r21_b = rckwrk(ncru) * rckwrk(na1 + 1)
          r22_b = rckwrk(ncru) * tk1
          r23_b = rckwrk(ncru) * rckwrk(na1)
          do g_i_ = 1, g_p_
            g_hml(g_i_, k + 1) = r22_v * g_rckwrk(g_i_, ncru) + r22_b * 
     *g_rckwrk(g_i_, na1) + r23_b * g_tk1(g_i_) + r20_b * g_rckwrk(g_i_,
     * na1 + 1) + r21_b * g_tk2(g_i_) + r16_b * g_rckwrk(g_i_, na1 + 2) 
     *+ r17_b * g_tk3(g_i_) + r12_b * g_rckwrk(g_i_, na1 + 3) + r13_b * 
     *g_tk4(g_i_) + r8_b * g_rckwrk(g_i_, na1 + 4) + r9_b * g_tk5(g_i_) 
     *+ rckwrk(ncru) * g_rckwrk(g_i_, na1 + ncp1 - 1)
          enddo
          hml(k + 1) = rckwrk(ncru) * r22_v
C--------
250       continue
99928   continue
C
C     end of SUBROUTINE CKHML
        return
      end
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckhms' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckhort' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckhrx' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cki2ch' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckicon' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckidif' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckieim' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckiexc' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckimom' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckindx' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckinit' excluded from output
C
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckdone' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckinu' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckion' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckiord' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckirev' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckirnu' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckisig' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckitde' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckitr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckivis' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckixsm' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckkfkr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckkfrt' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckktfl' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cklen' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cklen2' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckdlim' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckdtab' excluded from output
C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckchup' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckchlo' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cklkup' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C
CInactive procedure or block data 'ckfrch' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C
CInactive procedure or block data 'cklsch' excluded from output
C                                                                      C
CInactive procedure or block data 'ckslen' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckxmin' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckxmax' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckmmwc' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckmmwx' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckmmwy' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckmxtp' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckncf' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cknorm' excluded from output
C                                                                      C
CInactive procedure or block data 'ckscal' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
C*****precision > double
CInactive procedure or block data 'cksum' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cknpar' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cknu' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cknuf' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckpc' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckphaz' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckpnt' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckpx' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckpy' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckqc' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckqnue' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckqnui' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckqxp' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckqyp' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckqyr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckr2ch' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckraex' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckrat' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckratt' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckratx' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckrcxp' excluded from output
C
C----------------------------------------------------------------------C
C
CInactive procedure or block data 'ckrtcn' excluded from output
C
C----------------------------------------------------------------------C
C
CInactive procedure or block data 'ckrdex' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckrewr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckrhex' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckrhoc' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckrhox' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      subroutine g_ckrhoy(g_p_, p, g_p, ldg_p, t, g_t, ldg_t, y, g_y, ld
     *g_y, ickwrk, rckwrk, g_rckwrk, ldg_rckwrk, rho)
C
C  START PROLOGUE
C
C  SUBROUTINE CKRHOY (P, T, Y, ICKWRK, RCKWRK, RHO)
C  Returns the mass density of the gas mixture given pressure,
C  temperature(s) and mass fractions;  see Eq. (2).
C
C  INPUT
C  P         - Real scalar, pressure.
C                 cgs units, dynes/cm**2
C  T(*)      - Real array, temperature(s); dimension is determined by
C              the application program to be the total number of
C              species temperatures, nominally 1.
C                 cgs units, K
C  Y(*)      - Real array, mass fractions of the mixture;
C              dimension at least KK, the total species count.
C  ICKWRK(*) - Integer workspace array; dimension at least LENICK.
C  RCKWRK(*) - Real    workspace array; dimension at least LENRCK.
C
C  OUTPUT
C  RHO       - Real scalar, mass density.
C                 cgs units, gm/cm**3
C
C  END PROLOGUE
C*****precision > double
        implicit real (a-h, o-z), integer (i-n)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
C       Start of include file D:/DNS/ARK_Release/AUTOGETRATES/run/ckstrt.h
C       CVS Revision:$Revision: 1.1.1.1 $  created $Date: 2005/01/11 00:27:29 $
C     this is CHEMKIN-III file ckstrt.h V.3.1 December 2000;
C     it contains pointers for the gas-phase kinetics
C       subroutines' data storage arrays
C
C
C     include file for CHEMKIN-III cklib.f, dated: December 8, 2000
C
        integer nmm, nkk, nii, mxsp, mxtb, mxtp, ncp, ncp1, ncp2, ncp2t,
     * npar, nlar, nfar, nlan, nfal, nrev, nthb, nrlt, nwl, neim, njan, 
     *njar, nft1, nf1r, nexc, nmom, nxsm, ntde, nrnu, nord, mxord, kel, 
     *nkki, icmm, ickk, icnc, icph, icch, icnt, icnu, icnk, icnsu, icns,
     * icnr, iclt, icrl, icrv, icwl, icfl, icfo, icft, ickf, ictb, ickn,
     * ickt, icei, icet, icjn, icf1, icex, icmo, icmk, icxs, icxi, icxk,
     * ictd, ictk, icrnu, icord, ickor, icki, icktf, ick1, ick2, ncaw, n
     *cwt, nctt, ncaa, ncco, ncrv, nclt, ncrl, ncfl, nckt, ncwl, ncjn, n
     *cf1, ncex, ncru, ncrc, ncpa, ncavo, ncper, ncech, ncbol, ncpi, nck
     *f, nckr, ncrnu, ncrsu, nckor, nck1, nck2, nck3, nck4, nci1, nci2, 
     *nci3, nci4
C
C
        common /ckstrt/ nmm, nkk, nii, mxsp, mxtb, mxtp, ncp, ncp1, ncp2
     *, ncp2t, npar, nlar, nfar, nlan, nfal, nrev, nthb, nrlt, nwl, neim
     *, njan, njar, nft1, nf1r, nexc, nmom, nxsm, ntde, nrnu, nord, mxor
     *d, kel, nkki, icmm, ickk, icnc, icph, icch, icnt, icnu, icnk, icns
     *u, icns, icnr, iclt, icrl, icrv, icwl, icfl, icfo, icft, ickf, ict
     *b, ickn, ickt, icei, icet, icjn, icf1, icex, icmo, icmk, icxs, icx
     *i, icxk, ictd, ictk, icrnu, icord, ickor, icki, icktf, ick1, ick2,
     * ncaw, ncwt, nctt, ncaa, ncco, ncrv, nclt, ncrl, ncfl, nckt, ncwl,
     * ncjn, ncf1, ncex, ncru, ncrc, ncpa, ncavo, ncper, ncech, ncbol, n
     *cpi, nckf, nckr, ncrnu, ncrsu, nckor, nck1, nck2, nck3, nck4, nci1
     *, nci2, nci3, nci4
C
C     Integer constants
C
C        0     1     2     3     4     5     6     7     8     9
C        10    11    12    13    14    15    16    17    18    19   
C        20    21    22    23    24    25    26    27    28    29
C        30     31    32
C
C     Integer pointers to character arrays in CCKWRK
C
C        33    34
C
C     Integer pointers to integer arrays in ICKWRK
C
C        35    36    37    38    39    40    41    42    43    44     
C        45    46    47    48    49    50    51    52    53    54
C        55    56    57    58    59    60    61    62    63    64    
C        65    66    67    68    69    70    71    72    73   
C
C     Integer pointers to real variables and arrays in RCKWRK
C
C        74    75    76    77    78    79    80    81    82    83    
C        84    85    86    87    88    89    90    91     92
C        93     94     95    96    97    98 
C        99    100   101   102   103   104   105   106   107   108
C
C
C Logical global variables
C
C      NPERT = TRUE if any of the perturbation factors for the rate
C              constants have been changed from the value of one.
C
        logical lpert
        common /ckglbl/ lpert
C
C     END include file for cklib.f
C
        dimension t(*), y(*), ickwrk(*), rckwrk(*)
C
        integer g_pmax_
        parameter (g_pmax_ = 4)
        integer g_p_, ldg_p, ldg_t, ldg_y, ldg_rckwrk
        real g_p(ldg_p), g_t(ldg_t, *), g_y(ldg_y, *), g_rckwrk(ldg_rckw
     *rk, *)
        integer g_ehfid
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'ckrhoy','g_cklib.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        sum = 0.0
        nkm1 = nkk - 1
        do 99814 k = 0, nkm1
          sum = sum + y(k + 1) * t(ickwrk(icktf + k)) / rckwrk(ncwt + k)
150       continue
99814   continue
        rho = p / (sum * rckwrk(ncru))
C
C     end of SUBROUTINE CKRHOY
        return
      end
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckrp' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cksave' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cksbml' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cksbms' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cksmh' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      subroutine g_cksml(g_p_, t, g_t, ldg_t, ickwrk, rckwrk, g_rckwrk, 
     *ldg_rckwrk, sml, g_sml, ldg_sml)
C
C  START PROLOGUE
C
C  SUBROUTINE CKSML  (T, ICKWRK, RCKWRK, SML)
C  Returns the standard state entropies in molar units.
C
C  INPUT
C  T(*)      - Real array, temperature(s); dimension is determined by
C              the application program to be the total number of
C              species temperatures, nominally 1.
C                 cgs units, K
C  ICKWRK(*) - Integer workspace array; dimension at least LENICK.
C  RCKWRK(*) - Real    workspace array; dimension at least LENRCK.
C
C  OUTPUT
C  SML(*)    - Real array, standard state entropies for species;
C              dimension at least KK, the total species count.
C                 cgs units, ergs/(mole*K)
C
C  END PROLOGUE
C*****precision > double
        implicit real (a-h, o-z), integer (i-n)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
C       Start of include file D:/DNS/ARK_Release/AUTOGETRATES/run/ckstrt.h
C       CVS Revision:$Revision: 1.1.1.1 $  created $Date: 2005/01/11 00:27:29 $
C     this is CHEMKIN-III file ckstrt.h V.3.1 December 2000;
C     it contains pointers for the gas-phase kinetics
C       subroutines' data storage arrays
C
C
C     include file for CHEMKIN-III cklib.f, dated: December 8, 2000
C
        integer nmm, nkk, nii, mxsp, mxtb, mxtp, ncp, ncp1, ncp2, ncp2t,
     * npar, nlar, nfar, nlan, nfal, nrev, nthb, nrlt, nwl, neim, njan, 
     *njar, nft1, nf1r, nexc, nmom, nxsm, ntde, nrnu, nord, mxord, kel, 
     *nkki, icmm, ickk, icnc, icph, icch, icnt, icnu, icnk, icnsu, icns,
     * icnr, iclt, icrl, icrv, icwl, icfl, icfo, icft, ickf, ictb, ickn,
     * ickt, icei, icet, icjn, icf1, icex, icmo, icmk, icxs, icxi, icxk,
     * ictd, ictk, icrnu, icord, ickor, icki, icktf, ick1, ick2, ncaw, n
     *cwt, nctt, ncaa, ncco, ncrv, nclt, ncrl, ncfl, nckt, ncwl, ncjn, n
     *cf1, ncex, ncru, ncrc, ncpa, ncavo, ncper, ncech, ncbol, ncpi, nck
     *f, nckr, ncrnu, ncrsu, nckor, nck1, nck2, nck3, nck4, nci1, nci2, 
     *nci3, nci4
C
C
        common /ckstrt/ nmm, nkk, nii, mxsp, mxtb, mxtp, ncp, ncp1, ncp2
     *, ncp2t, npar, nlar, nfar, nlan, nfal, nrev, nthb, nrlt, nwl, neim
     *, njan, njar, nft1, nf1r, nexc, nmom, nxsm, ntde, nrnu, nord, mxor
     *d, kel, nkki, icmm, ickk, icnc, icph, icch, icnt, icnu, icnk, icns
     *u, icns, icnr, iclt, icrl, icrv, icwl, icfl, icfo, icft, ickf, ict
     *b, ickn, ickt, icei, icet, icjn, icf1, icex, icmo, icmk, icxs, icx
     *i, icxk, ictd, ictk, icrnu, icord, ickor, icki, icktf, ick1, ick2,
     * ncaw, ncwt, nctt, ncaa, ncco, ncrv, nclt, ncrl, ncfl, nckt, ncwl,
     * ncjn, ncf1, ncex, ncru, ncrc, ncpa, ncavo, ncper, ncech, ncbol, n
     *cpi, nckf, nckr, ncrnu, ncrsu, nckor, nck1, nck2, nck3, nck4, nci1
     *, nci2, nci3, nci4
C
C     Integer constants
C
C        0     1     2     3     4     5     6     7     8     9
C        10    11    12    13    14    15    16    17    18    19   
C        20    21    22    23    24    25    26    27    28    29
C        30     31    32
C
C     Integer pointers to character arrays in CCKWRK
C
C        33    34
C
C     Integer pointers to integer arrays in ICKWRK
C
C        35    36    37    38    39    40    41    42    43    44     
C        45    46    47    48    49    50    51    52    53    54
C        55    56    57    58    59    60    61    62    63    64    
C        65    66    67    68    69    70    71    72    73   
C
C     Integer pointers to real variables and arrays in RCKWRK
C
C        74    75    76    77    78    79    80    81    82    83    
C        84    85    86    87    88    89    90    91     92
C        93     94     95    96    97    98 
C        99    100   101   102   103   104   105   106   107   108
C
C
C Logical global variables
C
C      NPERT = TRUE if any of the perturbation factors for the rate
C              constants have been changed from the value of one.
C
        logical lpert
        common /ckglbl/ lpert
C
C     END include file for cklib.f
C
        dimension t(*), ickwrk(*), rckwrk(*), sml(*)
        save tn1, tnlog, tn2, tn3, tn4
        integer g_pmax_
        parameter (g_pmax_ = 4)
        integer g_i_, g_p_, ldg_t, ldg_sml, ldg_rckwrk
        real r23_b, r22_b, r21_b, r20_b, r12_b, r8_b, r17_b, r16_b, r2_v
     *, r22_v
        real r2_b, r1_p, r9_b, r13_b, g_tn1(g_pmax_), g_t(ldg_t, *), g_t
     *nlog(g_pmax_), g_tn2(g_pmax_), g_tn3(g_pmax_), g_tn4(g_pmax_)
        real g_tk1(g_pmax_), g_tklog(g_pmax_), g_tk2(g_pmax_), g_tk3(g_p
     *max_), g_tk4(g_pmax_), g_sml(ldg_sml, *), g_rckwrk(ldg_rckwrk, *)
        integer g_ehfid
        save g_tn1, g_tnlog, g_tn2, g_tn3, g_tn4, g_tk1, g_tklog, g_tk2,
     * g_tk3, g_tk4
        intrinsic real
        data tn1 /1.0/
C
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'cksml','g_cklib.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        if (t(1) .ne. tn1) then
C        FIRST of the species-specific temperature array (default)
          do g_i_ = 1, g_p_
            g_tn1(g_i_) = g_t(g_i_, 1)
          enddo
          tn1 = t(1)
C--------
          r2_v = log(tn1)
          r1_p = 1.0e0 / tn1
     
          do g_i_ = 1, g_p_
            g_tnlog(g_i_) = r1_p * g_tn1(g_i_)
          enddo
          tnlog = r2_v
C--------
          r2_b = tn1 + tn1
          do g_i_ = 1, g_p_
            g_tn2(g_i_) = r2_b * g_tn1(g_i_)
          enddo
          tn2 = tn1 * tn1
C--------
          do g_i_ = 1, g_p_
            g_tn3(g_i_) = tn2 * g_tn1(g_i_) + tn1 * g_tn2(g_i_)
          enddo
          tn3 = tn1 * tn2
C--------
          do g_i_ = 1, g_p_
            g_tn4(g_i_) = tn3 * g_tn1(g_i_) + tn1 * g_tn3(g_i_)
          enddo
          tn4 = tn1 * tn3
C--------
          r2_b = 1.0 / real(2)
          do g_i_ = 1, g_p_
            g_tn2(g_i_) = r2_b * g_tn2(g_i_)
          enddo
          tn2 = tn2 / real(2)
C--------
          r2_b = 1.0 / real(3)
          do g_i_ = 1, g_p_
            g_tn3(g_i_) = r2_b * g_tn3(g_i_)
          enddo
          tn3 = tn3 / real(3)
C--------
          r2_b = 1.0 / real(4)
          do g_i_ = 1, g_p_
            g_tn4(g_i_) = r2_b * g_tn4(g_i_)
          enddo
          tn4 = tn4 / real(4)
C--------
        endif
        nkm1 = nkk - 1
        do 99810 k = 0, nkm1
          if (ickwrk(icktf + k) .ne. 1) then
C           different temperature required by this species
            do g_i_ = 1, g_p_
              g_tk1(g_i_) = g_t(g_i_, ickwrk(icktf + k))
            enddo
            tk1 = t(ickwrk(icktf + k))
C--------
            r2_v = log(tk1)
            r1_p = 1.0e0 / tk1
     
            do g_i_ = 1, g_p_
              g_tklog(g_i_) = r1_p * g_tk1(g_i_)
            enddo
            tklog = r2_v
C--------
            r2_b = tk1 + tk1
            do g_i_ = 1, g_p_
              g_tk2(g_i_) = r2_b * g_tk1(g_i_)
            enddo
            tk2 = tk1 * tk1
C--------
            do g_i_ = 1, g_p_
              g_tk3(g_i_) = tk2 * g_tk1(g_i_) + tk1 * g_tk2(g_i_)
            enddo
            tk3 = tk1 * tk2
C--------
            do g_i_ = 1, g_p_
              g_tk4(g_i_) = tk3 * g_tk1(g_i_) + tk1 * g_tk3(g_i_)
            enddo
            tk4 = tk1 * tk3
C--------
            r2_b = 1.0 / real(2)
            do g_i_ = 1, g_p_
              g_tk2(g_i_) = r2_b * g_tk2(g_i_)
            enddo
            tk2 = tk2 / real(2)
C--------
            r2_b = 1.0 / real(3)
            do g_i_ = 1, g_p_
              g_tk3(g_i_) = r2_b * g_tk3(g_i_)
            enddo
            tk3 = tk3 / real(3)
C--------
            r2_b = 1.0 / real(4)
            do g_i_ = 1, g_p_
              g_tk4(g_i_) = r2_b * g_tk4(g_i_)
            enddo
            tk4 = tk4 / real(4)
C--------
          else
            do g_i_ = 1, g_p_
              g_tk1(g_i_) = g_tn1(g_i_)
            enddo
            tk1 = tn1
C--------
            do g_i_ = 1, g_p_
              g_tklog(g_i_) = g_tnlog(g_i_)
            enddo
            tklog = tnlog
C--------
            do g_i_ = 1, g_p_
              g_tk2(g_i_) = g_tn2(g_i_)
            enddo
            tk2 = tn2
C--------
            do g_i_ = 1, g_p_
              g_tk3(g_i_) = g_tn3(g_i_)
            enddo
            tk3 = tn3
C--------
            do g_i_ = 1, g_p_
              g_tk4(g_i_) = g_tn4(g_i_)
            enddo
            tk4 = tn4
C--------
          endif
C
C        number of temperature ranges for this species
          ntr = ickwrk(icnt + k) - 1
C        location of FIRST set of thermodynamic fit coefficients
          na1 = ncaa + k * ncp2t
C        location of upper limit of FIRST temperature range
          ktemp = nctt + k * mxtp + 1
C
200       continue
          if (ntr .gt. 1 .and. tk1 .gt. rckwrk(ktemp)) then
C           Remaining number of temperature ranges
            ntr = ntr - 1
C           Location of next set of fit coefficients
            na1 = na1 + ncp2
            ktemp = ktemp + 1
C           Check against next temperature, unless last
            if (ntr .gt. 1) then
              goto 200
            endif
          endif
C
          r22_v = rckwrk(na1) * tklog + rckwrk(na1 + 1) * tk1 + rckwrk(n
     *a1 + 2) * tk2 + rckwrk(na1 + 3) * tk3 + rckwrk(na1 + 4) * tk4 + rc
     *kwrk(na1 + ncp2 - 1)
          r8_b = rckwrk(ncru) * tk4
          r9_b = rckwrk(ncru) * rckwrk(na1 + 4)
          r12_b = rckwrk(ncru) * tk3
          r13_b = rckwrk(ncru) * rckwrk(na1 + 3)
          r16_b = rckwrk(ncru) * tk2
          r17_b = rckwrk(ncru) * rckwrk(na1 + 2)
          r20_b = rckwrk(ncru) * tk1
          r21_b = rckwrk(ncru) * rckwrk(na1 + 1)
          r22_b = rckwrk(ncru) * tklog
          r23_b = rckwrk(ncru) * rckwrk(na1)
          do g_i_ = 1, g_p_
            g_sml(g_i_, k + 1) = r22_v * g_rckwrk(g_i_, ncru) + r22_b * 
     *g_rckwrk(g_i_, na1) + r23_b * g_tklog(g_i_) + r20_b * g_rckwrk(g_i
     *_, na1 + 1) + r21_b * g_tk1(g_i_) + r16_b * g_rckwrk(g_i_, na1 + 2
     *) + r17_b * g_tk2(g_i_) + r12_b * g_rckwrk(g_i_, na1 + 3) + r13_b 
     ** g_tk3(g_i_) + r8_b * g_rckwrk(g_i_, na1 + 4) + r9_b * g_tk4(g_i_
     *) + rckwrk(ncru) * g_rckwrk(g_i_, na1 + ncp2 - 1)
          enddo
          sml(k + 1) = rckwrk(ncru) * r22_v
C--------
250       continue
99810   continue
C
C     end of SUBROUTINE CKSML
        return
      end
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cksms' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cksnum' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cksor' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cksubs' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cksyme' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cksymr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'cksyms' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckthb' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckubml' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckubms' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckuml' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckums' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckwc' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckwl' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckwt' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckwxp' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckwxr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckwyp' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckwypk' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckwyr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckxnum' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckxtcp' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckxtcr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckxty' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      subroutine g_ckytcp(g_p_, p, g_p, ldg_p, t, g_t, ldg_t, y, g_y, ld
     *g_y, ickwrk, rckwrk, g_rckwrk, ldg_rckwrk, c, g_c, ldg_c)
C
C  START PROLOGUE
C
C  SUBROUTINE CKYTCP (P, T, Y, ICKWRK, RCKWRK, C)
C  Returns the molar concentrations given pressure, temperature(s)
C  and mass fractions;  see Eq. (7).
C
C  INPUT
C  P         - Real scalar, pressure.
C                 cgs units, dynes/cm**2
C  T(*)      - Real array, temperature(s); dimension is determined by
C              the application program to be the total number of
C              species temperatures, nominally 1.
C                 cgs units, K
C  Y(*)      - Real array, mass fractions of the mixture;
C              dimension at least KK, the total species count.
C  ICKWRK(*) - Integer workspace array; dimension at least LENICK.
C  RCKWRK(*) - Real    workspace array; dimension at least LENRCK.
C
C  OUTPUT
C  C(*)      - Real array, concentrations of the species;
C              dimension at least KK, the total species count.
C                 cgs units, mole/cm**3
C
C  END PROLOGUE
C*****precision > double
        implicit real (a-h, o-z), integer (i-n)
C*****END precision > double
C*****precision > single
C      IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
C       Start of include file D:/DNS/ARK_Release/AUTOGETRATES/run/ckstrt.h
C       CVS Revision:$Revision: 1.1.1.1 $  created $Date: 2005/01/11 00:27:29 $
C     this is CHEMKIN-III file ckstrt.h V.3.1 December 2000;
C     it contains pointers for the gas-phase kinetics
C       subroutines' data storage arrays
C
C
C     include file for CHEMKIN-III cklib.f, dated: December 8, 2000
C
        integer nmm, nkk, nii, mxsp, mxtb, mxtp, ncp, ncp1, ncp2, ncp2t,
     * npar, nlar, nfar, nlan, nfal, nrev, nthb, nrlt, nwl, neim, njan, 
     *njar, nft1, nf1r, nexc, nmom, nxsm, ntde, nrnu, nord, mxord, kel, 
     *nkki, icmm, ickk, icnc, icph, icch, icnt, icnu, icnk, icnsu, icns,
     * icnr, iclt, icrl, icrv, icwl, icfl, icfo, icft, ickf, ictb, ickn,
     * ickt, icei, icet, icjn, icf1, icex, icmo, icmk, icxs, icxi, icxk,
     * ictd, ictk, icrnu, icord, ickor, icki, icktf, ick1, ick2, ncaw, n
     *cwt, nctt, ncaa, ncco, ncrv, nclt, ncrl, ncfl, nckt, ncwl, ncjn, n
     *cf1, ncex, ncru, ncrc, ncpa, ncavo, ncper, ncech, ncbol, ncpi, nck
     *f, nckr, ncrnu, ncrsu, nckor, nck1, nck2, nck3, nck4, nci1, nci2, 
     *nci3, nci4
C
C
        common /ckstrt/ nmm, nkk, nii, mxsp, mxtb, mxtp, ncp, ncp1, ncp2
     *, ncp2t, npar, nlar, nfar, nlan, nfal, nrev, nthb, nrlt, nwl, neim
     *, njan, njar, nft1, nf1r, nexc, nmom, nxsm, ntde, nrnu, nord, mxor
     *d, kel, nkki, icmm, ickk, icnc, icph, icch, icnt, icnu, icnk, icns
     *u, icns, icnr, iclt, icrl, icrv, icwl, icfl, icfo, icft, ickf, ict
     *b, ickn, ickt, icei, icet, icjn, icf1, icex, icmo, icmk, icxs, icx
     *i, icxk, ictd, ictk, icrnu, icord, ickor, icki, icktf, ick1, ick2,
     * ncaw, ncwt, nctt, ncaa, ncco, ncrv, nclt, ncrl, ncfl, nckt, ncwl,
     * ncjn, ncf1, ncex, ncru, ncrc, ncpa, ncavo, ncper, ncech, ncbol, n
     *cpi, nckf, nckr, ncrnu, ncrsu, nckor, nck1, nck2, nck3, nck4, nci1
     *, nci2, nci3, nci4
C
C     Integer constants
C
C        0     1     2     3     4     5     6     7     8     9
C        10    11    12    13    14    15    16    17    18    19   
C        20    21    22    23    24    25    26    27    28    29
C        30     31    32
C
C     Integer pointers to character arrays in CCKWRK
C
C        33    34
C
C     Integer pointers to integer arrays in ICKWRK
C
C        35    36    37    38    39    40    41    42    43    44     
C        45    46    47    48    49    50    51    52    53    54
C        55    56    57    58    59    60    61    62    63    64    
C        65    66    67    68    69    70    71    72    73   
C
C     Integer pointers to real variables and arrays in RCKWRK
C
C        74    75    76    77    78    79    80    81    82    83    
C        84    85    86    87    88    89    90    91     92
C        93     94     95    96    97    98 
C        99    100   101   102   103   104   105   106   107   108
C
C
C Logical global variables
C
C      NPERT = TRUE if any of the perturbation factors for the rate
C              constants have been changed from the value of one.
C
        logical lpert
        common /ckglbl/ lpert
C
C     END include file for cklib.f
C
        dimension t(*), y(*), ickwrk(*), rckwrk(*), c(*)
C
        integer g_pmax_
        parameter (g_pmax_ = 4)
        integer g_i_, g_p_, ldg_y, ldg_t, ldg_rckwrk, ldg_c, ldg_p
        real r7_b, r6_b, r5_b, r4_b, r3_b, r2_b, r6_v, r7_v, g_sumyow(g_
     *pmax_), g_y(ldg_y, *)
        real g_t(ldg_t, *), g_rckwrk(ldg_rckwrk, *), g_c(ldg_c, *), g_p(
     *ldg_p)
        integer g_ehfid
        save g_sumyow
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'ckytcp','g_cklib.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        do g_i_ = 1, g_p_
          g_sumyow(g_i_) = 0.0
        enddo
        sumyow = 0.0
C--------
        nkm1 = nkk - 1
        do 99782 k = 0, nkm1
          r6_v = y(k + 1) * t(ickwrk(icktf + k)) / rckwrk(ncwt + k)
          r4_b = 1.0 / rckwrk(ncwt + k)
          r5_b = (-r6_v) / rckwrk(ncwt + k)
          r6_b = r4_b * t(ickwrk(icktf + k))
          r7_b = r4_b * y(k + 1)
          do g_i_ = 1, g_p_
            g_sumyow(g_i_) = g_sumyow(g_i_) + r6_b * g_y(g_i_, k + 1) + 
     *r7_b * g_t(g_i_, ickwrk(icktf + k)) + r5_b * g_rckwrk(g_i_, ncwt +
     * k)
          enddo
          sumyow = sumyow + r6_v
C--------
150       continue
99782   continue
        do g_i_ = 1, g_p_
          g_sumyow(g_i_) = rckwrk(ncru) * g_sumyow(g_i_) + sumyow * g_rc
     *kwrk(g_i_, ncru)
        enddo
        sumyow = sumyow * rckwrk(ncru)
C--------
        do 99781 k = 1, nkk
          r6_v = sumyow * rckwrk(ncwt + k - 1)
          r7_v = p * y(k) / r6_v
          r2_b = 1.0 / r6_v
          r3_b = (-r7_v) / r6_v
          r4_b = r3_b * rckwrk(ncwt + k - 1)
          r5_b = r3_b * sumyow
          r6_b = r2_b * y(k)
          r7_b = r2_b * p
          do g_i_ = 1, g_p_
            g_c(g_i_, k) = r6_b * g_p(g_i_) + r7_b * g_y(g_i_, k) + r4_b
     * * g_sumyow(g_i_) + r5_b * g_rckwrk(g_i_, ncwt + k - 1)
          enddo
          c(k) = r7_v
C--------
200       continue
99781   continue
C
C     end of SUBROUTINE CKYTCP
        return
      end
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckytcr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckytx' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'pkindx' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckpari' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
CInactive procedure or block data 'ckparr' excluded from output
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
C     OBSOLETE ROUTINES...
C
CInactive procedure or block data 'upcase' excluded from output
C
CInactive procedure or block data 'locase' excluded from output
C
CInactive procedure or block data 'ipplen' excluded from output
C
CInactive procedure or block data 'ilasch' excluded from output
C
CInactive procedure or block data 'ifirch' excluded from output
C
CInactive procedure or block data 'ipparr' excluded from output
C
CInactive procedure or block data 'ippari' excluded from output

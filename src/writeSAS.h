/*******
 *
 *    writeSAS.h: Routines for writing SAS XPT formatted files
 *
 *    Author:  Gregory R. Warnes <greg@warnes.net>
 *
 *    Copyright (C) 2007  Gregory R. Warnes <greg@warnes.net>
 *
 *    This program is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *    02110-1301, USA
 *
 *******/


#ifndef WRITESAS_H
#define WRITESAS_H

#include <R.h>
#include <Rinternals.h>
#include <sys/types.h>
#include "to_bigend.h"
#include "ibm2ieee.h"


/*****
 * Useful constants
 *****/

#define MISSING 0x2e000000  /* Standard SAS missing value: '.' */

/*****
 * Useful macro functions
 *****/

#define MIN(x,y)  (x>y?y:x)
#ifdef DO_TEST
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) if(!(x)) error("Assertion failed: x")
#endif

/*****
 *  File Record Structures
 *****/

struct FILE_HEADER {
  /* Line 1 */
  char h1[80];

  /* Line 2 */
  char sas_symbol1[8];
  char sas_symbol2[8];
  char saslib[8];
  char sasver[8];
  char sas_os[8];
  char blanks[24];
  char sas_create[16];

  /* Line 3 */
  char sas_modified[16];
  char blanks2[64];
};

struct MEMBER_HEADER {
  /* Line 1 */
  char l1[80];

  /* Line 2 */
  char l2[80];

  /* Line 3 */
  char sas_symbol[8];
  char sas_dsname[32];
  char sasdata[8];
  char sasver[8];
  char sas_osname[8];
  //char blanks[24];
  char sas_create[16];

  /* Line 4 */
  char sas_modified[16];
  //char blanks2[64];
  char padding[16];
  char dslabel[40];
  char dstype[8];
};


struct NAMESTR_HEADER {
  char l1[54];
  char nvar[6];
  char zeros[20];
};


struct NAMESTR_RECORD {
  short   ntype;              /* VARIABLE TYPE: 1=NUMERIC, 2=CHAR    */
  short   nhfun;              /* HASH OF NNAME (always 0)            */
  short   nlng;               /* LENGTH OF VARIABLE IN OBSERVATION   */
  short   nvar0;              /* VARNUM                              */
  char    nname[8];           /* NAME OF VARIABLE                    */
  char    nlabel[40];         /* LABEL OF VARIABLE                   */

  char    nform[8];           /* NAME OF FORMAT                      */
  short   nfl;                /* FORMAT FIELD LENGTH OR 0            */
  short   nfd;                /* FORMAT NUMBER OF DECIMALS           */
  short   nfj;                /* 0=LEFT JUSTIFICATION, 1=RIGHT JUST  */

  char    nfill[2];           /* (UNUSED, FOR ALIGNMENT AND FUTURE)  */

  char    niform[8];          /* NAME OF INPUT FORMAT                */
  short   nifl;               /* INFORMAT LENGTH ATTRIBUTE           */
  short   nifd;               /* INFORMAT NUMBER OF DECIMALS         */

  int     npos;               /* POSITION OF VALUE IN OBSERVATION    */

  char    nlongname[32];      /* long name for Version 8- style      */
  short   nlablen;            /* length of label                     */
  char    rest[18];           /* remaining fields are irrelevant     */
};

struct LABEL8_HEADER {
  char l1[48];
  char nvar[15];
  char l3[17];
};

struct LABEL8_RECORD {
  
  
  short   varNum;              /* VARNUM                               */
  short   namLen;               /* LENGTH OF LABEL IN BYTES             */
  short   labLen;               /* LENGTH OF VARIABLE NAME IN BYTES     */
  char    varName[74];        /* LABEL OF VARIABLE  IN BYTES          */
  
};

struct OBS_HEADER {
  char l1[48];
  char nvar[15];
  char l3[17];
};

/*****
 * Function Prototypes
 *****/

void blankFill(char *target, int len);
void blankCopy(char *target, int len, char *source);

void zeroFill(char *target, int len);
void zeroCopy(char *target, int len, char *source);

void fill_file_header(char **cDate, char **mDate, char **sasVer, char **osType);
void fill_member_header(char **dfName, char **sasVer, char **osType, char **cDate,
			char **mDate, char **dfLabel, char **dfType);

void fill_namestr(int  *isChar, int  *nlng, int  *nvar0, char **nname, char **nlabel,
		  char **nform, int  *nfl, int  *nfd, int  *nfj, char **niform,
		  int  *nifl, int  *nifd, int  *npos, int *nlablen);

void fill_namestr_header(char **nvar);


void fill_label8(char **varName, int *varNum, int *namLen, int  *labLen, int  *totlen);

void fill_label8_header(char **nvar);

void fill_obs_header(char **nvar);

void fill_numeric_field(double *value);
void fill_character_field(char **value, int *width);

void fill_numeric_NA();
void fill_space(int *type, int *width);

SEXP getRawBuffer();

void doTest();

void ieee2ibm(register unsigned char *out, register const unsigned char *in, int count);

#endif /* WRITESAS_H */

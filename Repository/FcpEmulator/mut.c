/* $Header: /home/bill/Repository/FcpEmulator/mut.c,v 1.1.1.1 1999/07/01 07:15:10 bill Exp $ */

#include	<stdio.h>
#include	<string.h>
#include	<sys/file.h>

#include	"fcp.h"

#define MaxLineVals	8

static char UnifyTable[(TagBits+1)][(TagBits+1)];

main()
{
  FILE *FileFd = fopen("unify.h", "w");
  register int I, J, K;

  if (FileFd == NULL) {
    printf("Can not open \"unify.h\" for writing\n");
    exit(1);
  }
  /* fill table with codes */

  /* {IntTag, RealTag, StrTag, NilTag, ListTag, TplTag, VctrTag, InvldTag},
     default */
  for (I = 0; I < (TagBits+1); I++) {
    for (J = 0; J < (TagBits+1); J++) {
      UnifyTable[I][J] = 0x00;
    }
  }

  /* WrtTag, WrtTag */
  UnifyTable[WrtTag][WrtTag] = 0x01;

  /* WrtTag, {IntTag, NilTag} */
  UnifyTable[WrtTag][IntTag] = 0x02;
  UnifyTable[WrtTag][NilTag] = 0x02;

  /* WrtTag, {RoTag, RealTag, StrTag, ListTag, TplTag, VctrTag, InvldTag} */
  UnifyTable[WrtTag][RoTag] = 0x03;
  UnifyTable[WrtTag][RealTag] = 0x03;
  UnifyTable[WrtTag][StrTag] = 0x03;
  for (I = 0x00; I <= 0x0f; I++) {
    UnifyTable[WrtTag][Tag(I, L_RefFlag)] = 0x03;
  }
  UnifyTable[WrtTag][L_IntTag] = 0x03;
  UnifyTable[WrtTag][L_NilTag] = 0x03;
  UnifyTable[WrtTag][TplTag] = 0x03;
  UnifyTable[WrtTag][VctrTag] = 0x03;
  UnifyTable[WrtTag][InvldTag] = 0x03;

  /* {RoTag, RealTag, StrTag, ListTag, TplTag, VctrTag, InvldTag}, WrtTag */
  UnifyTable[RoTag][WrtTag] = 0x04;
  UnifyTable[RealTag][WrtTag] = 0x04;
  UnifyTable[StrTag][WrtTag] = 0x04;
  for (I = 0x00; I <= 0x0f; I++) {
    UnifyTable[Tag(I, L_RefFlag)][WrtTag] = 0x04;
  }
  UnifyTable[L_IntTag][WrtTag] = 0x04;
  UnifyTable[L_NilTag][WrtTag] = 0x04;
  UnifyTable[TplTag][WrtTag] = 0x04;
  UnifyTable[VctrTag][WrtTag] = 0x04;
  UnifyTable[InvldTag][WrtTag] = 0x04;

  /* RoTag, RoTag */
  UnifyTable[RoTag][RoTag] = 0x05;

  /* RoTag, {IntTag, RealTag, StrTag, NilTag, ListTag, TplTag, VctrTag,
     InvldTag} */
  UnifyTable[RoTag][IntTag] = 0x06;
  UnifyTable[RoTag][RealTag] = 0x06;
  UnifyTable[RoTag][StrTag] = 0x06;
  UnifyTable[RoTag][NilTag] = 0x06;
  for (I = 0x00; I <= 0x0f; I++) {
    UnifyTable[RoTag][Tag(I, L_RefFlag)] = 0x06;
  }
  UnifyTable[RoTag][L_IntTag] = 0x06;
  UnifyTable[RoTag][L_NilTag] = 0x06;
  UnifyTable[RoTag][TplTag] = 0x06;
  UnifyTable[RoTag][VctrTag] = 0x06;
  UnifyTable[RoTag][InvldTag] = 0x06;

  /* {IntTag, NilTag}, WrtTag */
  UnifyTable[IntTag][WrtTag] = 0x07;
  UnifyTable[NilTag][WrtTag] = 0x07;

  /* {IntTag, RealTag, StrTag, NilTag, ListTag, TplTag, VctrTag, InvldTag},
     RoTag */
  UnifyTable[IntTag][RoTag] = 0x08;
  UnifyTable[RealTag][RoTag] = 0x08;
  UnifyTable[StrTag][RoTag] = 0x08;
  UnifyTable[NilTag][RoTag] = 0x08;
  for (I = 0x00; I <= 0x0f; I++) {
    UnifyTable[Tag(I, L_RefFlag)][RoTag] = 0x08;
  }
  UnifyTable[L_IntTag][RoTag] = 0x08;
  UnifyTable[L_NilTag][RoTag] = 0x08;
  UnifyTable[TplTag][RoTag] = 0x08;
  UnifyTable[VctrTag][RoTag] = 0x08;
  UnifyTable[InvldTag][RoTag] = 0x08;

  /* IntTag, IntTag */
  UnifyTable[IntTag][IntTag] = 0x09;

  /* RealTag, RealTag */
  UnifyTable[RealTag][RealTag] = 0x0a;

  /* StrTag, StrTag */
  UnifyTable[StrTag][StrTag] = 0x0b;

  /* NilTag, NilTag */
  UnifyTable[NilTag][NilTag] = 0x0c;

  /* ListTag, ListTag */
  for (I = 0x00; I <= 0x0f; I++) {
    for (J = 0x00; J <= 0x0f; J++) {
      UnifyTable[Tag(I, L_RefFlag)][Tag(J, L_RefFlag)] = 0x0d;
    }
  }
  for (I = 0x00; I <= 0x0f; I++) {
    UnifyTable[Tag(I, L_RefFlag)][L_IntTag] = 0x0d;
  }
  for (I = 0x00; I <= 0x0f; I++) {
    UnifyTable[Tag(I, L_RefFlag)][L_NilTag] = 0x0d;
  }
  for (I = 0x00; I <= 0x0f; I++) {
    UnifyTable[L_IntTag][Tag(I, L_RefFlag)] = 0x0d;
  }
  UnifyTable[L_IntTag][L_IntTag] = 0x0d;
  for (I = 0x00; I <= 0x0f; I++) {
    UnifyTable[L_NilTag][Tag(I, L_RefFlag)] = 0x0d;
  }
  UnifyTable[L_NilTag][L_NilTag] = 0x0d;

  /* TplTag, TplTag */
  UnifyTable[TplTag][TplTag] = 0x0e;

  fprintf(FileFd, "static char UnifyTable[(TagBits+1)][(TagBits+1)] = {\n  ");
  K = 0;
  for (I = 0; I < (TagBits+1); I++) {
    fprintf(FileFd, "{");
    for (J = 0; J < (TagBits+1); J++) {
      if (K == MaxLineVals) {
	fprintf(FileFd, "\n   ");
	K = 0;
      }
      fprintf(FileFd, "0x%.2x", UnifyTable[I][J]);
      if (J != TagBits) {
	fprintf(FileFd, ", ");
      }
      K++;
    }
    fprintf(FileFd, "}");
    if (I != TagBits) {
      fprintf(FileFd, ",\n");
    }
    fprintf(FileFd, "\n");
    if (I != TagBits) {
      fprintf(FileFd, "  ");
    }
    K = 0;
  }
  fprintf(FileFd, "};\n");
  close(FileFd);
}

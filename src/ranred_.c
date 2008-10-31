/*---------------------------------------------------------------------------*/
/* Version 28-February-1997                                  File: ranred_.c */
/* @ ncl & fjg                                                               */
/*---------------------------------------------------------------------------*/
/*Comment                                                                    */
/*                                                                           */
/* float ranred_(int *nseed)                                                 */
/*                                                                           */
/* input: nseed                                                              */
/* output: ranred_ (function)                                                */
/*                                                                           */
/* Return a random number in the range [0,1). If nseed<0 a previous call     */
/* to srandom is also performed.                                             */
/*                                                                           */
/* int *nseed -> determines whether a new seed for the sequence of           */
/*               pseudo-random integers is initialized                       */
/*                                                                           */
/*Comment                                                                    */
/*---------------------------------------------------------------------------*/
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

float ranred_(int *nseed)
{
  int numrandom;
  double fnum;

  if(*nseed<0){
    srandom((unsigned int)time(NULL)/2);   /* Inicio del generador random */
    *nseed=0;
  }  /* ya no hace falta volver a inicializar la semilla */

  numrandom=2147483647;
  while (numrandom >= 2147483647)
    numrandom=random();
  fnum=(double)numrandom;
  fnum=fnum/(2147483647);
  return((float)fnum); /* retorna un numero real en el intervalo [0,1) */
}

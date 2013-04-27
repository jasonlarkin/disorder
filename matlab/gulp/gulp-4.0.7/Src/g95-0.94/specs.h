/* Definitions for specs for g95
   Copyright (C) 2004 Free Software Foundation, Inc.

   This file is licensed under the GPL.
*/

/* spec for options when invoking f951 */

#define F951_OPTIONS "f951 %i %(cc1_options)" G95_SPEC " %{E} %{cpp} \
  %{no-cpp} %{bundle} %{I*} %{M} %{r*} %{i*} %{D*} %{U*} \
  %{traditional} %{nontraditional} %{!fsyntax-only:%{!E:%(invoke_as)}}"


/* This is the contribution to the `default_compilers' array in gcc.c for
 * the fortran 95 language. */

  { ".f90",   "@f95-free",  0 },
  { ".f95",   "@f95-free",  0 },
  { ".f03",   "@f95-free",  0 },

  { ".F90",   "@f95-free", 0 },
  { ".F95",   "@f95-free", 0 },
  { ".F03",   "@f95-free", 0 },

  { "@f95-free", F951_OPTIONS }, 

  { ".f",     "@f95-fixed", 0 },
  { ".for",   "@f95-fixed", 0 },

  { ".F",     "@f95-fixed", 0 },
  { ".FOR",   "@f95-fixed", 0 },

  { ".fpp",   "@f95-fixed", 0 },
  { ".FPP",   "@f95-fixed", 0 },

  { "@f95-fixed", F951_OPTIONS },

  {"-",       "@f95-free",  0 },

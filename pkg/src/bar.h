/*
 *  bar.h
 *  tmp
 *
 *  Created by Roland Kaiser on 24.10.12.
 *  Copyright 2012 Universität Salzburg. All rights reserved.
 *
 */

#ifndef R_SP_H
#define R_SP_H

#ifdef SP_XPORT
# define SP_PREFIX(name) SP_XPORT(name)
#else
# define SP_PREFIX(name) name
#endif

void SP_PREFIX(test)(char** buf, int nps);

#endif
/* remember to touch local_stubs.c */


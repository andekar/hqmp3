/*
 * imdct.c - naive IDMCT implementation. This can be made O(n log n),
 * but as is the rest of the decoder is too slow for this to matter
 * we ignore it.
 *
 * This code is part of the Experimental Haskell MP3 Decoder, version 0.0.1.
 * Copyright (c) 2008 Bjorn Edstrom <be@bjrn.se>
 *
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 *   1. The origin of this software must not be misrepresented; you must not
 *   claim that you wrote the original software. If you use this software
 *   in a product, an acknowledgment in the product documentation would be
 *   appreciated but is not required.
 *
 *   2. Altered source versions must be plainly marked as such, and must not be
 *   misrepresented as being the original software.
 *
 *   3. This notice may not be removed or altered from any source
 *   distribution.
 */

#include <math.h>

#define PI 3.1415926535897931

void imdct18(double *in, double *out)
{
    int k, n;
    double sum;
    static double lookup[36][18];
    static int lookup_init = 0;

    if (lookup_init == 0) {
        
        for (n = 0; n < 36; n++)
            for (k = 0; k < 18; k++)
                lookup[n][k] = cos((PI/18) * (n + 0.5 + 18/2.0) * (k + 0.5));

        lookup_init = 1;
    }

    for (n = 0; n < 36; n++) {
        sum = 0.0;
        for (k = 0; k < 18; k+=3) {
            sum += in[k+0] * lookup[n][k+0];
            sum += in[k+1] * lookup[n][k+1];
            sum += in[k+2] * lookup[n][k+2];
        }
        out[n] = sum;
    }
}

void imdct(int points, double *in, double *out)
{
    int k, n;
    double sum;

    if (points == 18) {
        imdct18(in, out);
        return;
    }

    for (n = 0; n < points*2; n++) {
        sum = 0.0;
        for (k = 0; k < points; k++) 
            sum += in[k] * cos((PI/points) * (n + 0.5 + points/2.0) * (k + 0.5));
        out[n] = sum;
    }
}


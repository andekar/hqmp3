
#ifndef MP3SYNTH_H
#define MP3SYNTH_H 1

#ifdef __cplusplus
extern "C" {
#endif

void synth(double state[1024], double samples[576],
           double newstate[1024], double output[576]);

#ifdef __cplusplus
}
#endif

#endif


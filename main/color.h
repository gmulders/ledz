#include <math.h>
#include "freertos/FreeRTOS.h"

#ifndef _COLOR_H
#define _COLOR_H

typedef union {
  struct __attribute__ ((packed)) {
    uint8_t r, g, b, w;  // Little-endian ordered
  };
  uint32_t raw32;
} color_t;

color_t color_from_rgbw(uint8_t r, uint8_t g, uint8_t b, uint8_t w);
color_t hsv2rgb(float hue, float sat, float val);

#endif // _COLOR_H

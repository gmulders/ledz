#include <math.h>
#include "color.h"

color_t color_from_rgbw(uint8_t r, uint8_t g, uint8_t b, uint8_t w)
{
  color_t v;
  v.r = r;
  v.g = g;
  v.b = b;
  v.w = w;
  return v;
}

color_t hsv2rgb(float hue, float sat, float val)
{
  float red = 0, grn = 0, blu = 0;
  float i, f, p, q, t;

  if(val == 0) {
    red = 0;
    grn = 0;
    blu = 0;
  } else {
    hue/=60;
    i = floor(hue);
    f = hue-i;
    p = val*(1-sat);
    q = val*(1-(sat*f));
    t = val*(1-(sat*(1-f)));
    if (i==0) {red=val; grn=t; blu=p;}
    else if (i==1) {red=q; grn=val; blu=p;}
    else if (i==2) {red=p; grn=val; blu=t;}
    else if (i==3) {red=p; grn=q; blu=val;}
    else if (i==4) {red=t; grn=p; blu=val;}
    else if (i==5) {red=val; grn=p; blu=q;}
  }

  return color_from_rgbw(red * 255, grn * 255, blu * 255, 0);
}

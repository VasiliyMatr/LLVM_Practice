#include <cstdio>
#include <iostream>

#include <wrap.hpp>

using FixedPoint = int;

static int FIXED_POINT_POS = 16;
static FixedPoint FIXED_POINT_1 = 1 << FIXED_POINT_POS;

FixedPoint mulFixed(FixedPoint a, FixedPoint b) {
    return ((long long)a * (long long)b) >> FIXED_POINT_POS;
}

FixedPoint complMulRe(FixedPoint aRe, FixedPoint aIm, FixedPoint bRe, FixedPoint bIm) {
    return mulFixed(aRe, bRe) - mulFixed(aIm, bIm);
}

FixedPoint complMulIm(FixedPoint aRe, FixedPoint aIm, FixedPoint bRe, FixedPoint bIm) {
    return mulFixed(aRe, bIm) + mulFixed(aIm, bRe);
}

FixedPoint complSq(FixedPoint aRe, FixedPoint bIm) {
    return mulFixed(aRe, aRe) + mulFixed(bIm, bIm);
}

FixedPoint calcMandIterRe(FixedPoint zRe, FixedPoint zIm, FixedPoint cRe, FixedPoint cIm) {
    return complMulRe(zRe, zIm, zRe, zIm) + cRe;
}

FixedPoint calcMandIterIm(FixedPoint zRe, FixedPoint zIm, FixedPoint cRe, FixedPoint cIm) {
    return complMulIm(zRe, zIm, zRe, zIm) + cIm;
}

int main() {
    wrap_openWindow();

    int max_zoom_level = 20;

    FixedPoint x_center = -FIXED_POINT_1 * 7267 / 10000;
    FixedPoint y_center = FIXED_POINT_1 * 25748 / 100000;

    while (1) {
        FixedPoint x_range_len = FIXED_POINT_1 * 3;
        FixedPoint y_range_len = FIXED_POINT_1 * 3;

        for (int zoom_level = 0; zoom_level < max_zoom_level; ++zoom_level) {
            FixedPoint x_min = x_center - x_range_len / 2;
            FixedPoint y_min = y_center - y_range_len / 2;

            for (int x = 0; x < wrap_X_WINDOW_SIZE; ++x) {
                FixedPoint cRe = x * x_range_len / wrap_X_WINDOW_SIZE + x_min;
                for (int y = 0; y < wrap_Y_WINDOW_SIZE; ++y) {
                    FixedPoint cIm = y * y_range_len / wrap_Y_WINDOW_SIZE + y_min;

                    FixedPoint zRe = 0;
                    FixedPoint zIm = 0;

                    int i = 0;
                    for (; i < 256; ++i) {
                        if (complSq(zRe, zIm) > FIXED_POINT_1 * 4) {
                            break;
                        }

                        FixedPoint nextZRe = calcMandIterRe(zRe, zIm, cRe, cIm);
                        FixedPoint nextZIm = calcMandIterIm(zRe, zIm, cRe, cIm);

                        zRe = nextZRe;
                        zIm = nextZIm;
                    }

                    wrap_windowSetPixel(x, y, i, i, i);
                }
            }

            wrap_windowUpdate();

            x_range_len = x_range_len * 3 / 4;
            y_range_len = x_range_len * 3 / 4;
        }
    }

    return 0;
}

// Simple C API for 2D graphics

#ifndef INCL_WRAP_HPP
#define INCL_WRAP_HPP

static_assert(sizeof(int) == 4);
static_assert(sizeof(long long) == 8);

extern "C" {

/// Graphics
///

constexpr int wrap_X_WINDOW_SIZE = 400;
constexpr int wrap_Y_WINDOW_SIZE = 400;

void wrap_openWindow();
void wrap_windowSetPixel(int x, int y, int red, int green, int blue);
void wrap_windowUpdate();

} // extern "C"

#endif // INCL_WRAP_HPP

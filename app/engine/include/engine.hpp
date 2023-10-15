// Simple C API for 2D graphics

#ifndef ENGINE_HPP
#define ENGINE_HPP

static_assert(sizeof(int) == 4);
static_assert(sizeof(long long) == 8);

extern "C" {

/// Graphics
///

constexpr int engine_X_WINDOW_SIZE = 400;
constexpr int engine_Y_WINDOW_SIZE = 400;

// Implemented by lib user
void app();

void engine_windowSetPixel(int x, int y, int red, int green, int blue);
void engine_windowUpdate();

} // extern "C"

#endif // ENGINE_HPP

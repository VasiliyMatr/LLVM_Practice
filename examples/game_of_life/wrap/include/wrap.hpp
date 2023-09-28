// Simple C API for 2D graphics and random

#ifndef INCL_WRAP_HPP
#define INCL_WRAP_HPP

#include <cstdint>
#include <random>

using std::size_t;

extern "C" {

/// Random
///

// Generate random number.
// Generator seed is set with non-deterministic random number generator.
// Thus, generated sequence differs from run to run.
uint64_t wrap_rand();

/// Graphics
///

// Graphic window configuration
struct wrap_WindowConfig {
    const char *window_name = "";

    std::size_t x_size = 0;
    std::size_t y_size = 0;
};

// Graphic window
struct wrap_Window;

// RGB color
struct wrap_Color {
    uint8_t red = 0;
    uint8_t green = 0;
    uint8_t blue = 0;
};

// Create new graphic window.
// Return pointer to created window.
// Window must be deleted with deleteWindow function.
wrap_Window *wrap_newWindow(wrap_WindowConfig config);

// Delete graphic window
void wrap_deleteWindow(wrap_Window *window_ptr);

// Set graphic window pixel color.
// Window should be updated for changes to be displayed.
void wrap_windowSetPixel(wrap_Window *window_ptr, std::size_t x, std::size_t y,
                         wrap_Color color);

// Update graphic window for changes to be displayed
void wrap_updateWindow(wrap_Window *window_ptr);

} // extern "C"

#endif // INCL_WRAP_HPP

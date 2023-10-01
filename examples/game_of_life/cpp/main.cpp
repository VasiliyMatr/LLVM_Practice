#include <wrap.hpp>

namespace {

constexpr size_t X_CELLS_NUM = 1024;
constexpr size_t Y_CELLS_NUM = 768;

constexpr size_t CELL_PIXEL_SIZE = 1;

void setCell(wrap_Window *window_ptr, size_t x_cell_idx, size_t y_cell_idx,
             wrap_Color color) {
    for (size_t x = x_cell_idx * CELL_PIXEL_SIZE, end = x + CELL_PIXEL_SIZE;
         x != end; ++x) {
        for (size_t y = y_cell_idx * CELL_PIXEL_SIZE, end = y + CELL_PIXEL_SIZE;
             y != end; ++y) {
            wrap_windowSetPixel(window_ptr, x, y, color);
        }
    }
}

constexpr int NEIGHBORHOOD_RADIUS = 1;
uint32_t countNeighboursSum(uint8_t cells[X_CELLS_NUM][Y_CELLS_NUM],
                            size_t x_cell_idx, size_t y_cell_idx) {
    size_t x_shifted_idx = x_cell_idx + X_CELLS_NUM;
    size_t y_shifted_idx = y_cell_idx + Y_CELLS_NUM;

    uint32_t neighbour_sum = 0;

    int begin = -NEIGHBORHOOD_RADIUS;
    int end = NEIGHBORHOOD_RADIUS + 1;
    for (int i = begin; i != end; ++i) {
        for (int j = begin; j != end; ++j) {
            if (j == 0 && i == 0) {
                continue;
            }

            neighbour_sum += cells[(x_shifted_idx + i) % X_CELLS_NUM]
                                  [(y_shifted_idx + j) % Y_CELLS_NUM];
        }
    }

    return neighbour_sum;
}

wrap_Color cellValueToColor(uint8_t value) {
    uint8_t red = value * 255;
    uint8_t green = value * 255;
    uint8_t blue = value * 255;

    return {red, green, blue};
}

} // namespace

int main() {
    uint8_t cells[X_CELLS_NUM][Y_CELLS_NUM]{};
    uint8_t cells_swap[X_CELLS_NUM][Y_CELLS_NUM]{};

    for (auto &row : cells) {
        for (auto &cell : row) {
            cell = wrap_rand() % 2;
        }
    }

    size_t x_pixel_size = X_CELLS_NUM * CELL_PIXEL_SIZE;
    size_t y_pixel_size = Y_CELLS_NUM * CELL_PIXEL_SIZE;

    wrap_WindowConfig cfg{"Game of life", x_pixel_size, y_pixel_size};
    auto *window_ptr = wrap_newWindow(cfg);

    while (true) {
        // Draw cells
        for (size_t x_cell_idx = 0; x_cell_idx != X_CELLS_NUM; ++x_cell_idx) {
            for (size_t y_cell_idx = 0; y_cell_idx != Y_CELLS_NUM;
                 ++y_cell_idx) {
                auto cell_value = cells[x_cell_idx][y_cell_idx];

                setCell(window_ptr, x_cell_idx, y_cell_idx,
                        cellValueToColor(cell_value));
            }
        }
        wrap_updateWindow(window_ptr);

        // Calculate new cells values
        const size_t BORN_VALUE = 3;
        const size_t DEATH_UPPER_BOUND = 3;
        const size_t DEATH_LOWER_BOUND = 2;

        for (size_t x_cell_idx = 0; x_cell_idx != X_CELLS_NUM; ++x_cell_idx) {
            for (size_t y_cell_idx = 0; y_cell_idx != Y_CELLS_NUM;
                 ++y_cell_idx) {
                auto neighbours_sum =
                    countNeighboursSum(cells, x_cell_idx, y_cell_idx);

                if (neighbours_sum == BORN_VALUE) {
                    cells_swap[x_cell_idx][y_cell_idx] = 1;
                }
                if (neighbours_sum < DEATH_LOWER_BOUND) {
                    cells_swap[x_cell_idx][y_cell_idx] = 0;
                }
                if (neighbours_sum > DEATH_UPPER_BOUND) {
                    cells_swap[x_cell_idx][y_cell_idx] = 0;
                }
            }
        }

        // Copy cells values from cells_swap
        for (size_t x_cell_idx = 0; x_cell_idx != X_CELLS_NUM; ++x_cell_idx) {
            for (size_t y_cell_idx = 0; y_cell_idx != Y_CELLS_NUM;
                 ++y_cell_idx) {

                cells[x_cell_idx][y_cell_idx] =
                    cells_swap[x_cell_idx][y_cell_idx];
            }
        }
    }

    wrap_deleteWindow(window_ptr);
    return 0;
}

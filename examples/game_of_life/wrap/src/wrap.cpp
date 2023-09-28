#include <cstdint>
#include <exception>
#include <iostream>
#include <random>

#include <SFML/Graphics.hpp>
#include <SFML/Graphics/Color.hpp>
#include <SFML/Graphics/Image.hpp>
#include <SFML/Graphics/PrimitiveType.hpp>
#include <SFML/Graphics/RenderWindow.hpp>
#include <SFML/Graphics/Sprite.hpp>
#include <SFML/Window/Window.hpp>

#include <string_view>
#include <wrap.hpp>

namespace {

sf::Color convertColor(const wrap_Color &color) {
    return {color.red, color.green, color.blue};
}

void perrAndTerminate(std::string_view error_msg) {
    std::cerr << error_msg;
    std::terminate();
}

} // namespace

extern "C" {

uint64_t wrap_rand() {
    static std::mt19937_64 mt{std::random_device{}()};
    return mt();
}

struct wrap_Window {
    sf::Image buff{};
    sf::RenderWindow render_window{};
};

wrap_Window *wrap_newWindow(wrap_WindowConfig config) {
    size_t x_size = config.x_size;
    size_t y_size = config.y_size;

    auto *window_ptr = new wrap_Window{};

    window_ptr->buff.create(x_size, y_size, sf::Color{0, 0, 0});
    window_ptr->render_window.create(sf::VideoMode(x_size, y_size),
                                     config.window_name);

    if (!window_ptr->render_window.isOpen()) {
        delete window_ptr;
        return nullptr;
    }

    return window_ptr;
}

void wrap_deleteWindow(wrap_Window *window_ptr) { delete window_ptr; }

void wrap_windowSetPixel(wrap_Window *window_ptr, size_t x, size_t y,
                         wrap_Color color) {
    if (window_ptr == nullptr) {
        perrAndTerminate("Null window pointer in wrap_windowSetPixel\n");
    }

    window_ptr->buff.setPixel(x, y, convertColor(color));
}

void wrap_updateWindow(wrap_Window *window_ptr) {
    if (window_ptr == nullptr) {
        perrAndTerminate("Null window pointer in wrap_updateWindow\n");
    }

    sf::Texture texture;
    texture.loadFromImage(window_ptr->buff);
    sf::Sprite sprite;
    sprite.setTexture(texture, true);

    window_ptr->render_window.clear();
    window_ptr->render_window.draw(sprite);
    window_ptr->render_window.display();
}

} // extern "C"

#include <cassert>
#include <cstdint>
#include <exception>
#include <iostream>
#include <random>
#include <stdexcept>
#include <string_view>

#include <SFML/Graphics.hpp>
#include <SFML/Graphics/Color.hpp>
#include <SFML/Graphics/Image.hpp>
#include <SFML/Graphics/PrimitiveType.hpp>
#include <SFML/Graphics/RenderWindow.hpp>
#include <SFML/Graphics/Sprite.hpp>
#include <SFML/Window/Window.hpp>

#include <wrap.hpp>

namespace {

sf::Color convertColor(int red, int green, int blue) {
    return {static_cast<uint8_t>(red), static_cast<uint8_t>(green),
            static_cast<uint8_t>(blue)};
}

struct Window final {
    struct OpenError final : public std::runtime_error {
        OpenError() : std::runtime_error("Error opening window.") {}
    };

  private:
    sf::Image m_buff{};
    sf::RenderWindow m_render_window{};

  public:
    Window(std::string_view name, size_t x_size, size_t y_size) {
        m_buff.create(x_size, y_size, sf::Color{});
        m_render_window.create(sf::VideoMode(x_size, y_size), name.data());

        if (!m_render_window.isOpen()) {
            throw OpenError{};
        }
    }

    void setPixel(size_t x, size_t y, sf::Color color) noexcept {
        m_buff.setPixel(x, y, color);
    }

    void update() noexcept {
        sf::Texture texture;
        texture.loadFromImage(m_buff);
        sf::Sprite sprite;
        sprite.setTexture(texture, true);

        m_render_window.clear();
        m_render_window.draw(sprite);
        m_render_window.display();
    }
};

Window *window_ptr = nullptr;

} // namespace

extern "C" {

void wrap_openWindow() {
    try {
        window_ptr = new Window{"Wrap", wrap_X_WINDOW_SIZE, wrap_Y_WINDOW_SIZE};
    } catch (Window::OpenError &error) {
        std::cerr << error.what() << std::endl;
        std::terminate();
    }
}

void wrap_windowSetPixel(int x, int y, int red, int green, int blue) {
    assert(window_ptr != nullptr);

    assert(x >= 0 && x < wrap_X_WINDOW_SIZE);
    assert(y >= 0 && y < wrap_Y_WINDOW_SIZE);

    assert(red >= 0);
    assert(green >= 0);
    assert(blue >= 0);

    window_ptr->setPixel(x, y, convertColor(red, green, blue));
}

void wrap_windowUpdate() {
    assert(window_ptr != nullptr);

    window_ptr->update();
}

} // extern "C"

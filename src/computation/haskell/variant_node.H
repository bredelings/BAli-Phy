#ifndef HASKELL_VARIANT_NODE_H
#define HASKELL_VARIANT_NODE_H

#include <ostream>
#include <type_traits>
#include <variant>

#include "util/cow-ptr.H"

namespace Haskell
{

// Shared storage for strongly-typed Haskell AST roots.
// This deliberately mirrors Hs::Type's small cow_ptr variant interface.
template <typename... Alternatives>
struct VariantNode
{
    std::variant<std::monostate, cow_ptr<Alternatives>...> node_ptr;

    bool empty() const
    {
        return std::holds_alternative<std::monostate>(node_ptr);
    }

    void clear()
    {
        node_ptr = std::monostate{};
    }

    explicit operator bool() const
    {
        return not empty();
    }

    // Print by delegating to the concrete node currently stored in the variant.
    // Empty nodes only arise from default construction during parser moves.
    std::string print() const
    {
        return std::visit(
            // Match the old expression_ref print delegation without reintroducing Object storage.
            [](const auto& ptr) -> std::string
            {
                using Ptr = std::decay_t<decltype(ptr)>;
                if constexpr (std::is_same_v<Ptr, std::monostate>)
                    return "[empty]";
                else
                    return ptr->print();
            },
            node_ptr);
    }

    // Return a pointer to the stored node when the requested type is an AST alternative.
    // Requests for non-alternatives are well-formed and simply return nullptr.
    template <typename T>
    const T* to() const
    {
        if constexpr ((std::is_same_v<T, Alternatives> || ...))
        {
            if (std::holds_alternative<cow_ptr<T>>(node_ptr))
                return std::get<cow_ptr<T>>(node_ptr).get();
        }
        return nullptr;
    }

    // Return a modifiable pointer to the stored node using cow_ptr copy-on-write.
    // Requests for non-alternatives are well-formed and simply return nullptr.
    template <typename T>
    T* to_modifiable()
    {
        if constexpr ((std::is_same_v<T, Alternatives> || ...))
        {
            if (std::holds_alternative<cow_ptr<T>>(node_ptr))
                return std::get<cow_ptr<T>>(node_ptr).modify_ptr().get();
        }
        return nullptr;
    }

    template <typename T>
    bool is_a() const
    {
        return to<T>();
    }

    template <typename T>
    const T& as_() const
    {
        return *std::get<cow_ptr<T>>(node_ptr).get_ptr();
    }

    template <typename T>
    T& as_modifiable()
    {
        return *std::get<cow_ptr<T>>(node_ptr).modify_ptr();
    }

    VariantNode& operator=(const VariantNode&) = default;
    VariantNode& operator=(VariantNode&&) noexcept = default;

    // Replace the current node with a concrete AST alternative.
    // This keeps assignment behavior close to the old expression_ref wrapper.
    template <typename T>
        requires ((std::is_same_v<std::decay_t<T>, Alternatives> || ...))
    VariantNode& operator=(T&& t)
    {
        node_ptr = make_cow_ptr<std::decay_t<T>>(std::forward<T>(t));
        return *this;
    }

    VariantNode() = default;
    VariantNode(const VariantNode&) = default;
    VariantNode(VariantNode&&) noexcept = default;

    // Construct the variant from one concrete AST alternative.
    // There are intentionally no constructors from Object or expression_ref.
    template <typename T>
        requires ((std::is_same_v<std::decay_t<T>, Alternatives> || ...))
    VariantNode(T&& t)
        :node_ptr(make_cow_ptr<std::decay_t<T>>(std::forward<T>(t)))
    {}

    ~VariantNode() = default;
};

template <typename... Alternatives>
inline std::ostream& operator<<(std::ostream& o, const VariantNode<Alternatives...>& node)
{
    return o << node.print();
}

}

#endif

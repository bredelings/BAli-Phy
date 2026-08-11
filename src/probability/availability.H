#ifndef AVAILABILITY_H
#define AVAILABILITY_H

#include <compare>
#include <concepts>
#include <optional>
#include <ostream>
#include <type_traits>
#include <utility>

struct unavailable_t
{
    explicit constexpr unavailable_t() = default;
};

inline constexpr unavailable_t unavailable;

// Availability distinguishes an unavailable arithmetic term from an available
// value such as zero, and propagates that distinction through choice arithmetic.
template <typename T>
class Availability
{
    std::optional<T> value_;

public:
    constexpr Availability() = default;
    constexpr Availability(unavailable_t) {}
    explicit constexpr Availability(const T& value): value_(value) {}
    explicit constexpr Availability(T&& value): value_(std::move(value)) {}

    constexpr explicit operator bool() const {return value_.has_value();}
    constexpr bool has_value() const {return value_.has_value();}

    constexpr const T& operator*() const& {return *value_;}
    constexpr T& operator*() & {return *value_;}
    constexpr T&& operator*() && {return *std::move(value_);}

    constexpr const T* operator->() const {return &*value_;}
    constexpr T* operator->() {return &*value_;}

    // Accumulate available alternatives while ignoring unavailable ones.
    constexpr Availability& operator+=(const Availability& other)
    {
        *this = *this + other;
        return *this;
    }

    // Accumulate required factors and retain any prior unavailability.
    constexpr Availability& operator*=(const Availability& other)
    {
        *this = *this * other;
        return *this;
    }

    // Unavailable is the additive identity: removing a candidate must not
    // change the total of the candidates that remain available.
    friend constexpr Availability operator+(const Availability& x, const Availability& y)
    {
        if (not x)
            return y;
        if (not y)
            return x;
        return Availability(*x + *y);
    }

    // Unavailable is a multiplicative annihilator because a product is usable
    // only when every factor required to compute it is available.
    friend constexpr Availability operator*(const Availability& x, const Availability& y)
    {
        if (not x or not y)
            return unavailable;
        return Availability(*x * *y);
    }

    // Compare presence first, then compare two contained values.
    friend constexpr bool operator==(const Availability& x, const Availability& y)
        requires std::equality_comparable<T>
    {
        if (not x or not y)
            return bool(x) == bool(y);
        return *x == *y;
    }

    // Sorting unavailable entries below every available entry makes them a
    // zero-probability prefix in choose_MH while preserving the original indices.
    friend constexpr auto operator<=>(const Availability& x, const Availability& y)
        requires std::three_way_comparable<T>
    {
        using ordering = std::compare_three_way_result_t<T>;
        if (not x and not y)
            return ordering::equivalent;
        if (not x)
            return ordering::less;
        if (not y)
            return ordering::greater;
        return *x <=> *y;
    }

    // Make unavailable candidates identifiable in choice diagnostics.
    friend std::ostream& operator<<(std::ostream& out, const Availability& x)
    {
        if (x)
            return out << *x;
        return out << "unavailable";
    }
};

template <typename T>
Availability(T) -> Availability<T>;

template <typename T>
struct is_availability: std::false_type {};

template <typename T>
struct is_availability<Availability<T>>: std::true_type {};

template <typename T>
inline constexpr bool is_availability_v = is_availability<std::remove_cvref_t<T>>::value;

// Lift one already-constructed value without supporting in-place construction.
template <typename T>
constexpr Availability<std::decay_t<T>> available(T&& value)
{
    return Availability<std::decay_t<T>>(std::forward<T>(value));
}

// A raw factor is unconditionally available.  Promote it explicitly to T so
// this convenience does not make raw values implicitly convertible to Availability.
template <typename U, typename T>
    requires (not is_availability_v<U> and std::constructible_from<T, U> and
              std::same_as<std::remove_cvref_t<decltype(T(std::declval<U>()) * std::declval<const T&>())>, T>)
constexpr Availability<T> operator*(U&& x, const Availability<T>& y)
{
    if (not y)
        return unavailable;
    return available(T(std::forward<U>(x)) * *y);
}

// Multiply by an unconditionally available raw factor on the right.
template <typename T, typename U>
    requires (not is_availability_v<U> and std::constructible_from<T, U> and
              std::same_as<std::remove_cvref_t<decltype(std::declval<const T&>() * T(std::declval<U>()))>, T>)
constexpr Availability<T> operator*(const Availability<T>& x, U&& y)
{
    if (not x)
        return unavailable;
    return available(*x * T(std::forward<U>(y)));
}

// Division projects an unavailable numerator to an available zero, but an
// unavailable denominator means that no ratio can be computed.
template <typename T>
constexpr auto operator/(const Availability<T>& x, const Availability<T>& y)
{
    using result_type = std::remove_cvref_t<decltype(*x / *y)>;
    if (not y)
        return Availability<result_type>(unavailable);
    if (not x)
        return available(result_type(0.0));
    return available(*x / *y);
}

// Preserve absence while computing the probability complement of a present value.
template <typename T>
constexpr Availability<T> complement(const Availability<T>& x)
{
    if (not x)
        return unavailable;
    return available(T(1.0) - *x);
}

#endif

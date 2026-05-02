#ifndef NEUMAIER_H
#define NEUMAIER_H

#include "util/math/log-double.H"
#include "util/math/LogDensity.h"

template <typename T>
class NeumaierAccumulator
{
    T sum_ = 0.0;
    T compensation_ = 0.0;
public:
    constexpr NeumaierAccumulator() noexcept = default;
    
    constexpr explicit NeumaierAccumulator(T initial) noexcept 
        : sum_(initial) {}

    constexpr NeumaierAccumulator& operator+=(T x) noexcept {
        const double t = sum_ + x;
        compensation_ += (std::abs(sum_) >= std::abs(x))
            ? (sum_ - t) + x
            : (x - t) + sum_;
        sum_ = t;
        return *this;
    }

    constexpr NeumaierAccumulator& operator-=(T x) noexcept {
        return *this += -x;
    }

    constexpr NeumaierAccumulator& operator+=(const NeumaierAccumulator& other) noexcept {
        *this += other.sum_;
        compensation_ += other.compensation_;
        return *this;
    }

    constexpr NeumaierAccumulator& operator-=(const NeumaierAccumulator& other) noexcept {
        *this -= other.sum_;
        compensation_ -= other.compensation_;
        return *this;
    }

    constexpr T total() const noexcept {
        return sum_ + compensation_;
    }

    constexpr void reset() noexcept {
        sum_ = 0.0;
        compensation_ = 0.0;
    }
};

template <typename T>
struct NeumaierMultiplier
{
    NeumaierAccumulator<T> data;
    NeumaierMultiplier& operator *=(LogNum<T> x) {data += x.log(); return *this;}
    NeumaierMultiplier& operator /=(LogNum<T> x) {data -= x.log(); return *this;}

    void reset() {data.reset();}

    NeumaierMultiplier& operator *=(const NeumaierMultiplier& m2) {data += m2.data; return *this;}
    NeumaierMultiplier& operator /=(const NeumaierMultiplier& m2) {data -= m2.data; return *this;}

    LogNum<T> result() const {return exp_to_log_space(data.total());}
};

template <typename T>
NeumaierMultiplier<T> operator*(NeumaierMultiplier<T> m1, const NeumaierMultiplier<T>& m2)
{
    m1 *= m2;
    return m1;
}

template <typename T>
NeumaierMultiplier<T> operator/(NeumaierMultiplier<T> m1, const NeumaierMultiplier<T>& m2)
{
    m1 /= m2;
    return m1;
}

template <>
class NeumaierAccumulator<LogDensity>
{
    NeumaierAccumulator<double> neginfs_, ones_, infs_;
    int nans_ = 0;
public:
    constexpr NeumaierAccumulator() noexcept = default;

    // Do I use this?
    explicit NeumaierAccumulator(const LogDensity& initial) noexcept
        : neginfs_(initial.neginfs()),
          ones_(initial.ones()),
          infs_(initial.infs()),
          nans_(initial.nans())
        { }

    NeumaierAccumulator& operator+=(const LogDensity& x) noexcept {
        neginfs_ += x.neginfs();
        ones_ += x.ones();
        infs_ += x.infs();
        nans_ += x.nans();
        return *this;
    }

    constexpr NeumaierAccumulator& operator-=(const LogDensity& x) noexcept {
        return *this += -x;
    }

    constexpr NeumaierAccumulator& operator+=(const NeumaierAccumulator& other) noexcept {
        neginfs_ += other.neginfs_;
        ones_ += other.ones_;
        infs_ += other.infs_;
        nans_ += other.nans_;
        return *this;
    }

    constexpr NeumaierAccumulator& operator-=(const NeumaierAccumulator& other) noexcept {
        neginfs_ -= other.neginfs_;
        ones_ -= other.ones_;
        infs_ -= other.infs_;
        nans_ -= other.nans_;
        return *this;
    }

    constexpr LogDensity total() const noexcept {
        return LogDensity(neginfs_.total(), ones_.total(), infs_.total(), nans_);
    }

    constexpr void reset() noexcept {
        neginfs_.reset();
        ones_.reset();
        infs_.reset();
        nans_ = 0;
    }
};

#endif

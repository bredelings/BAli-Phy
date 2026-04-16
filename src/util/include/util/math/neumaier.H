#ifndef NEUMAIER_H
#define NEUMAIER_H

#include "util/math/log-double.H"

struct NeumaierAccumulator
{
public:
    constexpr NeumaierAccumulator() noexcept = default;
    
    constexpr explicit NeumaierAccumulator(double initial) noexcept 
        : sum_(initial) {}

    constexpr NeumaierAccumulator& operator+=(double x) noexcept {
        const double t = sum_ + x;
        compensation_ += (std::abs(sum_) >= std::abs(x))
            ? (sum_ - t) + x
            : (x - t) + sum_;
        sum_ = t;
        return *this;
    }

    constexpr NeumaierAccumulator& operator-=(double x) noexcept {
        return *this += -x;
    }

    constexpr NeumaierAccumulator& operator+=(const NeumaierAccumulator& other) noexcept {
        *this += other.sum_;
        compensation_ += other.compensation_;
        return *this;
    }

    constexpr double total() const noexcept {
        return sum_ + compensation_;
    }

    constexpr explicit operator double () const {return total();}

    constexpr void reset() noexcept {
        sum_ = 0.0;
        compensation_ = 0.0;
    }

private:
    double sum_ = 0.0;
    double compensation_ = 0.0;
};

struct NeumaierMultiplier
{
    NeumaierAccumulator data;
    NeumaierMultiplier& operator *=(log_double_t x) {data += x.log(); return *this;}
    NeumaierMultiplier& operator /=(log_double_t x) {data -= x.log(); return *this;}

    void reset() {data.reset();}

    explicit operator log_double_t() {return exp_to<log_double_t>(double(data));}
};

#endif

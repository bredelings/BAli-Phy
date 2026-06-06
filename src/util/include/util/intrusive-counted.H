#ifndef UTIL_INTRUSIVE_COUNTED_H
#define UTIL_INTRUSIVE_COUNTED_H

template <typename T>
class intrusive_counted
{
    mutable int refs_ = 0;

public:
    void add_intrusive_ref() const
    {
        ++refs_;
    }

    void release_intrusive_ref() const
    {
        if (--refs_ == 0)
            delete const_cast<T*>(static_cast<const T*>(this));
    }

    int ref_count() const { return refs_; }

    bool operator==(const intrusive_counted&) const { return true; }

    intrusive_counted() = default;
    intrusive_counted(const intrusive_counted&) {}

    intrusive_counted& operator=(const intrusive_counted&)
    {
        return *this;
    }

protected:
    ~intrusive_counted() = default;
};

#endif

#ifndef UTIL_INTRUSIVE_COUNTED_H
#define UTIL_INTRUSIVE_COUNTED_H

template <typename T>
class intrusive_counted
{
    mutable int refs_ = 0;

    friend void intrusive_ptr_add_ref(const T* ptr)
    {
        ++ptr->refs_;
    }

    friend void intrusive_ptr_release(const T* ptr)
    {
        if (--ptr->refs_ == 0)
            delete const_cast<T*>(ptr);
    }

public:
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

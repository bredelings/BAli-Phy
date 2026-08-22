#ifndef MODELS_MODEL_BOX_H
#define MODELS_MODEL_BOX_H

#include <cassert>
#include <utility>

namespace CmdModel
{

template<class T>
class Box
{
    T* value_ = nullptr;

public:
    Box() = delete;

    // Takes ownership of an already allocated recursive AST node.
    explicit Box(T* value)
        :value_(value)
    {
        assert(value_);
    }

    Box(const Box&);

    Box(Box&&) noexcept;

    Box& operator=(const Box&);

    Box& operator=(Box&&) noexcept;

    ~Box();

          T& operator*()       {return *value_;}
    const T& operator*() const {return *value_;}

          T* operator->()       {return value_;}
    const T* operator->() const {return value_;}

          T& get()       {return *value_;}
    const T& get() const {return *value_;}
};

// Copies the owned recursive AST node so boxed variants keep value semantics.
template<class T>
Box<T>::Box(const Box& box)
    :value_(new T(*box.value_))
{}

template<class T>
Box<T>::Box(Box&& box) noexcept
    :value_(std::exchange(box.value_, nullptr))
{}

// Copies the owned recursive AST node so assignment keeps value semantics.
template<class T>
Box<T>& Box<T>::operator=(const Box& box)
{
    if (this != &box)
    {
        if (box.value_)
        {
            if (value_)
                *value_ = *box.value_;
            else
                value_ = new T(*box.value_);
        }
        else
        {
            delete value_;
            value_ = nullptr;
        }
    }
    return *this;
}

// Moves ownership of the recursive AST node, leaving the source empty.
template<class T>
Box<T>& Box<T>::operator=(Box&& box) noexcept
{
    if (this != &box)
    {
        delete value_;
        value_ = std::exchange(box.value_, nullptr);
    }
    return *this;
}

// Releases the owned recursive AST node.
template<class T>
Box<T>::~Box()
{
    delete value_;
}

}

#endif

#ifndef MATRIX_H
#define MATRIX_H

#include <utility>

template <typename T>
class matrix
{
  T* data_ = nullptr;

  int size1_ = 0;
  int size2_ = 0;

  int size_ = 0;

  void allocate(int s)
  {
    if (s > size_)
    {
      delete data_;
      data_ = new T[s];
    }
  }

public:

  T* begin() {return data_;}
  const T* begin() const {return data_;}

  T* end() {return data_ + size();}
  const T* end() const {return data_ + size();}

  int size1() const {return size1_;};
  int size2() const {return size2_;};

  int size() const {return size1_ * size2_;}

        T& operator()(int i, int j)       {return data_[size2_*i + j];}
  const T& operator()(int i, int j) const {return data_[size2_*i + j];}

  void swap(matrix<T>& m2)
  {
    std::swap(data_, m2.data_);
    std::swap(size1_, m2.size1_);
    std::swap(size2_, m2.size2_);
    std::swap(size_, m2.size_);
  }

  matrix& operator=(const matrix<T>& m2)
  {
    allocate(m2.size1()*m2.size2());
    for(int i=0;i<m2.size();i++)
      data_[i] = m2.data_[i];
    return *this;
  }

  matrix& operator=(matrix<T>&& m2) noexcept
  {
    swap(m2);
    return *this;
  }

  void resize(int s1,int s2)
  {
    size1_ = s1;
    size2_ = s2;
    allocate(size());
  };

  void resize(int s1,int s2, T f)
  {
    resize(s1, s2);
    fill(f);
  };

  void fill(T t)
  {
    for(int i=0;i<size();i++)
      data_[i] = t;
  }

  matrix() {}

  matrix(int s1, int s2)
  {
    resize(s1,s2);
  }

  matrix(int s1, int s2, T f)
  {
    resize(s1,s2);
    fill(f);
  }

  matrix(const matrix<T>& m2)
  {
    operator=(m2);
  }

  matrix(matrix<T>&& m2) noexcept
  {
    operator=(std::move(m2));
  }

  ~matrix()
  {
    delete data_;
  }
};

#endif /* MATRIX_H */

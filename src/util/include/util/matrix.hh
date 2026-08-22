#ifndef MATRIX_H
#define MATRIX_H

#include <utility>
#include "util/assert.hh"

namespace bali_phy
{

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
		    delete[] data_;
		    data_ = new T[s];
		    size_ = s;
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

        T& operator()(int i, int j)       
	    {
#ifdef DEBUG_BOUNDS_CHECK
		assert( 0 <= i and i < size1() );
		assert( 0 <= j and j < size2() );
#endif
		return data_[size2_*i + j];
	    }

	const T& operator()(int i, int j) const
	    {
#ifdef DEBUG_BOUNDS_CHECK
		assert( 0 <= i and i < size1() );
		assert( 0 <= j and j < size2() );
#endif
		return data_[size2_*i + j];
	    }

	void swap(matrix<T>& m2) noexcept
	    {
		std::swap(data_, m2.data_);
		std::swap(size1_, m2.size1_);
		std::swap(size2_, m2.size2_);
		std::swap(size_, m2.size_);
	    }

	matrix& operator=(const matrix<T>& m2)
	    {
		resize(m2.size1(), m2.size2());

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

	matrix<T>& operator*=(T t) {
	    for(int i=0;i<size();i++)
		data_[i] *= t;
	    return *this;
	}

	matrix<T>& operator/=(T t) {
	    for(int i=0;i<size();i++)
		data_[i] /= t;
	    return *this;
	}

	matrix<T>& operator+=(const matrix<T>& M2) 
	    {
		for(int i=0;i<size();i++)
		    data_[i] += M2.begin()[i];
		return *this;
	    }

	bool operator==(const matrix& M) const
	    {
		if (this == &M) return true;

		if (size1() != M.size1()) return false;

		if (size2() != M.size2()) return false;

		for(int i=0;i<size();i++)
		    if (data_[i] != M.data_[i]) return false;

		return true;
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
		swap(m2);
	    }

	~matrix()
	    {
		delete[] data_;
	    }
    };

    template <typename T>
    class matrix3
    {
	T* data_ = nullptr;

	int size1_ = 0;
	int size2_ = 0;
	int size3_ = 0;

	int size_ = 0;

	void allocate(int s)
	    {
		if (s > size_)
		{
		    delete[] data_;
		    data_ = new T[s];
		    size_ = s;
		}
	    }

    public:

	T* begin() {return data_;}
	const T* begin() const {return data_;}

	T* end() {return data_ + size();}
	const T* end() const {return data_ + size();}

	int size1() const {return size1_;};
	int size2() const {return size2_;};
	int size3() const {return size3_;};

	int size() const {return size1_ * size2_ * size3_;}

        T& operator()(int i, int j, int k)
	    {
#ifdef DEBUG_BOUNDS_CHECK
		assert( 0 <= i and i < size1() );
		assert( 0 <= j and j < size2() );
		assert( 0 <= k and k < size3() );
#endif
		return data_[size3_*(size2_*i + j) + k];
	    }

	const T& operator()(int i, int j, int k) const
	    {
#ifdef DEBUG_BOUNDS_CHECK
		assert( 0 <= i and i < size1() );
		assert( 0 <= j and j < size2() );
		assert( 0 <= k and k < size3() );
#endif
		return data_[size3_*(size2_*i + j) + k];
	    }

	void swap(matrix3<T>& m2) noexcept
	    {
		std::swap(data_, m2.data_);
		std::swap(size1_, m2.size1_);
		std::swap(size2_, m2.size2_);
		std::swap(size_, m2.size_);
	    }

	matrix3& operator=(const matrix3<T>& m2)
	    {
		matrix3<T>(m2).swap(*this);
		return *this;
	    }

	matrix3& operator=(matrix3<T>&& m2) noexcept
	    {
		matrix3<T>(m2).swap(*this);
		return *this;
	    }

	void resize(int s1,int s2, int s3)
	    {
		size1_ = s1;
		size2_ = s2;
		size3_ = s3;
		allocate(size());
	    };

	void resize(int s1,int s2, int s3, T f)
	    {
		resize(s1, s2, s3);
		fill(f);
	    };

	void fill(T t)
	    {
		for(int i=0;i<size();i++)
		    data_[i] = t;
	    }

	matrix3<T>& operator/=(T t) {
	    for(int i=0;i<size();i++)
		data_[i] /= t;
	    return *this;
	}

	matrix3<T>& operator+=(const matrix3<T>& M2) 
	    {
		for(int i=0;i<size();i++)
		    data_[i] += M2.begin()[i];
		return *this;
	    }

	bool operator==(const matrix3& M) const
	    {
		if (this == &M) return true;

		if (size1() != M.size1()) return false;

		if (size2() != M.size2()) return false;

		for(int i=0;i<size();i++)
		    if (data_[i] != M.data_[i]) return false;

		return true;
	    }

	matrix3() {}

	matrix3(int s1, int s2, int s3)
	    {
		resize(s1,s2,s3);
	    }

	matrix3(int s1, int s2, int s3, T f)
	    {
		resize(s1,s2,s3);
		fill(f);
	    }

	matrix3(const matrix3<T>& m2)
	    {
		resize(m2.size1(), m2.size2(), m2.size3());

		for(int i=0;i<m2.size();i++)
		    data_[i] = m2.data_[i];
	    }

	matrix3(matrix3<T>&& m2) noexcept
	    {
		swap(m2);
	    }

	~matrix3()
	    {
		delete[] data_;
	    }
    };

}

using bali_phy::matrix;

using bali_phy::matrix3;

typedef matrix<double> Matrix;

typedef matrix3<double> Matrix3;

#endif /* MATRIX_H */

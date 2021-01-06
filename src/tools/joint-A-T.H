/*
  Copyright (C) 2005,2019 Benjamin Redelings

  This file is part of BAli-Phy.

  BAli-Phy is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  for more details.

  You should have received a copy of the GNU General Public License
  along with BAli-Phy; see the file COPYING.  If not see
  <http://www.gnu.org/licenses/>.  */

#ifndef JOINT_A_T_H
#define JOINT_A_T_H

#include <string>
#include <vector>
#include <optional>
#include <boost/program_options.hpp>

#include "tree/sequencetree.H"
#include "alignment/alignment.H"

// This is like read-trees.H.  Maybe we should use range-v3...

template <typename T>
struct reader
{
    typedef T value_type;

    virtual bool done() const = 0;

    virtual std::optional<T> next_one() = 0;
    virtual void skip_one() { next_one();}

    void skip(int n) { for(int i=0;i<n and not done();i++) skip_one();}
    std::optional<T> next(int n=1) { skip(n-1); return next_one();}

    virtual ~reader() = default;
};

template <typename T>
class file_reader: public reader<T>
{
protected:
    std::istream& file;
public:
    bool done() const {return not file;}

    file_reader(std::istream& f):file(f) {}
};

class line_reader: public file_reader<std::string>
{
public:
    std::optional<std::string> next_one();

    line_reader(std::istream&);
};

class alignment_reader: public file_reader<std::string>
{
public:
    std::optional<std::string> next_one();

    alignment_reader(std::istream&);
};

template <typename R>
class subsample: public reader<typename R::value_type>
{
    int factor;
    R stream;
public:
    bool done() const {return stream.done();}

    std::optional<typename R::value_type> next_one()
    {
        auto v = stream.next_one();
        if (v)
            stream.skip(factor-1);
        return v;
    }

    subsample(int i, const R& r)
        :factor(i), stream(r)
        { }
};

template <typename R1, typename R2>
class zip: public reader<std::pair<typename R1::value_type,typename R2::value_type>>
{
    R1 stream1;
    R2 stream2;

public:
    bool done() const {return stream1.done() or stream2.done();}

    std::optional<std::pair<typename R1::value_type,typename R2::value_type>> next_one()
    {
        auto v1 = stream1.next_one();
        auto v2 = stream2.next_one();
        if (v1 and v2)
            return {{*v1,*v2}};
        else
            return {};
    }

    zip(const R1& r1, const R2& r2)
        :stream1(r1),stream2(r2)
        { }
};

//--------------------------- Class joint_A_T ----------------------------//
class joint_A_T: public std::vector<std::pair<alignment,SequenceTree>>
{
    std::optional<std::vector<std::string>> leaf_names_;

public:

    const std::vector<std::string>& leaf_names() const;

    const alignment& A(int i) {return (*this)[i].first;}
    const SequenceTree& T(int i) {return (*this)[i].second;}
    joint_A_T& load(const std::vector<alignment>& A1,const std::vector<SequenceTree>& T1,bool internal);

    joint_A_T() {};
    joint_A_T(const std::vector<alignment>& A1,const std::vector<SequenceTree>& T1,bool internal);
};

joint_A_T get_joint_A_T(const boost::program_options::variables_map& args,bool internal);

joint_A_T get_multiple_joint_A_T(const boost::program_options::variables_map& args,bool internal);

#endif

#ifndef PROG_MAPPING_H
#define PROG_MAPPING_H

#include <vector>
#include <utility>
#include "util/assert.hh"

class mapping
{
    std::vector<std::pair<int,int>> delta_;

public:
    mapping& operator=(mapping&) = default;

    mapping& operator=(mapping&& vm) noexcept
	{
	    delta_ = std::move(vm.delta_);
	    return *this;
	}

    void add_value(int r, int v)
    {
        // For the force_count program, we need 0-values.
	// assert(v);

	delta_.emplace_back(r,v);
    }

    int erase_value_at(int index)
    {
	auto back = delta_.back();
	delta_.pop_back();

	// If we are deleting from the middle, move the last element to the middle
	if (index < delta_.size())
	    delta_[index] = back;

	return back.second;
    }

    void clear() {delta_.clear();}
    void resize(int s) { delta_.reserve(s);}
    bool empty() const {return delta_.empty();}

    const std::vector<std::pair<int,int>>& delta() const {return delta_;}
          std::vector<std::pair<int,int>>& delta()       {return delta_;}
  
    // This is for debugging
    std::vector<int> find_values(int r0) const;

    mapping() {}
    mapping(int s) {resize(s);}
    mapping(const mapping&) = default;
    mapping(mapping&& vm) noexcept
	:delta_(std::move(vm.delta_))
	{ }
};

// Given a mapping (m1,v1) at the root followed by the relative mapping (m2,v2), construct a new mapping
// where (m2,v2) is at the root and (m1,v1) is relative.
inline void pivot_mapping(std::vector<int>& prog1, mapping& vm2)
{
    for(int i=0;i<vm2.delta().size();i++)
    {
	int r = vm2.delta()[i].first;

	auto& s1 = prog1[r];
	auto& s2 = vm2.delta()[i].second;

        // For the force_count_program, we need 0 values.
	// Check that neither value is 0
	// assert(s1 != 0);
	// assert(s2 != 0);

	// switch root positions
	std::swap(s1,s2);
    }
}

#endif

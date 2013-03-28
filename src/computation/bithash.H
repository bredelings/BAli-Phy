#ifndef BITHASH_H
#define BITHASH_H

#include <bitset>
#include <utility>
#include <vector>
#include <list>

template <typename Key, typename T>
class bithash
{
  typedef std::pair<Key, T> value_type;
  typedef T mapped_type;

  struct bucket
  {
    int n=0;
    value_type first;
    std::list<value_type> rest;

    bool empty() const {return n==0;}

    void clear() {
      n = 0;
      first = value_type();
      rest.clear();
    }

    // Set map[k] = v, and return true if we inserted anything a new key.
    std::pair<T&,bool> find_or_add(Key k)
    {
      // If the bucket is completely empty, take the first spot.
      if (not n)
      {
	first.first = k;
	n++;
	return {first.second, true};
      }

      // If the bucket has a first spot, check if the key is right.
      if (first.first == k)
	return {first.second, false};

      // If the key isn't in the first spot, check all the other spots, and over-write one if we find it.
      for(auto& p: rest)
	if (p.first == k)
	  return {p.second,false};
      
      // If the key isn't anywhere, add it at the beginning of the list.
      rest.insert(rest.begin(),{k,{}});
      n++;
      return {rest.begin()->second,true};
    }

    // Set map[k] = v if k is not in the hash, and return true if we insert a new key.
    bool insert(Key k, const T& v)
    {
      // If the bucket is completely empty, add to the reserved spot.
      if (not n)
      {
	first = {k,v};
	n++;
	return true;
      }

      // If the bucket has a first spot, check and over-write it if the key is right.
      if (first.first == k)
	return false;

      // If the key isn't in the first spot, check all the other spots, and over-write one if we find it.
      for(auto& p: rest)
	if (p.first == k)
	  return false;
      
      // If the key isn't anywhere, add it at the end of the list.
      rest.insert(rest.end(),{k,v});
      n++;
      return true;
    }

    // Remove any map[k]=<any value>, and return true if such a k was in the hash.
    bool erase(Key k)
    {
      // If the bucket is completely empty, there's nothing to delete.
      if (not n) return false;

      // Check if the key is in the first spot
      if (first.first == k)
      {
	// If there are no list elements, this bucket is empty!
	if (rest.empty())
	  assert(n == 0);
	// If there are list elements, then move one down.
	else
	{
	  first = std::move( *rest.begin() );
	  rest.erase(rest.begin());
	}
	n--;
	return true;
      }
      // Check if the key is in the rest of the bucket
      else
	for(auto loc = rest.begin(); loc != rest.end();loc++)
	  if (loc->first == k)
	  {
	    rest.erase(loc);
	    n--;
	    return true;
	  }

      return false;
    }
  
    bool contains_key(Key k) const
    {
      if (n == 0) return false;

      if (first.first == k) return true;

      for(const auto& i: rest)
	if (i.first == k) return true;

      return false;
    }

    const T& lookup_key(Key k) const
    {
      if (n == 0) std::abort();

      if (first.first == k) return first.second;

      for(const auto& i: rest)
	if (i.first == k) return i.second;

      std::abort();
    }

  };

  std::vector<bucket> buckets;
  int n_elements_=0;
  int n_buckets_=0;

public:
  bool empty() const {return n_elements_ == 0;}
  int size() const {return n_elements_;}
  int n_buckets() const {return n_buckets_;}
  int max_buckets() const {return buckets.size();}

  void clear()
  {
    n_elements_ = 0;
    n_buckets_ = 0;
    for(auto& b: buckets)
      b.clear();
  }

  int hash_for_key(Key k) const
  {
    return k.to_ulong();
  }

  int bucket_for_key(Key k) const
  {
    return hash_for_key(k)%buckets.size();
  }

  // Set map[k] = v if k is not in the hash, and return true if we inserted a new key.
  bool insert(Key k, const T& v)
  {
    int b = bucket_for_key(k);
    bucket& B = buckets[b];

    bool new_key = B.insert(k,v);

    if (new_key)
    {
      n_elements_++;
      if (B.n == 1)
	n_buckets_++;
    }

    return new_key;
  }

  // Remove any map[k]=<any value>, and return true if such a k was in the hash.
  bool erase(Key k)
  {
    int b = bucket_for_key(k);
    bucket& B = buckets[b];

    bool had_key = B.erase(k);

    if (had_key)
    {
      n_elements_--;
      if (B.n == 0)
	n_buckets_--;
    }

    return had_key;
  }
  
  bool contains_key(Key k) const
  {
    int b = bucket_for_key(k);
    return buckets[b].contains_key(k);
  }

  const T& lookup_key(Key k) const
  {
    int b = bucket_for_key(k);
    return buckets[b].lookup_key(k);
  }

  const T& operator[](Key k) const
  {
    return lookup_key(k);
  }

  // Set map[k] = v, and return true if we inserted anything a new key.
  std::pair<T&,bool> find_or_add(Key k)
  {
    int b = bucket_for_key(k);
    bucket& B = buckets[b];

    std::pair<T&,bool> p = B.find_or_add(k);

    if (p.second)
    {
      n_elements_++;
      if (B.n==1)
	n_buckets_++;
    }

    return p;
  }

  T& operator[](Key k)
  {
    return find_or_add(k).first;
  }

  float load_factor() const {return float(n_elements_)/n_buckets_;}

  bithash(int n):buckets(n) { }

  bithash():bithash(1024) { }
};

#endif

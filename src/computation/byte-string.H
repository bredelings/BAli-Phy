#ifndef BYTE_STRING_H
#define BYTE_STRING_H

#include <cstddef>
#include <memory>
#include <vector>

#include "computation/object.H"

class ByteString: public Object
{
public:
    using byte = std::byte;
    using storage_type = std::vector<byte>;

private:
    std::shared_ptr<const storage_type> storage_;
    std::size_t offset_ = 0;
    std::size_t length_ = 0;

    void check_invariant() const;

public:
    ByteString();
    explicit ByteString(storage_type bytes);
    ByteString(std::shared_ptr<const storage_type> storage, std::size_t offset, std::size_t length);

    ByteString* clone() const override;
    bool operator==(const Object& object) const override;
    bool operator==(const ByteString& other) const;
    std::string print() const override;

    std::size_t size() const { return length_; }
    bool empty() const { return length_ == 0; }
    const byte* data() const;
    byte operator[](std::size_t index) const;
    ByteString slice(std::size_t offset, std::size_t length) const;
};

#endif

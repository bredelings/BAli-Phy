#include "computation/byte-string.H"

#include <iomanip>
#include <sstream>

#include "util/myexception.H"

// Check the shared-storage slice invariants in one place so every constructor
// and slice operation rejects impossible byte windows consistently.
void ByteString::check_invariant() const
{
    if (not storage_)
        throw myexception()<<"ByteString: null storage.";

    if (offset_ > storage_->size())
        throw myexception()<<"ByteString: offset "<<offset_<<" is past storage size "<<storage_->size()<<".";

    if (length_ > storage_->size() - offset_)
        throw myexception()<<"ByteString: length "<<length_<<" exceeds storage size "<<storage_->size()<<" at offset "<<offset_<<".";
}

ByteString::ByteString()
    :storage_(std::make_shared<storage_type>())
{ }

ByteString::ByteString(storage_type bytes)
    :storage_(std::make_shared<storage_type>(std::move(bytes))), offset_(0), length_(storage_->size())
{ }

ByteString::ByteString(std::shared_ptr<const storage_type> storage, std::size_t offset, std::size_t length)
    :storage_(std::move(storage)), offset_(offset), length_(length)
{
    check_invariant();
}

ByteString* ByteString::clone() const
{
    return new ByteString(*this);
}

// Compare ByteStrings by visible bytes, not by shared backing identity.  This
// keeps slices and independently-built byte strings interchangeable.
bool ByteString::operator==(const Object& object) const
{
    auto other = dynamic_cast<const ByteString*>(&object);
    return other and *this == *other;
}

// Compare the selected slice of two byte strings without requiring either side
// to normalize or copy its backing storage first.
bool ByteString::operator==(const ByteString& other) const
{
    if (size() != other.size())
        return false;

    for(std::size_t i = 0; i < size(); i++)
        if ((*this)[i] != other[i])
            return false;

    return true;
}

// Print bytes as numeric data for debugging.  This deliberately does not try to
// decode the byte string as text.
std::string ByteString::print() const
{
    std::ostringstream out;
    out<<"ByteString [";
    for(std::size_t i = 0; i < size(); i++)
    {
        if (i != 0)
            out<<", ";
        out<<std::to_integer<int>((*this)[i]);
    }
    out<<"]";
    return out.str();
}

const ByteString::byte* ByteString::data() const
{
    if (empty())
        return nullptr;
    return storage_->data() + offset_;
}

ByteString::byte ByteString::operator[](std::size_t index) const
{
    if (index >= size())
        throw myexception()<<"ByteString index "<<index<<" is out of range for length "<<size()<<".";

    return (*storage_)[offset_ + index];
}

// Return a new byte-string view over the same immutable backing storage.  The
// backing vector stays alive through shared ownership until every slice is gone.
ByteString ByteString::slice(std::size_t offset, std::size_t length) const
{
    if (offset > size())
        throw myexception()<<"ByteString slice offset "<<offset<<" is past length "<<size()<<".";

    if (length > size() - offset)
        throw myexception()<<"ByteString slice length "<<length<<" exceeds length "<<size()<<" at offset "<<offset<<".";

    return ByteString(storage_, offset_ + offset, length);
}

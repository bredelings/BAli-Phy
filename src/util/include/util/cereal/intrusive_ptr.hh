#ifndef UTIL_CEREAL_INTRUSIVE_PTR_H
#define UTIL_CEREAL_INTRUSIVE_PTR_H

#include <boost/intrusive_ptr.hpp>
#include <cereal/cereal.hpp>
#include <cstdint>
#include <memory>
#include <type_traits>

namespace cereal
{
    template <class Archive, class T>
    void save(Archive& ar, const boost::intrusive_ptr<T>& ptr)
    {
        std::shared_ptr<const void> registry_ptr;
        if (ptr)
        {
            registry_ptr = std::shared_ptr<const void>(
                ptr.get(),
                [keep_alive = ptr](const void*) mutable { keep_alive.reset(); });
        }

        std::uint32_t id = ar.registerSharedPointer(registry_ptr);
        ar(CEREAL_NVP_("id", id));

        if (id & detail::msb_32bit)
            ar(CEREAL_NVP_("data", *ptr));
    }

    template <class Archive, class T>
    void load(Archive& ar, boost::intrusive_ptr<T>& ptr)
    {
        std::uint32_t id;
        ar(CEREAL_NVP_("id", id));

        if (id & detail::msb_32bit)
        {
            using NonConstT = std::remove_const_t<T>;
            boost::intrusive_ptr<NonConstT> loaded(new NonConstT);

            std::shared_ptr<void> registry_ptr(
                loaded.get(),
                [keep_alive = loaded](void*) mutable { keep_alive.reset(); });
            ar.registerSharedPointer(id, registry_ptr);

            ar(CEREAL_NVP_("data", *loaded));
            ptr = std::move(loaded);
        }
        else
        {
            auto shared = ar.getSharedPointer(id);
            ptr = boost::intrusive_ptr<T>(static_cast<T*>(shared.get()));
        }
    }
}

#endif

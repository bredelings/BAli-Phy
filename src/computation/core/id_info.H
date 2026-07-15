#ifndef CORE_ID_INFO_H
#define CORE_ID_INFO_H

namespace Core
{

enum class one_shot_info
{
    unknown,
    one_shot,
};

struct id_info
{
    int arity = 0;
    int call_arity = 0;
    one_shot_info one_shot = one_shot_info::unknown;

    // Serialize only metadata that remains valid across compilation boundaries.
    template <class Archive>
    void serialize(Archive& ar)
    {
        ar(arity, one_shot);
    }
};

}

#endif

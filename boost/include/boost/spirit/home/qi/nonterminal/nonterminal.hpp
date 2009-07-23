/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
==============================================================================*/
#if !defined(BOOST_SPIRIT_NONTERMINAL_FEB_19_2007_0236PM)
#define BOOST_SPIRIT_NONTERMINAL_FEB_19_2007_0236PM

#include <boost/spirit/home/support/nonterminal/nonterminal.hpp>
#include <boost/spirit/home/support/nonterminal/locals.hpp>
#include <boost/spirit/home/support/argument.hpp>

#include <boost/xpressive/proto/proto.hpp>
#include <boost/function_types/result_type.hpp>
#include <boost/function_types/parameter_types.hpp>
#include <boost/function_types/is_function.hpp>
#include <boost/fusion/include/as_vector.hpp>
#include <boost/fusion/include/mpl.hpp>
#include <boost/fusion/include/joint_view.hpp>
#include <boost/fusion/include/single_view.hpp>

#include <boost/type_traits/add_reference.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/filter_view.hpp>
#include <boost/mpl/find_if.hpp>
#include <boost/mpl/not.hpp>
#include <boost/mpl/or.hpp>
#include <boost/mpl/size.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/preprocessor/enum_params.hpp>
#include <boost/preprocessor/enum_params_with_a_default.hpp>
#include <boost/utility/enable_if.hpp>

namespace boost { namespace spirit { namespace qi
{
    template <typename Derived, typename Sig, typename Locals>
    struct nonterminal
      : proto::extends<
            typename make_nonterminal_holder<
                Derived const*, Derived
            >::type
          , Derived
        >
    {
        typedef Sig sig_type;
        typedef typename function_types::result_type<sig_type>::type attr_type_;

        // This is the nonterminal return type
        typedef typename
            mpl::if_<
                is_same<attr_type_, void>
              , unused_type
              , attr_type_
            >::type
        attr_type;
        typedef typename add_reference<attr_type>::type attr_reference_type;

        // param_types is a sequence of types passed as parameters to the nonterminal
        typedef typename function_types::parameter_types<sig_type>::type param_types;

        // locals_type is a sequence of types to be used as local variables
        typedef typename fusion::result_of::as_vector<Locals>::type locals_type;

        //  The overall context_type consist of a tuple with:
        //      1) a tuple of the return value and parameters
        //      2) the locals
        typedef fusion::vector<
                typename fusion::result_of::as_vector<
                    fusion::joint_view<
                        fusion::single_view<attr_reference_type>
                      , param_types
                    >
                >::type
              , typename fusion::result_of::as_vector<locals_type>::type
            >
        context_type;

        typedef nonterminal<Derived, Sig, Locals> self_type;
        typedef nonterminal_holder<Derived const*, Derived> nonterminal_holder_;
        typedef typename proto::terminal<nonterminal_holder_>::type nonterminal_tag;
        typedef proto::extends<nonterminal_tag, Derived> base_type;

        explicit nonterminal()
          : base_type(make_tag())
        {
        }

        // bring in the operator() overloads
        #include <boost/spirit/home/support/nonterminal/detail/nonterminal_fcall.hpp>

    private:

        nonterminal_tag make_tag() const
        {
            nonterminal_tag xpr = {{static_cast<Derived const*>(this)}};
            return xpr;
        }
    };

    template <typename Derived, typename T0, typename T1, typename T2>
    struct make_nonterminal
    {
        typedef mpl::vector<T0, T1, T2> types;
        typedef function_types::is_function<mpl::_> is_function;
        typedef spirit::detail::is_locals<mpl::_> is_locals;
        typedef spirit::traits::is_component<qi::domain, mpl::_> is_skipper;

        typedef typename mpl::find_if<types, is_function>::type sig_;
        typedef typename mpl::find_if<types, is_locals>::type locals_;
        typedef typename mpl::find_if<types, is_skipper>::type skipper_;

        typedef typename
            mpl::eval_if<
                is_same<sig_, typename mpl::end<types>::type>
              , mpl::identity<unused_type()>
              , mpl::deref<sig_>
            >::type
        sig_type;

        typedef typename
            mpl::eval_if<
                is_same<locals_, typename mpl::end<types>::type>
              , mpl::identity<locals<> >
              , mpl::deref<locals_>
            >::type
        locals_type;

        typedef typename
            mpl::eval_if<
                is_same<skipper_, typename mpl::end<types>::type>
              , mpl::identity<unused_type>
              , mpl::deref<skipper_>
            >::type
        skipper_type;

        typedef nonterminal<Derived, sig_type, locals_type> type;
    };
}}}

#endif

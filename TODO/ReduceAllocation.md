# Reduce interpreter allocations

This note is based on `/home/bredelings/Work/n2.heaptrack.zst`, recorded from:

```
heaptrack -o n2.heaptrack bali-phy 25-muscle.fasta --seed=0 -Inone --iter=300
```

The ranking below was generated from folded allocation stacks:

```
heaptrack_print /home/bredelings/Work/n2.heaptrack.zst \
  --flamegraph-cost-type allocations \
  --print-flamegraph /tmp/n2.allocations.stacks \
  --print-allocators 0 --print-temporary 0 --print-peaks 0
```

This is an exclusive allocation-site ranking: each folded stack contributes its
count only to the deepest frame in that stack. This excludes callees from the
ranked function count. Some exclusive frames are C++ library helpers, such as
`std::vector` growth or hash-table insertion; for those entries, the caller
trace still has to be inspected to find the project code that causes the
allocation. Total allocation calls in the run: 112,420,771.

For library-like allocation frames, the indented bullets show the dominant
immediate caller chain, up to four callers. The chain stops after reaching
BAli-Phy/runtime code. If recursive template/deserialization frames would
immediately repeat a symbol, the next largest non-repeating caller is used.
Here "library-like" means a frame whose defining symbol looks like `std`,
`boost`, `cereal`, `immer`, `reflex`, or an unresolved raw address.

## Top 100 exclusive allocation frames

1. 8,923,605 (7.94%) - `expm1(Eigen::SelfAdjointEigenSolver<> const&, double)`
2. 7,986,138 (7.10%) - `substitution::peel_internal_branch_SEV(Likelihood_Cache_Branch const&, Likelihood_Cache_Branch const&, Runtime::RVector const&)`
3. 5,820,951 (5.18%) - `std::enable_if<>::type cereal::load<>(cereal::BinaryInputArchive&, cereal::memory_detail::PtrWrapper<>&)`
   - caller: `void cereal::load<>(cereal::BinaryInputArchive&, std::variant<>&)` (77.5% of previous frame, 4.01% total)
   - caller: `std::enable_if<>::type cereal::load<>(cereal::BinaryInputArchive&, std::vector<>&)` (4.2% of previous frame, 0.17% total)
   - caller: `std::enable_if<` (42.7% of previous frame, 0.07% total)
4. 4,773,903 (4.25%) - `immer::detail::hamts::champ<>::do_add(immer::detail::hamts::node<>*, int, unsigned long, unsigned int) const [clone .isra.0]`
   - caller: `builtin_function_mapNegate` (100.0% of previous frame, 4.25% total)
5. 4,588,239 (4.08%) - `builtin_function_list_to_vector`
6. 4,136,846 (3.68%) - `reg_heap::set_reg_value(int, closure&&, int, bool)`
7. 4,095,766 (3.64%) - `std::pair<> std::_Hashtable<>::_M_emplace_uniq<>(std::pair<>&&) [clone .isra.0]`
   - caller: `std::_Function_handler<>::_M_invoke(std::_Any_data const&, register_prob const&, int&&)` (41.4% of previous frame, 1.51% total)
   - caller: `reg_heap::register_prior(register_prob const&, int)` (34.5% of previous frame, 0.52% total)
8. 3,930,174 (3.50%) - `std::pair<>& std::vector<>::emplace_back<>(int&, int&) [clone .isra.0]`
   - caller: `reg_heap::incremental_evaluate2_changeable_(int)` (83.8% of previous frame, 2.93% total)
9. 3,723,481 (3.31%) - `cereal::InputArchive<>::registerSharedPointer(unsigned int, std::shared_ptr<>)`
   - caller: `std::enable_if<>::type cereal::load<>(cereal::BinaryInputArchive&, cereal::memory_detail::PtrWrapper<>&)` (75.3% of previous frame, 2.49% total)
   - caller: `void cereal::load<>(cereal::BinaryInputArchive&, std::variant<>&)` (80.5% of previous frame, 2.01% total)
   - caller: `std::enable_if<>::type cereal::load<>(cereal::BinaryInputArchive&, std::vector<>&)` (4.2% of previous frame, 0.08% total)
   - caller: `std::enable_if<` (42.7% of previous frame, 0.04% total)
10. 3,610,384 (3.21%) - `std::pair<> std::_Hashtable<>::_M_emplace_uniq<>(int const&) [clone .isra.0]`
   - caller: `reg_heap::mark_effect_to_unregister_at_step(int)` (59.5% of previous frame, 1.91% total)
11. 3,569,442 (3.18%) - `exp_small(Eigen::SelfAdjointEigenSolver<> const&, std::vector<> const&, double)`
12. 3,239,201 (2.88%) - `builtin_function_restrictKeysToVector`
13. 2,947,280 (2.62%) - `substitution::peel_leaf_branch_SEV(SparseLikelihoods const&, Runtime::RVector const&)`
14. 2,868,219 (2.55%) - `void cereal::load<>(cereal::BinaryInputArchive&, std::variant<>&)`
   - caller: `std::enable_if<>::type cereal::load<>(cereal::BinaryInputArchive&, cereal::memory_detail::PtrWrapper<>&)` (87.6% of previous frame, 2.24% total)
   - caller: `void cereal::load<>(cereal::BinaryInputArchive&, std::multimap<>&)` (0.4% of previous frame, 0.01% total)
15. 2,596,608 (2.31%) - `reg_heap::inc_count(int)`
16. 2,450,588 (2.18%) - `std::enable_if<`
   - caller: `void cereal::load<>(cereal::BinaryInputArchive&, std::variant<>&)` (98.1% of previous frame, 2.14% total)
   - caller: `std::enable_if<>::type cereal::load<>(cereal::BinaryInputArchive&, cereal::memory_detail::PtrWrapper<>&)` (79.0% of previous frame, 1.69% total)
17. 2,162,170 (1.92%) - `Runtime::Exp::Exp(bool)`
18. 2,041,018 (1.82%) - `immer::detail::hamts::node<>::make_inner_n(unsigned int, unsigned int)`
   - caller: `immer::detail::hamts::champ<>::do_add(immer::detail::hamts::node<>*, int, unsigned long, unsigned int) const [clone .isra.0]` (50.8% of previous frame, 0.92% total)
   - caller: `TreeInterface::reconnect_branch(int, int, int)` (97.0% of previous frame, 0.89% total)
19. 1,917,732 (1.71%) - `Box<>::clone() const`
20. 1,848,208 (1.64%) - `Runtime::RVector::operator std::vector<>() const`
21. 1,838,082 (1.64%) - `Runtime::RMaybe::clone() const`
22. 1,831,254 (1.63%) - `builtin_function_lExpRaw`
23. 1,645,178 (1.46%) - `immer::detail::hamts::champ<>::sub_result immer::detail::hamts::champ<>::do_sub<>(immer::detail::hamts::node<>*, int const&, unsigned long, unsigned int) const [clone .isra.0]`
   - caller: `builtin_function_delete` (100.0% of previous frame, 1.46% total)
24. 1,609,378 (1.43%) - `boost::container::vec_iterator<> boost::container::vector<>::priv_insert_forward_range_no_capacity<>(int*, unsigned long, boost::container::dtl::insert_emplace_proxy<>, boost::move_detail::integral_constant<>) [clone .isra.0]`
   - caller: `reg_heap::set_prev_prog_token(int, std::optional<>)` (75.9% of previous frame, 1.09% total)
25. 1,491,315 (1.33%) - `substitution::calc_prob_at_root_SEV(Runtime::RVector const&, Runtime::RVector const&, bali_phy::matrix<> const&, Runtime::RVector const&)`
26. 1,341,611 (1.19%) - `TreeInterface::partition(int) const`
27. 1,226,630 (1.09%) - `boost::container::vec_iterator<> boost::container::vector<>::priv_insert_forward_range_no_capacity<>(use_force_edge*, unsigned long, boost::container::dtl::insert_emplace_proxy<>, boost::move_detail::integral_constant<>) [clone .isra.0]`
   - caller: `reg_heap::set_used_reg(int, int)` (58.8% of previous frame, 0.64% total)
28. 1,170,313 (1.04%) - `reg_heap::set_token_for_unset_context_(int, int)`
29. 1,138,800 (1.01%) - `tree_constants::tree_constants(context_ref&, int)`
30. 1,101,475 (0.98%) - `std::_Hashtable<>::_M_insert_unique_node(unsigned long, unsigned long, std::__detail::_Hash_node<>*, unsigned long)`
   - caller: `std::pair<> std::_Hashtable<>::_M_emplace_uniq<>(std::pair<>&&) [clone .isra.0]` (98.8% of previous frame, 0.97% total)
   - caller: `std::_Function_handler<>::_M_invoke(std::_Any_data const&, register_prob const&, int&&)` (99.0% of previous frame, 0.96% total)
   - caller: `reg_heap::register_prior(register_prob const&, int)` (25.5% of previous frame, 0.24% total)
31. 1,006,190 (0.90%) - `TreeInterface::reconnect_branch(int, int, int)`
32. 854,319 (0.76%) - `builtin_function_register_prior`
33. 804,121 (0.72%) - `reg_heap::do_pending_effect_unregistrations()`
34. 759,127 (0.68%) - `std::pair<> std::_Rb_tree<>::_M_insert_unique<>(std::pair<> const&)`
   - caller: `reflex::Pattern::gen_predict_match_transitions(unsigned short, reflex::Pattern::DFA::State*, std::pair<> const&, std::map<>&, bool&)` (99.9% of previous frame, 0.67% total)
   - caller: `reflex::Pattern::gen_predict_match(std::set<>&)` (100.0% of previous frame, 0.67% total)
   - caller: `reflex::Pattern::analyze_dfa(reflex::Pattern::DFA::State*)` (100.0% of previous frame, 0.67% total)
   - caller: `reflex::Pattern::assemble(reflex::Pattern::DFA::State*)` (100.0% of previous frame, 0.67% total)
35. 685,463 (0.61%) - `reg_heap::reroot_at(int) [clone .localalias]`
36. 653,901 (0.58%) - `TreeInterface::branches_after(int) const`
37. 618,600 (0.55%) - `reg_heap::find_set_regs_on_path(int) const`
38. 565,959 (0.50%) - `std::__cxx11::basic_string<>::_M_mutate(unsigned long, unsigned long, char const*, unsigned long)`
   - caller: `std::__cxx11::basic_string<>::_M_replace_aux(unsigned long, unsigned long, unsigned long, char)` (94.9% of previous frame, 0.48% total)
   - caller: `std::enable_if<>::type cereal::load<>(cereal::BinaryInputArchive&, std::__cxx11::basic_string<>&)` (100.0% of previous frame, 0.48% total)
   - caller: `std::enable_if<>::type cereal::load<>(cereal::BinaryInputArchive&, cereal::memory_detail::PtrWrapper<>&)` (30.2% of previous frame, 0.14% total)
   - caller: `void cereal::load<>(cereal::BinaryInputArchive&, std::variant<>&)` (45.3% of previous frame, 0.07% total)
39. 562,602 (0.50%) - `std::vector<>::reserve(unsigned long)`
   - caller: `substitution::calc_prob_at_root_SEV(Runtime::RVector const&, Runtime::RVector const&, bali_phy::matrix<> const&, Runtime::RVector const&)` (88.4% of previous frame, 0.44% total)
40. 517,296 (0.46%) - `reg_heap::equivalent_contexts(int) const`
41. 499,870 (0.44%) - `reg_heap::equivalent_tokens(int) const`
42. 499,870 (0.44%) - `reg_heap::unshare_and_evaluate_program(int)`
43. 497,105 (0.44%) - `builtin_function_register_likelihood`
44. 476,923 (0.42%) - `std::_Rb_tree_iterator<> std::_Rb_tree<>::_M_emplace_hint_unique<>(std::_Rb_tree_const_iterator<>, std::piecewise_construct_t const&, std::tuple<>&&, std::tuple<>&&) [clone .isra.0]`
   - caller: `reflex::Pattern::parse2(bool, unsigned int&, std::vector<>&, std::vector<>&, bool&, std::map<>&, unsigned char&, std::vector<>&, reflex::ORanges<>*, reflex::ORanges<>&, unsigned short&)` (33.3% of previous frame, 0.14% total)
   - caller: `reflex::Pattern::parse1(bool, unsigned int&, std::vector<>&, std::vector<>&, bool&, std::map<>&, unsigned char&, std::vector<>&, reflex::ORanges<>*, reflex::ORanges<>&, unsigned short&)` (100.0% of previous frame, 0.14% total)
   - caller: `reflex::Pattern::parse4(bool, unsigned int&, std::vector<>&, std::vector<>&, bool&, std::map<>&, unsigned char&, std::vector<>&, reflex::ORanges<>*, reflex::ORanges<>&, unsigned short&)` (100.0% of previous frame, 0.14% total)
   - caller: `reflex::Pattern::parse3(bool, unsigned int&, std::vector<>&, std::vector<>&, bool&, std::map<>&, unsigned char&, std::vector<>&, reflex::ORanges<>*, reflex::ORanges<>&, unsigned short&)` (100.0% of previous frame, 0.14% total)
45. 451,057 (0.40%) - `void std::vector<>::_M_realloc_append<>(int const&)`
   - caller: `TreeInterface::append_branches_after(int, std::vector<>&) const` (45.8% of previous frame, 0.18% total)
46. 411,437 (0.37%) - `void cereal::load<>(cereal::BinaryInputArchive&, std::multimap<>&)`
   - caller: `std::enable_if<>::type cereal::load<>(cereal::BinaryInputArchive&, cereal::memory_detail::PtrWrapper<>&)` (100.0% of previous frame, 0.37% total)
   - caller: `read_cached_module(module_loader const&, std::__cxx11::basic_string<> const&, std::__cxx11::basic_string<> const&)` (100.0% of previous frame, 0.37% total)
47. 398,315 (0.35%) - `void cereal::load<>(cereal::BinaryInputArchive&, boost::intrusive_ptr<>&)`
   - caller: `void Runtime::Exp::load<>(cereal::BinaryInputArchive&)` (100.0% of previous frame, 0.35% total)
48. 391,849 (0.35%) - `reg_heap::make_child_token(int, token_type)`
49. 366,831 (0.33%) - `sample_A5_multi(std::vector<>&, std::vector<> const&, std::vector<> const&)`
50. 365,689 (0.33%) - `TreeInterface::append_branches_before(int, std::vector<>&) const`
51. 364,944 (0.32%) - `A5::get_nodes(TreeInterface const&, int)`
52. 350,014 (0.31%) - `std::_Rb_tree_node_base* std::_Rb_tree<>::_M_copy<>(std::_Rb_tree_node<>*, std::_Rb_tree_node_base*, std::_Rb_tree<>::_Alloc_node&) [clone .isra.0]`
   - caller: `spr_info::spr_info(TreeInterface const&, tree_edge const&, std::map<> const&)` (14.3% of previous frame, 0.04% total)
53. 311,462 (0.28%) - `reg_heap::do_pending_effect_registrations()`
54. 310,019 (0.28%) - `reg_heap::translate_refs(Runtime::Exp const&, boost::container::small_vector<>&, int)`
55. 308,897 (0.27%) - `void std::vector<>::_M_range_insert<>(__gnu_cxx::__normal_iterator<>, __gnu_cxx::__normal_iterator<>, __gnu_cxx::__normal_iterator<>, std::forward_iterator_tag) [clone .isra.0]`
   - caller: `reflex::Pattern::parse2(bool, unsigned int&, std::vector<>&, std::vector<>&, bool&, std::map<>&, unsigned char&, std::vector<>&, reflex::ORanges<>*, reflex::ORanges<>&, unsigned short&)` (59.7% of previous frame, 0.16% total)
   - caller: `reflex::Pattern::parse1(bool, unsigned int&, std::vector<>&, std::vector<>&, bool&, std::map<>&, unsigned char&, std::vector<>&, reflex::ORanges<>*, reflex::ORanges<>&, unsigned short&)` (100.0% of previous frame, 0.16% total)
   - caller: `reflex::Pattern::parse4(bool, unsigned int&, std::vector<>&, std::vector<>&, bool&, std::map<>&, unsigned char&, std::vector<>&, reflex::ORanges<>*, reflex::ORanges<>&, unsigned short&)` (100.0% of previous frame, 0.16% total)
   - caller: `reflex::Pattern::parse3(bool, unsigned int&, std::vector<>&, std::vector<>&, bool&, std::map<>&, unsigned char&, std::vector<>&, reflex::ORanges<>*, reflex::ORanges<>&, unsigned short&)` (100.0% of previous frame, 0.16% total)
56. 267,300 (0.24%) - `walk_tree_path(TreeInterface const&, int)`
57. 245,425 (0.22%) - `reg_heap::remove_zero_count_regs(std::vector<> const&, std::vector<> const&)`
58. 238,558 (0.21%) - `MCMC::Result::Result(int, int)`
59. 234,038 (0.21%) - `spr_to_index(Parameters&, spr_info&, int, std::vector<> const&)`
60. 232,665 (0.21%) - `exp(Eigen::SelfAdjointEigenSolver<> const&, double)`
61. 232,527 (0.21%) - `std::vector<>::vector(std::vector<> const&)`
   - caller: `reg_heap::translate_refs(Runtime::Exp const&, boost::container::small_vector<>&, int)` (39.3% of previous frame, 0.08% total)
62. 231,618 (0.21%) - `std::pair<> std::_Rb_tree<>::_M_insert_unique<>(std::pair<>&&)`
   - caller: `SPR_search_attachment_points(Parameters, tree_edge const&, std::map<> const&, spr_attachment_points const&, std::map<> const&, bool)` (94.0% of previous frame, 0.19% total)
63. 223,265 (0.20%) - `Runtime::Exp& std::vector<>::emplace_back<>(Runtime::Exp&&) [clone .isra.0]`
   - caller: `reg_heap::translate_refs(Runtime::Exp const&, boost::container::small_vector<>&, int)` (53.8% of previous frame, 0.11% total)
64. 219,360 (0.20%) - `three_way_NNI_sample(Parameters&, MCMC::MoveStats&, int, int, int, int)`
65. 198,782 (0.18%) - `std::vector<>::insert(__gnu_cxx::__normal_iterator<>, reflex::Pattern::Position const&) [clone .isra.0]`
   - caller: `reflex::Pattern::parse4(bool, unsigned int&, std::vector<>&, std::vector<>&, bool&, std::map<>&, unsigned char&, std::vector<>&, reflex::ORanges<>*, reflex::ORanges<>&, unsigned short&)` (81.7% of previous frame, 0.14% total)
   - caller: `reflex::Pattern::parse3(bool, unsigned int&, std::vector<>&, std::vector<>&, bool&, std::map<>&, unsigned char&, std::vector<>&, reflex::ORanges<>*, reflex::ORanges<>&, unsigned short&)` (100.0% of previous frame, 0.14% total)
   - caller: `reflex::Pattern::parse2(bool, unsigned int&, std::vector<>&, std::vector<>&, bool&, std::map<>&, unsigned char&, std::vector<>&, reflex::ORanges<>*, reflex::ORanges<>&, unsigned short&)` (100.0% of previous frame, 0.14% total)
   - caller: `reflex::Pattern::parse1(bool, unsigned int&, std::vector<>&, std::vector<>&, bool&, std::map<>&, unsigned char&, std::vector<>&, reflex::ORanges<>*, reflex::ORanges<>&, unsigned short&)` (100.0% of previous frame, 0.14% total)
66. 192,896 (0.17%) - `boost::container::vec_iterator<> boost::container::vector<>::priv_insert_forward_range_no_capacity<>(std::pair<>*, unsigned long, boost::container::dtl::insert_emplace_proxy<>, boost::move_detail::integral_constant<>) [clone .isra.0]`
   - caller: `reg_heap::set_used_reg(int, int)` (99.7% of previous frame, 0.17% total)
67. 189,759 (0.17%) - `std::enable_if<>::type cereal::load<>(cereal::BinaryInputArchive&, std::vector<>&)`
   - caller: `std::enable_if<` (38.2% of previous frame, 0.06% total)
   - caller: `void cereal::load<>(cereal::BinaryInputArchive&, std::variant<>&)` (87.7% of previous frame, 0.06% total)
   - caller: `void symbol_info::serialize<>(cereal::BinaryInputArchive&)` (85.1% of previous frame, 0.05% total)
68. 188,839 (0.17%) - `std::filesystem::__cxx11::path::_List::_List(std::filesystem::__cxx11::path::_List const&)`
   - caller: `check_file_in_path(std::vector<> const&, std::filesystem::__cxx11::path const&)` (66.3% of previous frame, 0.11% total)
69. 188,483 (0.17%) - `sample_SPR_search_one(Parameters&, MCMC::MoveStats&, tree_edge const&, std::map<> const&, bool)`
70. 170,371 (0.15%) - `spr_full_range(TreeInterface const&, tree_edge const&)`
71. 169,649 (0.15%) - `std::_Rb_tree_iterator<> std::_Rb_tree<>::_M_insert_unique_<>(std::_Rb_tree_const_iterator<>, std::pair<> const&, std::_Rb_tree<>::_Alloc_node&) [clone .isra.0]`
   - caller: `reflex::Pattern::gen_predict_match_transitions(unsigned short, reflex::Pattern::DFA::State*, std::pair<> const&, std::map<>&, bool&)` (100.0% of previous frame, 0.15% total)
   - caller: `reflex::Pattern::gen_predict_match(std::set<>&)` (100.0% of previous frame, 0.15% total)
   - caller: `reflex::Pattern::analyze_dfa(reflex::Pattern::DFA::State*)` (100.0% of previous frame, 0.15% total)
   - caller: `reflex::Pattern::assemble(reflex::Pattern::DFA::State*)` (100.0% of previous frame, 0.15% total)
72. 157,230 (0.14%) - `reg_heap::unshare_regs1(int)::{lambda(int)#3}::operator()(int) const`
73. 152,852 (0.14%) - `reflex::Pattern::transition(std::__cxx11::list<>&, reflex::Pattern::Chars&, std::vector<> const&) const`
   - caller: `reflex::Pattern::compile_transition(reflex::Pattern::DFA::State*, std::map<>&, std::vector<> const&, reflex::ORanges<> const*, std::map<> const&, std::__cxx11::list<>&) const` (100.0% of previous frame, 0.14% total)
   - caller: `reflex::Pattern::compile(reflex::Pattern::DFA::State*, std::map<>&, std::vector<> const&, reflex::ORanges<> const*, std::map<> const&)` (100.0% of previous frame, 0.14% total)
   - caller: `reflex::Pattern::init(char const*, char const*)` (100.0% of previous frame, 0.14% total)
   - caller: `reflex::Pattern::Pattern(char const*, char const*)` (100.0% of previous frame, 0.14% total)
74. 143,179 (0.13%) - `std::__cxx11::basic_string<>::reserve(unsigned long)`
   - caller: `std::filesystem::__cxx11::path::operator/=(std::filesystem::__cxx11::path const&)` (88.6% of previous frame, 0.11% total)
   - caller: `check_file_in_path(std::vector<> const&, std::filesystem::__cxx11::path const&)` (98.6% of previous frame, 0.11% total)
75. 135,887 (0.12%) - `Program::module_names_set[abi:cxx11]() const`
76. 129,643 (0.12%) - `0x7f3e851a93e2`
   - caller: `std::filesystem::__cxx11::path::operator/=(std::filesystem::__cxx11::path const&)` (98.5% of previous frame, 0.11% total)
   - caller: `check_file_in_path(std::vector<> const&, std::filesystem::__cxx11::path const&)` (98.1% of previous frame, 0.11% total)
77. 129,600 (0.12%) - `get_distance(TreeInterface const&, int)`
78. 125,176 (0.11%) - `check_file_in_path(std::vector<> const&, std::filesystem::__cxx11::path const&)`
79. 122,431 (0.11%) - `Runtime::Exp::Exp(std::__cxx11::basic_string<>)`
80. 120,821 (0.11%) - `void std::__cxx11::basic_string<>::_M_construct<>(char const*, unsigned long)`
   - caller: `Program::module_names_set[abi:cxx11]() const` (43.4% of previous frame, 0.05% total)
81. 119,598 (0.11%) - `reg_heap::decrement_counts_from_invalid_step_demands(std::vector<> const&, std::vector<>&)`
82. 93,126 (0.08%) - `slice_sample_branch_length(owned_ptr<>&, MCMC::MoveStats&, int)`
83. 93,066 (0.08%) - `exp_large(Eigen::SelfAdjointEigenSolver<> const&, std::vector<> const&, double)`
84. 87,619 (0.08%) - `std::map<>::operator[](tree_edge const&)`
   - caller: `set_attachment_probability(spr_attachment_probabilities&, spr_attachment_points const&, tree_edge const&, tree_edge const&, Parameters, std::map<> const&, std::tuple<> const&, bool)` (97.0% of previous frame, 0.08% total)
85. 87,619 (0.08%) - `SPR_search_attachment_points(Parameters, tree_edge const&, std::map<> const&, spr_attachment_points const&, std::map<> const&, bool)`
86. 84,970 (0.08%) - `move_pruned_subtree(Parameters&, std::tuple<> const&, tree_edge const&, tree_edge, tree_edge const&, std::optional<> const&, bool)`
87. 78,000 (0.07%) - `context_ptr::head() const`
88. 75,627 (0.07%) - `Runtime::make_trim(Runtime::Exp const&, std::vector<> const&)`
89. 70,735 (0.06%) - `peel_muts_fixed_A(Runtime::RVector const&, alphabet const&, Runtime::RVector const&, bali_phy::matrix<> const&)`
90. 67,608 (0.06%) - `TreeInterface::directed_branches() const`
91. 65,926 (0.06%) - `immer::detail::hamts::node<>::make_merged(unsigned int, int, unsigned long, int, unsigned long)`
   - caller: `immer::detail::hamts::champ<>::do_add(immer::detail::hamts::node<>*, int, unsigned long, unsigned int) const [clone .isra.0]` (100.0% of previous frame, 0.06% total)
   - caller: `TreeInterface::reconnect_branch(int, int, int)` (84.3% of previous frame, 0.05% total)
92. 64,398 (0.06%) - `spr_info::spr_info(TreeInterface const&, tree_edge const&, std::map<> const&)`
93. 63,797 (0.06%) - `simple_function_append`
94. 62,005 (0.06%) - `find_file_in_path(std::vector<> const&, std::filesystem::__cxx11::path const&)`
95. 61,452 (0.05%) - `builtin_function_getEigensystemRaw`
96. 54,243 (0.05%) - `branch_pairs_after(TreeInterface const&, tree_edge const&, tree_edge const&, std::vector<>&, std::map<> const&) [clone .localalias]`
97. 54,118 (0.05%) - `std::vector<> range<>(int, int)`
98. 52,368 (0.05%) - `int choose_MH<>(int, std::vector<> const&)`
99. 50,454 (0.04%) - `Runtime::merge_vars(std::vector<> const&, std::vector<> const&)`
100. 48,702 (0.04%) - `MCMC::Result::inc(MCMC::Result const&)`

## Analysis of entries >= 0.5%

The entries below are sorted by expected usefulness as allocation-reduction
targets, not by allocation count. Each section names the profile row it covers
so it can be cross-checked against the ranking above.

### 17. `Runtime::Exp::Exp(bool)` (1.92%)

- Allocation: the old profile allocated a boxed nullary `ConstructorApp` for
  every Bool result. The dominant caller was `simple_function_cIsJust`.
- Purpose: represent Haskell `True` and `False` as runtime constructor values.
- Reduction difficulty: already addressed in the parent change by sharing static
  immutable Bool constructor expressions. This should disappear from a fresh
  heaptrack run.

### 11. `exp_small(...)` (3.18%)

- Allocation: the self allocations are the `DP` and `DN` temporary
  `std::vector<double>` objects used to rescale rows and columns after
  `expm1(...)`.
- Purpose: convert the symmetric reversible-chain exponential back into the
  original Markov-chain basis, then add the identity.
- Reduction difficulty: good target. The temporary vectors can probably be
  removed or reduced to one scratch vector. The tradeoff is whether to recompute
  square roots in the nested matrix loop or add an explicit reusable scratch
  buffer to the matrix-exponential API.

### 20. `Runtime::RVector::operator std::vector<>() const` (1.64%)

- Allocation: almost all calls come from `builtin_function_lExpRaw`, where the
  equilibrium frequencies are copied from `RVector` into `std::vector<double>`.
- Purpose: adapt runtime vector values to the math API used by transition-matrix
  exponentiation.
- Reduction difficulty: good target. Passing an `RVector` view, span-like
  accessor, or a small typed wrapper into `exp(...)` would avoid the conversion
  allocation. This is narrower and easier than redesigning all `RVector`
  conversions.

### 5. `builtin_function_list_to_vector` (4.08%)

- Allocation: it allocates a new `RVector` and pushes each evaluated Haskell
  list element into it. There is no capacity hint, so long lists also pay vector
  growth allocations.
- Purpose: materialize a Haskell list as a runtime vector while recording
  dynamic USE edges for the list spine and element values.
- Reduction difficulty: good target, but dependent-use tracking matters. A
  cheap length source would allow `reserve()`. A better fix is avoiding the
  list materialization in common producers and building an `RVector` directly.

### 12. `builtin_function_restrictKeysToVector` (2.88%)

- Allocation: it allocates an `RVector` result for selected `IntMap` values. It
  already reserves `keys.size()`, so the remaining allocation is mostly the
  result object/storage itself.
- Purpose: collect selected map values in key order while recording dynamic USE
  edges for the selected registers.
- Reduction difficulty: good but caller-dependent. The obvious local reserve
  win is already present. Further reductions require avoiding the intermediate
  vector in callers, reusing a result buffer, or replacing a map-to-vector
  pipeline with a direct vector-producing operation.

### 33. `reg_heap::do_pending_effect_unregistrations()` (0.72%)

- Allocation: `steps_pending_effect_unregistration` is copied into a temporary
  `std::vector` before iteration so the unordered set can be modified safely.
- Purpose: unregister delayed effects after rerooting or unsharing without
  invalidating the active set iteration.
- Reduction difficulty: good target. A reusable scratch vector, a drain loop, or
  a pending-list representation with stable iteration could remove the per-call
  vector allocation. The main caution is preserving the rule that handlers may
  mutate the pending set.

### 15. `reg_heap::inc_count(int)` (2.31%)

- Allocation: when a force count is first changed in a token,
  `tokens[t].vm_force_count.add_value(...)` grows the `mapping` vector.
- Purpose: record the old force count so rerooting can pivot or restore it.
- Reduction difficulty: good target. `mapping` currently uses `std::vector` for
  all deltas. Force-count deltas may be short enough for small-vector storage or
  a token-local scratch/arena. This touches the token pivoting contract, so it
  needs focused tests around rerooting and force counts.

### 8. `std::vector<>::emplace_back<>(int&, int&)` (3.50%)

- Allocation: this is `mapping::add_value(...)` growing the token delta vectors,
  mostly during `reg_heap::incremental_evaluate2_changeable_`.
- Purpose: record token-local mappings from registers to steps/results/force
  counts so evaluation can be rerooted and undone.
- Reduction difficulty: good target with moderate design cost. Many mappings are
  likely tiny, so replacing `mapping` storage with a small-vector form or
  reserving per-token deltas could help several rows at once. The risk is
  increasing token size enough to hurt memory locality.

### 27. `boost::container::vector<>::priv_insert_forward_range_no_capacity<>(use_force_edge*)` (1.09%)

- Allocation: `small_vector<use_force_edge, 2>` overflows in `set_used_reg` and
  `set_used_reg_for_step`.
- Purpose: record dependent USE/FORCE edges for invalidation and force-count
  maintenance.
- Reduction difficulty: good target if edge counts are commonly just above two.
  Raising the inline capacity or splitting uncommon edge kinds can remove
  heap allocations, but it increases every `reg`/`Step` size. Measure edge-count
  distributions before changing the inline capacity.

### 24. `boost::container::vector<>::priv_insert_forward_range_no_capacity<>(int*)` (1.43%)

- Allocation: `small_vector<int, 2>` overflows for previous-program-token
  reference lists, mainly in `reg_heap::set_prev_prog_token`.
- Purpose: maintain active/inactive backrefs from a previous program execution
  token to later tokens that can revert to it.
- Reduction difficulty: good to moderate. This may be improved by a larger
  inline capacity, a specialized intrusive list, or pooling these backrefs. The
  same size-vs-allocation tradeoff as USE edges applies.

### 10. `std::_Hashtable<>::_M_emplace_uniq<>(int const&)` (3.21%)

- Allocation: inserts into the pending effect registration/unregistration
  `std::unordered_set<int>` containers, mostly
  `mark_effect_to_unregister_at_step` and `mark_effect_to_register_at_step`.
- Purpose: remember effect steps whose registration state must be reconciled
  after graph changes.
- Reduction difficulty: good to moderate. Since each `Step` already has pending
  bits, the set may be replaceable with a vector/list plus duplicate prevention
  from those bits. That would also pair well with the
  `do_pending_effect_unregistrations()` allocation.

### 6. `reg_heap::set_reg_value(int, closure&&, int, bool)` (3.68%)

- Allocation: this path allocates or records a step and token deltas when setting
  modifiable values. The largest callers are `context_ref::set_modifiable_value`
  and `force_simple_set_path_to_PPET`.
- Purpose: install a new value for a modifiable register and update the token
  graph so the change can be rerooted, invalidated, or undone.
- Reduction difficulty: good but broad. Wins probably come from reducing
  repeated token-delta and step allocation rather than changing the public
  operation. This should be attacked after the lower-level `mapping` and
  pending-effect storage choices are clearer.

### 39. `std::vector<>::reserve(unsigned long)` (0.50%)

- Allocation: most calls are `LC.reserve(LCN.size() + LCB.size())` in
  `calc_prob_at_root_SEV`; a smaller portion comes from tree branch collection.
- Purpose: build a temporary combined CLV vector before computing the root
  likelihood.
- Reduction difficulty: good local target. `calc_prob_at_root_SEV` could avoid
  building `LC` when one side is empty, or iterate over the two source vectors
  through a small local accessor. A scratch vector is another option, but it
  would need clear lifetime ownership.

### 25. `substitution::calc_prob_at_root_SEV(...)` (1.33%)

- Allocation: this function builds temporary root-probability state: the
  combined `RVector LC`, a scratch `Matrix`, a `dynamic_bitset`, and the
  per-CLV index vector.
- Purpose: combine leaf/internal likelihood caches at the root and compute the
  final sequence probability.
- Reduction difficulty: good but needs design. The combined CLV vector and
  scratch objects are natural candidates for reuse or replacement with views.
  This is safer to do before introducing a wider likelihood-cache arena.

### 2. `substitution::peel_internal_branch_SEV(...)` (7.10%)

- Allocation: every internal branch peel allocates a new
  `Likelihood_Cache_Branch`, including its bitset, scale storage, CLV storage,
  and scratch.
- Purpose: produce the parent-side conditional likelihood vector after combining
  two child branch caches and applying transition matrices.
- Reduction difficulty: high-impact but harder. Reusing CLV storage, allocating
  from a per-evaluation arena, or returning into caller-owned storage could
  remove many allocations. The design must preserve cache invalidation and avoid
  retaining buffers across the wrong runtime context.

### 13. `substitution::peel_leaf_branch_SEV(SparseLikelihoods const&, ...)` (2.62%)

- Allocation: the sparse-leaf peel allocates a new `Likelihood_Cache_Branch` for
  the propagated leaf cache.
- Purpose: convert sparse observed leaf likelihoods into dense branch CLVs after
  applying transition probabilities.
- Reduction difficulty: high-impact but similar to internal peels. Leaf and
  internal peels should probably share the same storage-reuse design so that the
  cache lifetime model is tested consistently.

### 1. `expm1(Eigen::SelfAdjointEigenSolver<> const&, double)` (7.94%)

- Allocation: this function copies eigenvalues into an `Eigen::VectorXd`,
  allocates the result `Matrix`, and may allocate Eigen temporaries for
  `O * Diagonal(D) * O.transpose()`.
- Purpose: compute `exp(M) - I` from an eigensystem for small reversible-chain
  branch lengths.
- Reduction difficulty: high-impact but harder than `exp_small`. A caller-owned
  result matrix and scratch eigenvalue vector would help, but the API currently
  returns a fresh `Matrix`. This needs a transition-matrix lifetime design before
  editing.

### 22. `builtin_function_lExpRaw` (1.63%)

- Allocation: allocates `Box<Matrix>` for the transition matrix and constructs a
  `RMaybe` wrapper around it.
- Purpose: expose matrix exponentiation to the runtime as a fallible operation,
  returning `Nothing` for invalid eigensystems or bad transition matrices.
- Reduction difficulty: moderate. Constructing the matrix directly in final
  storage and avoiding extra wrapper copies would help. Eliminating the result
  object entirely is unlikely because callers need a runtime value.

### 21. `Runtime::RMaybe::clone() const` (1.64%)

- Allocation: cloning `RMaybe` allocates a new boxed optional runtime value,
  almost entirely from `builtin_function_lExpRaw`.
- Purpose: copy the `Maybe Matrix` runtime result when it is placed in a
  closure/object value.
- Reduction difficulty: moderate. This is best handled together with
  `builtin_function_lExpRaw`: return or move an already boxed value where
  possible, or avoid constructing a temporary object that must be cloned.

### 32. `builtin_function_register_prior` (0.76%)

- Allocation: allocates a `register_prior` effect object and a register for the
  effect closure.
- Purpose: make prior-density contributions visible to the incremental
  probability-ratio machinery.
- Reduction difficulty: moderate. The object is semantically real, but the
  effect representation may be made more compact or pooled. It is coupled to the
  `register_prior` hash-table entries below.

### 7. `std::_Hashtable<>::_M_emplace_uniq<>(std::pair<>&&)` (3.64%)

- Allocation: mostly `unordered_map` node allocation in `reg_heap::register_prior`
  and probability-ratio handlers. The same profile path also covers likelihood
  registration and `get_cost(TreeInterface const&)`.
- Purpose: store active prior/likelihood terms and temporary old/new probability
  maps while moving between contexts.
- Reduction difficulty: moderate. Reserving can reduce rehashing but not node
  allocation. Better targets are flat maps, vectors keyed by step id, or reusing
  probability-ratio work maps across calls.

### 30. `std::_Hashtable<>::_M_insert_unique_node(...)` (0.98%)

- Allocation: this is the lower-level hash-node insertion under the preceding
  `unordered_map::emplace` path, again mainly prior registration.
- Purpose: allocate and link individual hash-table nodes.
- Reduction difficulty: same as rank 7. Treat it as duplicate evidence for
  replacing or reusing the registration/probability-ratio hash tables, not as a
  separate fix.

### 26. `TreeInterface::partition(int) const` (1.19%)

- Allocation: builds an `std::unordered_set<int>` by collecting all nodes on one
  side of a branch. Most calls are from `subtree_contains`.
- Purpose: answer subtree membership and topology-move questions.
- Reduction difficulty: moderate and promising for topology moves. Many callers
  only need membership for one node, so a traversal predicate or cached
  partition bitset could avoid materializing a full set.

### 36. `TreeInterface::branches_after(int) const` (0.58%)

- Allocation: creates a small `std::vector<int>` for outgoing branches after a
  directed branch.
- Purpose: provide branch lists to topology-cost and SPR/NNI traversal code.
- Reduction difficulty: moderate. The local vector reserves four entries, so
  allocation remains when called frequently. Prefer append-style APIs, fixed
  small vectors, or direct iteration in hot callers such as `get_cost` and
  `branch_pairs_after`.

### 31. `TreeInterface::reconnect_branch(int, int, int)` (0.90%)

- Allocation: updates two persistent `immer::set<int>` adjacency sets with
  erase/insert operations and constructs updated runtime values.
- Purpose: modify tree topology by moving one directed branch endpoint.
- Reduction difficulty: moderate to hard. This is a real persistent data
  structure update. Reductions probably require batching the four topology
  field updates or using a transient tree-edit representation during proposal
  construction.

### 37. `reg_heap::find_set_regs_on_path(int) const` (0.55%)

- Allocation: builds a `std::vector<set_interchange_op>` while walking set tokens
  before forcing a simple set path.
- Purpose: preserve modifiable set/interchange operations while rewriting the
  path to a previous program execution token.
- Reduction difficulty: moderate. The vector could use small storage or be
  filled into caller-provided scratch. Semantics are subtle because the collected
  operations are replayed after the token path is changed.

### 28. `reg_heap::set_token_for_unset_context_(int, int)` (1.04%)

- Allocation: records context-to-token references, including growth of
  `context_refs` vectors when contexts are copied or moved to another token.
- Purpose: keep the token graph aware of which runtime contexts currently point
  at each token.
- Reduction difficulty: moderate to hard. This is core token ownership
  bookkeeping. Pooling or small-vector-like storage may help, but changing it
  risks lifetime bugs in reroot/copy-context behavior.

### 35. `reg_heap::reroot_at(int)` (0.61%)

- Allocation: rerooting rearranges token/register state and triggers vector or
  small-vector growth in the machinery it calls.
- Purpose: move the machine root so evaluation and modification happen relative
  to the requested context.
- Reduction difficulty: moderate to hard. This is likely better reduced by
  improving the lower-level token delta, pending-effect, and ref-list storage
  than by editing `reroot_at` directly.

### 29. `tree_constants::tree_constants(context_ref&, int)` (1.01%)

- Allocation: constructs maps/vectors of tree-node and tree-branch parameter
  references while walking the runtime tree object.
- Purpose: cache constant structural access paths for `TreeInterface` and
  `ModifiablesTreeInterface`.
- Reduction difficulty: moderate to hard. If these constants are rebuilt often
  for the same tree register, sharing or caching them could help. If they are
  mostly construction-time work, leave this behind loop-time interpreter fixes.

### 4. `immer::detail::hamts::champ<>::do_add(...)` (4.25%)

- Allocation: persistent HAMT node allocation from `immer::set`/`immer::map`
  insertion. In this profile it is essentially all from
  `builtin_function_mapNegate`.
- Purpose: build a new persistent set/map while preserving structural sharing.
- Reduction difficulty: harder. Batching with an immer transient builder or
  avoiding repeated set reconstruction can reduce allocations, but ordinary
  insertion must allocate to preserve persistence.

### 18. `immer::detail::hamts::node<>::make_inner_n(...)` (1.82%)

- Allocation: internal HAMT node creation under both insertion and deletion,
  split between `mapNegate`/tree reconnect insertion and deletion paths.
- Purpose: create persistent trie nodes needed by `immer` updates.
- Reduction difficulty: same class as rank 4. Do not treat it as a separate
  local fix; reduce the number of persistent set/map updates or use transient
  batch construction where semantics permit.

### 23. `immer::detail::hamts::champ<>::do_sub(...)` (1.46%)

- Allocation: persistent HAMT node allocation for deletion, dominated by
  `builtin_function_delete`.
- Purpose: produce a new persistent set/map without the deleted key.
- Reduction difficulty: harder. It may improve if callers avoid delete/insert
  pairs or batch topology edits. A direct library-level allocation removal is
  unlikely.

### 19. `Box<>::clone() const` (1.71%)

- Allocation: clones boxed objects, mostly from `builtin_function_mapNegate` and
  `builtin_function_delete` returning updated boxed persistent sets/maps.
- Purpose: copy runtime object values when closures or object values need their
  own boxed instance.
- Reduction difficulty: harder and should be driven by callers. The clone is
  often preserving value semantics. Reducing persistent set/map churn will
  likely reduce this row more safely than changing `Box::clone` globally.

### 3. `cereal::load(... PtrWrapper ...)` (5.18%)

- Allocation: cereal shared-pointer deserialization while loading cached modules
  and runtime AST/program metadata.
- Purpose: reconstruct serialized graphs with shared ownership and pointer
  identity.
- Reduction difficulty: lower priority for interpreter-run allocation unless
  startup/cache-load time is in scope. Potential fixes are cache-format changes,
  fewer shared pointers in serialized data, or pre-sized deserialization maps.

### 9. `cereal::InputArchive<>::registerSharedPointer(...)` (3.31%)

- Allocation: cereal's internal shared-pointer tracking table grows during
  deserialization.
- Purpose: preserve shared pointer identity while reading cached modules.
- Reduction difficulty: lower priority and same domain as rank 3. It probably
  needs serialization-format work rather than hot interpreter changes.

### 14. `void cereal::load(... std::variant ...)` (2.55%)

- Allocation: variant deserialization allocates as it reconstructs runtime AST
  alternatives and object graphs.
- Purpose: load cached runtime expressions/program data.
- Reduction difficulty: lower priority for MCMC loop allocation. Improve only if
  module-cache load time is a target or if the cache format is being redesigned.

### 16. `std::enable_if<` (2.18%)

- Allocation: unresolved/truncated cereal template frame in the same
  deserialization stack as ranks 3, 9, and 14.
- Purpose: generic archive loading helper work.
- Reduction difficulty: lower priority. Treat it as duplicate evidence that
  cache deserialization allocates heavily, not as an actionable standalone
  function.

### 38. `std::__cxx11::basic_string<>::_M_mutate(...)` (0.50%)

- Allocation: string growth while cereal loads strings, mostly under
  `cereal::load(... std::string&)` and pointer-wrapper deserialization.
- Purpose: reconstruct names, symbols, or paths from cached program/module data.
- Reduction difficulty: lower priority. It could improve with string interning or
  a cache format that stores repeated names more compactly, but this is startup
  oriented rather than hot interpreter execution.

### 34. `std::_Rb_tree<>::_M_insert_unique<>(std::pair<> const&)` (0.68%)

- Allocation: `std::map` node insertion inside `reflex::Pattern` regex
  compilation.
- Purpose: build DFA/prediction transition maps while compiling regular
  expressions.
- Reduction difficulty: least likely for interpreter-loop allocation. This is
  third-party/library regex setup work; only revisit if profile separation shows
  regex compilation occurring repeatedly during the run.

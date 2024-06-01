#ifndef DESUGARTYPE_H
#define DESUGARTYPE_H
TypeVar desugar(const Hs::LTypeVar& t);
TypeCon desugar(const Hs::LTypeCon& t);
Context desugar(const Hs::Context& c);
std::vector<TypeVar> desugar(const std::vector<Hs::LTypeVar>& t);

Type desugar(const Hs::LType& t);
std::vector<Type> desugar(const std::vector<Hs::LType>& t);

#endif

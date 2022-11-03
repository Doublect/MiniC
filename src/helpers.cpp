#include "helpers.hpp"

TypeSpecType least_constraining_value(TypeSpecType a, TypeSpecType b){
    return std::min(a, b);
}
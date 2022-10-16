#include <iostream>
#include <string>
#include <memory>
#include <vector>

#include "helpers.hpp"

#include "ast_print.hpp"

std::unique_ptr<ASTPrint> make_ast_print(std::string name, std::string var, std::vector<std::unique_ptr<ASTPrint>> &&children)
{
  return std::make_unique<ASTPrint>(ASTPrint(name, var, std::move(children)));
}

std::unique_ptr<ASTPrint> make_ast_children(std::string name, std::vector<std::unique_ptr<ASTPrint>> &&children)
{
  return unique_ptr_cast<ASTPrint>(ASTPrintChildren(name, std::move(children)));
}

std::unique_ptr<ASTPrint> make_ast_labelled(std::string name, std::unique_ptr<ASTPrint> &&child)
{
    return unique_ptr_cast<ASTPrint>(ASTPrintLabelled(name, std::move(child)));
}

std::unique_ptr<ASTPrint> make_ast_leaf(std::string name)
{
  return unique_ptr_cast<ASTPrint>(ASTPrintLeaf(name));
}
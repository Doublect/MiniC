#include "ast_print.hpp"

#include "helpers.hpp"

std::shared_ptr<ASTPrint> make_ast_print(std::string name, std::string var, std::vector<std::shared_ptr<ASTPrint>> &&children)
{
  return std::make_unique<ASTPrint>(ASTPrint(name, var, std::move(children)));
}

std::shared_ptr<ASTPrint> make_ast_children(std::string name, std::vector<std::shared_ptr<ASTPrint>> &&children)
{
  return unique_ptr_cast<ASTPrint>(ASTPrintChildren(name, std::move(children)));
}

std::shared_ptr<ASTPrint> make_ast_labelled(std::string name, std::shared_ptr<ASTPrint> &&child)
{
    return unique_ptr_cast<ASTPrint>(ASTPrintLabelled(name, std::move(child)));
}

std::shared_ptr<ASTPrint> make_ast_leaf(std::string name)
{
  return unique_ptr_cast<ASTPrint>(ASTPrintLeaf(name));
}
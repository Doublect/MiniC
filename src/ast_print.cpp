#include "ast_print.hpp"

#include "helpers.hpp"

const std::shared_ptr<ASTPrint> make_ast_print(const std::string name, const std::string var, const std::vector<std::shared_ptr<ASTPrint>> &&children)
{
  return std::make_shared<ASTPrint>(ASTPrint(name, var, std::move(children)));
}

const std::shared_ptr<ASTPrint> make_ast_children(const std::string name, const std::vector<std::shared_ptr<ASTPrint>> &&children)
{
  return shared_ptr_cast<ASTPrint>(ASTPrintChildren(name, std::move(children)));
}

const std::shared_ptr<ASTPrint> make_ast_labelled(const std::string name, const std::shared_ptr<ASTPrint> &&child)
{
    return shared_ptr_cast<ASTPrint>(ASTPrintLabelled(name, std::move(child)));
}

const std::shared_ptr<ASTPrint> make_ast_leaf(const std::string name)
{
  return shared_ptr_cast<ASTPrint>(ASTPrintLeaf(name));
}
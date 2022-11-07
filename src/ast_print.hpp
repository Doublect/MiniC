#ifndef AST_PRINT_H
#define AST_PRINT_H

#include <iostream>
#include <string>
#include <memory>
#include <vector>

class ASTPrint
{
    std::string name;
    std::string var;
    std::vector<std::shared_ptr<ASTPrint>> children;

public:
  ASTPrint() {};
  ASTPrint(
      const std::string name,
      const std::string var,
      const std::vector<std::shared_ptr<ASTPrint>> &&children) : name(name), var(var), children(std::move(children)) {}

  virtual void printAST(std::string indent = "", bool last = false)
  {
    std::cout << indent << (last ? "└─" : "├─") << name << " " << var << std::endl;

    indent = indent + (last ? "  " : "│ ");

    if (children.size() == 0)
        return;

    // TODO: vars
    for (int i = 0; i < this->children.size() - 1; i++)
    {
        children[i].get()->printAST(indent, false);
    }
    children.back().get()->printAST(indent, true);
  }
};

class ASTPrintChildren : public ASTPrint
{
  std::string name;
  std::vector<std::shared_ptr<ASTPrint>> children;

public:
  ASTPrintChildren(
    const std::string name,
    const std::vector<std::shared_ptr<ASTPrint>> &&children) : name(name), children(std::move(children)) {}

  virtual void printAST(std::string indent, bool last = false)
  {
    std::cout << indent << (last ? "└─" : "├─") << name << std::endl;

    indent = indent + (last ? "  " : "│ ");

    if (children.size() == 0)
        return;

    for (int i = 0; i < this->children.size() - 1; i++)
    {
        this->children[i].get()->printAST(indent, false);
    }
    this->children.back().get()->printAST(indent, true);
  }
};

class ASTPrintLabelled : public ASTPrint
{
  std::string name;
  std::shared_ptr<ASTPrint> child;

public:
  ASTPrintLabelled(
    const std::string name,
    const std::shared_ptr<ASTPrint> &&child) : name(name), child(std::move(child)) {}

  virtual void printAST(std::string indent, bool last = false)
  {
    std::cout << indent << (last ? "└─" : "├─") << name << std::endl;

    indent = indent + (last ? "  " : "│ ");

    child.get()->printAST(indent, true);
  }

};

class ASTPrintLeaf : public ASTPrint
{
  std::string name;

public:
  ASTPrintLeaf(std::string name) : name(name) {}

  virtual void printAST(std::string indent, bool last = false) 
  {
    std::cout << indent << (last ? "└─" : "├─") << name << std::endl;
  }
};


const std::shared_ptr<ASTPrint> make_ast_print(const std::string name, const std::string var = "", const std::vector<std::shared_ptr<ASTPrint>> &&children = {});
const std::shared_ptr<ASTPrint> make_ast_children(const std::string name, const std::vector<std::shared_ptr<ASTPrint>> &&children = {});
const std::shared_ptr<ASTPrint> make_ast_labelled(const std::string name, const std::shared_ptr<ASTPrint> &&child = {});
const std::shared_ptr<ASTPrint> make_ast_leaf(const  std::string name);

#endif // AST_PRINT_H
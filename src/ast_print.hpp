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
      std::string name,
      std::string var,
      std::vector<std::shared_ptr<ASTPrint>> &&children) : name(name), var(var), children(std::move(children)) {}

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
    std::string name,
    std::vector<std::shared_ptr<ASTPrint>> &&children) : name(name), children(std::move(children)) {}

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
    std::string name,
    std::shared_ptr<ASTPrint> &&child) : name(name), child(std::move(child)) {}

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


std::shared_ptr<ASTPrint> make_ast_print(std::string name, std::string var = "", std::vector<std::shared_ptr<ASTPrint>> &&children = {});
std::shared_ptr<ASTPrint> make_ast_children(std::string name, std::vector<std::shared_ptr<ASTPrint>> &&children = {});
std::shared_ptr<ASTPrint> make_ast_labelled(std::string name, std::shared_ptr<ASTPrint> &&child = {});
std::shared_ptr<ASTPrint> make_ast_leaf(std::string name);

#endif // AST_PRINT_H
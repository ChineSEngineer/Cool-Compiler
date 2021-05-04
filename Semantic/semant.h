#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include <sstream>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;
typedef SymbolTable<Symbol, Symbol> scope_t;
typedef SymbolTable<Symbol, method_class> m_scope_t;
typedef method_class *Method;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
public:
  Class_ cur_class;
  std::unordered_map<Symbol, Class_> ump;
  scope_t scope;
  m_scope_t m_scope;
  std::unordered_map<tree_node*, scope_t> scopetable;
  std::unordered_map<Class_, m_scope_t> m_scopetable;
  std::stringstream useless_stream;

private:
  int semant_errors;
  void install_basic_classes();
  std::unordered_map<Class_, Class_> father;
  std::unordered_map<Class_, std::vector<Class_>> son;
  ostream& error_stream;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  ostream& semant_error(tree_node *t);
  ostream& semant_debug(tree_node *t);
  void check_inheritance_tree();
  void traverse(Class_);
  void type_check(Class_);
  Symbol lca_query(Symbol a, Symbol b);
  bool sub_class(Symbol a, Symbol b);
  //bool sub_class(Class_ a, Class_ b);
};




#endif


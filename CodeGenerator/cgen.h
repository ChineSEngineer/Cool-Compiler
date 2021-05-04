#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

#include <unordered_map>
#include <functional>
#include <vector>
using std::unordered_map;
using std::string;
using std::vector;

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

struct ClassInfoCollector;
typedef attr_class Attr;
typedef method_class Method;
typedef attr_class* AttrP;
typedef method_class* MethodP;

class VarBinding;
typedef SymbolTable<Symbol, VarBinding> bind_t;

class DispBinding;
typedef SymbolTable<Symbol, DispBinding> disp_t;


class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   vector<CgenNodeP> nodes;
   ostream& str;
   int objectclasstag;
   int ioclasstag;
   int intclasstag;
   int boolclasstag;
   int stringclasstag;
   int size;
   int current_tag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   void code_name_table();
   void code_object_table();
   void code_dispatch_table(ClassInfoCollector& collector);
   void code_prototype_object(ClassInfoCollector& collector);
   void code_init_function(ClassInfoCollector& collector);

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
   void retag(CgenNodeP, int);
   void dfs(std::function<void(CgenNodeP, int)> func, CgenNodeP cur);
   void dfs(std::function<void(CgenNodeP, int)> func);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   int m_tag;
   int m_last_tag;
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   int tag() { return m_tag; }
   int last_tag() { return m_last_tag; }
   void set_tag(int tag) { m_tag = tag; }
   void set_last_tag(int tag) { m_last_tag = tag; }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};


template <class SYM, class DAT>
class SimpleTable {
public:
  unordered_map<SYM, vector<DAT>> ump;
private:
  vector<DAT> storage;
  vector<int> helper;
public:
  void enterscope() {
    helper.push_back(0);
  }
  void exitscope(const SYM& s) {
    snapshot(s);
    int count = storage.size();
    int tmp = helper.back();
    helper.pop_back();
    count -= tmp;
    storage.resize(count);
  }
  void addid(const DAT d) {
    helper.back() += 1;
    storage.push_back(d);
  }
  void snapshot(const SYM& s) {
    ump[s] = storage;
  }
  vector<DAT> collect(const SYM& s) {
    return ump.at(s);
  }
};

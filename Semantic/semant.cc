#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <stack>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;
int curr_lineno;
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

int method_class::find_main() {
  if (name == main_meth) {
    return 1;
  }
  return 0;
}

int attr_class::find_main() {
  return 0;
}

std::vector<Symbol> get_formal_types(Method m) {
  Formals f = m->get_formals();
  std::vector<Symbol> formal_type;
  for(int i = f->first(); f->more(i); i = f->next(i)) {
    formal_type.push_back(f->nth(i)->get_type());
  }
  return formal_type;
}

std::vector<Symbol> get_formal_names(Method m) {
  Formals f = m->get_formals();
  std::vector<Symbol> formal_name;
  for(int i = f->first(); f->more(i); i = f->next(i)) {
    formal_name.push_back(f->nth(i)->get_name());
  }
  return formal_name;
}

bool check_multiple_method_definition(Symbol name, ClassTable& table, tree_node* t) {
  if (table.m_scope.probe(name) != nullptr) {
    table.semant_error(t) << "Multiple definitions of methods:" << endl;
    return false;
  }
  return true;
}

bool check_multiple_formals(Symbol name, ClassTable& table, Method m) {
  std::vector<Symbol> formal_names = get_formal_names(m);
  std::unordered_set<Symbol> ust;
  for (size_t i = 0; i < formal_names.size(); ++i) {
    if (ust.find(formal_names[i]) != ust.end()) {
      table.semant_error(m) << "Formal parameter " << formal_names[i] << " is multiply defined." << endl;
      return false;
    } else {
      ust.insert(formal_names[i]);
    }
  }
  return true;

}

bool check_inherited_method_definition(Symbol name, ClassTable& table, Method cur_method) {
  Method pre_method = table.m_scope.lookup(name);
  if (pre_method == nullptr) {
    return true;
  }
  std::vector<Symbol> cur_method_types = get_formal_types(cur_method);
  std::vector<Symbol> pre_method_types = get_formal_types(pre_method);
  Symbol cur_method_return = cur_method->get_return_type();
  Symbol pre_method_return = pre_method->get_return_type();
  if (cur_method_return != pre_method_return) {
    table.semant_error(cur_method) << "In redefined method " << name << ", return type "
                                   << cur_method_return << " is different from original return type "
                                   << pre_method_return << endl;
    return false;
  }
  if (pre_method_types.size() != cur_method_types.size()) {
    table.semant_error(cur_method) << "Incompatible number of formal parameters in redefined method "
                                   << name << "." << endl;
    return false;
  }
  for (size_t i = 0; i < cur_method_types.size(); ++i) {
    if (cur_method_types[i] != pre_method_types[i]) {
      table.semant_error(cur_method) << "In redefined method " << name << ", parameter type "
                                     << cur_method_types[i] << " is different from original type "
                                     << pre_method_types[i] << endl;
      return false;
    }
  }

  return false;
}

bool check_inherited_definition(Symbol name, ClassTable& table, tree_node* t) {
  Symbol* typep = table.scopetable[t].lookup(name);
  if (typep != nullptr) {
    table.semant_error(t) << "Attribute " << name << " is an attribute of an inherited class." << endl;
    return false;
  }
  return true;
}
bool check_multiple_definition(Symbol name, ClassTable& table, tree_node* t) {
  Symbol* typep = table.scopetable[t].probe(name);
  if (typep != nullptr) {
    table.semant_error(t) << "Attribute " << name << " is multiply defined in class." << endl;
    return false;
  }
  return true;
}

Symbol lookup(Symbol name, ClassTable& table, tree_node* t) {
  Symbol* typep = table.scopetable[t].lookup(name);
  if (typep != nullptr) {
    return *typep;
  } else {
    table.semant_error(t) << "Undeclared identifier " << name << "." << endl;
    return Object;
  }
}

Method lookup_method(Symbol class_name, Symbol method_name, ClassTable& table, tree_node* t) {
  Method m = table.m_scopetable[table.ump[class_name]].lookup(method_name);
  if (m != nullptr) {
    return m;
  } else {
    table.semant_error(t) << "Dispatch to undefined method " << method_name << "." << endl;
    throw new std::exception();
  }
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
  install_basic_classes();

  for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ cur = classes->nth(i);
    Symbol name = cur->get_name();
    if (name == SELF_TYPE) {
        semant_error(cur) << "Redefinition of basic class SELF_TYPE." << endl;
    }
    if (ump.find(name) != ump.end()) {
      if (name == Object || name == Int || name == Bool || name == Str) {
        semant_error(cur) << "Redefinition of basic class " << name << "." << endl;
      } else {
        semant_error(cur) << "Class " << name << " was previously defined." << endl;
      }
    } else {
      ump[name] = cur;
    }
  }

  for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ cur = classes->nth(i);
    Symbol name = cur->get_name();
    Symbol parent = cur->get_parent();
    if (parent == SELF_TYPE) {
      semant_error(cur) << "Class " << name << " cannot inherit class SELF_TYPE." << endl;
      continue;
    }
    if (ump.find(parent) == ump.end()) {
      semant_error(cur) << "Class " << name << " inherits from an undefined class "
                        << parent << "." << endl;
      return;
    }
    Class_ p = ump[parent];

    if (parent == Bool || parent == Str || parent == Int) {
      semant_error(cur) << "Class " << name << " cannot inherit class " << parent << "." << endl;
    }
    if (name == No_class || parent == No_class) {
      semant_error(cur) << "no_class should not be used except the Object\n";
      exit(1);
    }

    if (father.find(cur) == father.end()) {
      father[cur] = p;
    } else {
      semant_error(cur) << "multiple inheritance\n";
      exit(1);
    }

    if (son.find(p) == son.end()) {
      son[p] = {cur};
    } else {
      son[p].push_back(cur);
    }
  }
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
        class_(Object, 
               No_class,
               append_Features(
                               append_Features(
                                               single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                                               single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                               single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
               filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
        class_(IO, 
               Object,
               append_Features(
                               append_Features(
                                               append_Features(
                                                               single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                                                                      SELF_TYPE, no_expr())),
                                                               single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                                                                      SELF_TYPE, no_expr()))),
                                               single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                               single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
               filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
        class_(Int, 
               Object,
               single_Features(attr(val, prim_slot, no_expr())),
               filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
        class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
        class_(Str, 
               Object,
               append_Features(
                               append_Features(
                                               append_Features(
                                                               append_Features(
                                                                               single_Features(attr(val, Int, no_expr())),
                                                                               single_Features(attr(str_field, prim_slot, no_expr()))),
                                                               single_Features(method(length, nil_Formals(), Int, no_expr()))),
                                               single_Features(method(concat, 
                                                                      single_Formals(formal(arg, Str)),
                                                                      Str, 
                                                                      no_expr()))),
                               single_Features(method(substr, 
                                                      append_Formals(single_Formals(formal(arg, Int)), 
                                                                     single_Formals(formal(arg2, Int))),
                                                      Str, 
                                                      no_expr()))),
               filename);

    ump[Object] = Object_class;
    ump[Str] = Str_class;
    ump[Bool] = Bool_class;
    ump[Int] = Int_class;
    ump[IO] = IO_class;
    father[IO_class] = Object_class;
    father[Int_class] = Object_class;
    father[Bool_class] = Object_class;
    father[Str_class] = Object_class;
    father[Object_class] = nullptr;
    son[Object_class] = {IO_class, Int_class, Bool_class, Str_class};
}



void ClassTable::check_inheritance_tree() {
  std::stack<Class_> stk;
  std::unordered_set<Class_> ust;
  stk.push(ump[Object]);
  bool find_main_class = false;
  while (!stk.empty()) {
    Class_ cur = stk.top();
    if (cur->get_name() == Main) {
      find_main_class = true;
    }
    stk.pop();
    if (ust.find(cur) != ust.end()) {
      semant_error(cur) << "there exists an inheritance circle\n";
      exit(1);
    } else {
      ust.insert(cur);
    }
    for (size_t i = 0; i < son[cur].size(); ++i) {
      stk.push(son[cur][i]);
    }
  }
  for (auto it = ump.begin(); it != ump.end(); ++it) {
    if (ust.find(it->second) == ust.end()) {
      Symbol class_name = it->second->get_name();
      semant_error(it->second) << "Class " << class_name << ", or an ancestor of " << class_name << ", is involved in an inheritance cycle." << endl;
    }
  }
  if (!find_main_class) {
    semant_error() << "Class Main is not defined." << endl;
  }
}

void ClassTable::traverse(Class_ cur) {
  scope.enterscope();
  m_scope.enterscope();
  cur->traverse(*this);
  for (size_t i = 0; i < son[cur].size(); ++i) {
    traverse(son[cur][i]);
  }
  m_scope.exitscope();
  scope.exitscope();
}

void ClassTable::type_check(Class_ cur) {
  cur->type_check(*this);
  for (size_t i = 0; i < son[cur].size(); ++i) {
    type_check(son[cur][i]);
  }
}


Symbol ClassTable::lca_query(Symbol as, Symbol bs) {
  if (as == SELF_TYPE && bs == SELF_TYPE) {
    return SELF_TYPE;
  }
  Class_ a = ump[as];
  Class_ b = ump[bs];
  std::unordered_set<Class_> ust;
  while (a != nullptr) {
    ust.insert(a);
    a = father[a];
  }
  while (b != nullptr) {
    if (ust.find(b) != ust.end()) {
      return b->get_name();
    }
    b = father[b];
  }
  return nullptr;
}

bool ClassTable::sub_class(Symbol type_a, Symbol type_b) {
  if (type_a == cur_class->get_name() && type_b == SELF_TYPE) {
    return false;
  }
  Class_ a = ump[type_a];
  Class_ b = ump[type_b];
  while (a != nullptr) {
    if (a == b) {
      return true;
    }
    a = father[a];
  }
  return false;
}



////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
//    All versions return an output stream to which you should write
//    an appropriate human-readable error message.
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 

ostream& ClassTable::semant_error(tree_node *t)
{
    if (cur_class == NULL) {
      cerr << "Error on semant_error" << endl;
      exit(1);
    }
    return semant_error(cur_class->get_filename(), t);
}

ostream& ClassTable::semant_debug(tree_node *t)
{
    if (cur_class == NULL) {
      std::clog << "Error on semant_error" << endl;
      exit(1);
    }
    return useless_stream;
    // semant_errors--;
    // return semant_error(cur_class->get_filename(), t);
}


/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants(); 
    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);
    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
    try {
      classtable->check_inheritance_tree();
      classtable->traverse(classtable->ump[Object]);
      classtable->type_check(classtable->ump[Object]);
    } catch (...) {
      
    }
    /* some semantic analysis code may go here */
    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

void class__class::traverse(ClassTable& classtable)
{
  classtable.cur_class = this;

  classtable.scope.addid(self, &SELF_TYPE);
  classtable.ump[SELF_TYPE] = this;

  int num_main = 0;
  for(int i = features->first(); features->more(i); i = features->next(i)) {
    features->nth(i)->traverse(classtable);
  }
  if (name == Main) {
    for(int i = features->first(); features->more(i); i = features->next(i)) {
      num_main += features->nth(i)->find_main();
    }
    if (num_main == 0) {
      classtable.semant_error(this) << "No 'main' method in class Main." << endl;
    }
  }

  classtable.m_scopetable[classtable.cur_class] = classtable.m_scope;
}

void method_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  classtable.scope.enterscope();

  if (classtable.ump.find(return_type) == classtable.ump.end()) {
    classtable.semant_error(this) << "Undefined return type " << return_type << " in method " << name << "." << endl;
    throw new std::exception();
  }

  check_multiple_method_definition(name, classtable, this);
  check_inherited_method_definition(name, classtable, this);
  check_multiple_formals(name, classtable, this);
  

  classtable.m_scope.addid(name, this);
  
  for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
    formals->nth(i)->traverse(classtable);
  }
  if (name == main_meth && formals->len() != 0) {
    classtable.semant_error(this) << "'main' method in class Main should have no arguments." << endl;
  }
  expr->traverse(classtable);

  classtable.scope.exitscope();
}

void attr_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;

  if (name == self) {
    classtable.semant_error(this) << "'self' cannot be the name of an attribute." << endl;
    throw std::exception();
  }
  if (check_multiple_definition(name, classtable, this)) {
    check_inherited_definition(name, classtable, this);
  }
  if (type_decl != prim_slot && classtable.ump.find(type_decl) == classtable.ump.end()) {
    classtable.semant_error(this) << "Undefined type: " << type_decl << endl;
  } else {
    classtable.scope.addid(name, &type_decl);
  }
  init->traverse(classtable);
}

void formal_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  if (name == self) {
    classtable.semant_error(this) << "'self' cannot be the name of a formal parameter." << endl;
  }
  if (type_decl == SELF_TYPE) {
    classtable.semant_error(this) << "Formal parameter " << name << " cannot have type SELF_TYPE." << endl;
  }
  if (type_decl != prim_slot && classtable.ump.find(type_decl) == classtable.ump.end()) {
    classtable.semant_error(this) << "Undefined type: " << type_decl << endl;
  } else {
    classtable.scope.addid(name, &type_decl);
  }
}

void branch_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  classtable.scope.enterscope();
  if (type_decl == SELF_TYPE) {
    classtable.semant_error(this) << "Identifier " << name
                                  << " declared with type SELF_TYPE in case branch." << endl;
  }
  if (classtable.ump.find(type_decl) == classtable.ump.end()) {
    classtable.semant_error(this) << "Class "<< type_decl
                                  << " of case branch is undefined." << endl;
    throw std::exception();
  } else {
    classtable.scope.addid(name, &type_decl);
  }
  expr->traverse(classtable);
  classtable.scope.exitscope();
}

void assign_class::traverse(ClassTable& classtable)
{
  if (name == self) {
    classtable.semant_error(this) << "Cannot assign to 'self'." << endl;
  }
  classtable.scopetable[this] = classtable.scope;
  expr->traverse(classtable);
}

void static_dispatch_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  expr->traverse(classtable);
  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->traverse(classtable);
  }
}

void dispatch_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->traverse(classtable);
  }
  expr->traverse(classtable);
}

void cond_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  pred->traverse(classtable);
  then_exp->traverse(classtable);
  else_exp->traverse(classtable);
}

void loop_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  pred->traverse(classtable);
  body->traverse(classtable);
}

void typcase_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  std::unordered_set<Symbol> case_types;

  expr->traverse(classtable);
  for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
    auto case_i = cases->nth(i);
    case_i->traverse(classtable);
    Symbol cur_type = case_i->get_case_type();
    if (case_types.find(cur_type) == case_types.end()) {
      case_types.insert(cur_type); 
    } else {
      classtable.semant_error(this) << "Duplicate branch " << cur_type
                                    << " in case statement." << endl;
    }
  }
}

void block_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  classtable.scope.enterscope();
  for(int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->traverse(classtable);
  }
  classtable.scope.exitscope();
}

void let_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  classtable.scope.enterscope();
  if (identifier == self) {
    classtable.semant_error(this) << "'self' cannot be bound in a 'let' expression." << endl;
  }
  if (classtable.ump.find(type_decl) == classtable.ump.end()) {
    classtable.semant_error(this) << "Class " << type_decl << " of let-bound identifier "
                                  << identifier <<" is undefined." << endl;
    throw new std::exception();
  } else {
    classtable.scope.addid(identifier, &type_decl);
  }
  init->traverse(classtable);
  body->traverse(classtable);
  classtable.scope.exitscope();
}

void plus_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  e1->traverse(classtable);
  e2->traverse(classtable);
}

void sub_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  e1->traverse(classtable);
  e2->traverse(classtable);
}

void mul_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  e1->traverse(classtable);
  e2->traverse(classtable);
}

void divide_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  e1->traverse(classtable);
  e2->traverse(classtable);
}

void neg_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  e1->traverse(classtable);
}

void lt_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  e1->traverse(classtable);
  e2->traverse(classtable);
}


void eq_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  e1->traverse(classtable);
  e2->traverse(classtable);
}

void leq_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  e1->traverse(classtable);
  e2->traverse(classtable);
}

void comp_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  e1->traverse(classtable);
}

void int_const_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
}

void bool_const_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
}

void string_const_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
}

void new__class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
}

void isvoid_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
  e1->traverse(classtable);
}

void no_expr_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
}

void object_class::traverse(ClassTable& classtable)
{
  classtable.scopetable[this] = classtable.scope;
}


void class__class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  classtable.cur_class = this;
  classtable.ump[SELF_TYPE] = this;

  for(int i = features->first(); features->more(i); i = features->next(i)) {
    features->nth(i)->type_check(classtable);
  }
}

void method_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
    formals->nth(i)->type_check(classtable);
  }
  expr->type_check(classtable);
  // Assume only primary type's method body can be no_expr
  if (expr->get_type() == No_type) {
    return;
  }
  Symbol t0_prime = expr->get_type();
  Symbol t0 = return_type;
  if (!classtable.sub_class(t0_prime, t0)) {
    classtable.semant_error(this) << "Inferred return type " << t0_prime << " of method " << name 
                                  << " does not conform to declared return type " << t0 << "." << endl;
  }
}

void attr_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  init->type_check(classtable);
  if (init->get_type() == No_type) {
    return;
  }
  Symbol rhs = init->get_type();
  Symbol lhs = type_decl;
  if (!classtable.sub_class(rhs, lhs)) {
      classtable.semant_error(this) << "Inferred type " << init->get_type() << " of initialization of attribute "
                                    << name << " does not conform to declared type " << type_decl << "." << endl;
      throw std::exception();
  } else {
      classtable.semant_debug(this) << "Inferred type " << init->get_type() << " of initialization of attribute "
                                    << name << " does not conform to declared type " << type_decl << "." << endl;
  }
}

void formal_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
}

void branch_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];

  expr->type_check(classtable);
  type = expr->get_type();
}

void assign_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
                                    
  expr->type_check(classtable);
  Symbol lhs = lookup(name, classtable, this);
  Symbol rhs = expr->type;
  if (!classtable.sub_class(rhs, lhs)) {
      classtable.semant_error(this) << "Type " << rhs << " of assigned expression does not conform to declared type " 
                                    << lhs << " of identifier " << name <<"." << endl;
      throw std::exception();
  } else {
      classtable.semant_debug(this) << "Type " << rhs << " of assigned expression does not conform to declared type " 
                                    << lhs << " of identifier " << name <<"." << endl;
      type = expr->type;
  }
  // cout << *(classtable.scope.lookup(name)) <<  "--->" << name << endl;
}

void static_dispatch_class::type_check(ClassTable& classtable)
{
  std::vector<Symbol> actual_type;
  std::vector<Symbol> formal_type;
  std::vector<Symbol> formal_name;
  scope_t scp = classtable.scopetable[this];

  expr->type_check(classtable);
  Symbol t0 = expr->get_type();
  if (!classtable.sub_class(t0, type_name)) {
    classtable.semant_error(this) << "Expression type " << t0
                                  << " does not conform to declared static dispatch type "
                                  << type_name << "." << endl;
  }

  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression actual_i = actual->nth(i); 
    actual_i->type_check(classtable);
    actual_type.push_back(actual_i->get_type());
  }

  Method m = lookup_method(type_name, name, classtable, this);
  formal_type = get_formal_types(m);
  formal_name = get_formal_names(m);
  Symbol t_return_prime = m->get_return_type();

  if (actual_type.size() != formal_type.size()) {
    classtable.semant_error(this) << "Method " << name << " called with wrong number of arguments." << endl;
  } else {
    for (size_t i = 0; i < actual_type.size(); ++i) {
      if (!classtable.sub_class(actual_type[i], formal_type[i])) {
          classtable.semant_error(this) << "In call of method " << name << ", type " << actual_type[i]
                                        << " of parameter " << formal_name[i] << " does not conform to declared type "
                                        << formal_type[i] << "." << endl;
      } else {
          classtable.semant_debug(this) << "In call of method " << name << ", type " << actual_type[i]
                                        << " of parameter " << formal_name[i] << " does not conform to declared type "
                                        << formal_type[i] << "." << endl;
      }
    }
  }
  if (t_return_prime == SELF_TYPE) {
    type = t0;
  } else {
    type = t_return_prime;
  }
}

void dispatch_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];

  std::vector<Symbol> actual_type;
  std::vector<Symbol> formal_type;
  std::vector<Symbol> formal_name;

  expr->type_check(classtable);
  Symbol t0 = expr->get_type();
  Symbol t0_prime = t0;
  // if (t0_prime == SELF_TYPE) {
  //   t0_prime = classtable.cur_class->get_name();
  // }
  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression actual_i = actual->nth(i); 
    actual_i->type_check(classtable);
    actual_type.push_back(actual_i->get_type());
  }

  Method m = lookup_method(t0_prime, name, classtable, this);
  formal_type = get_formal_types(m);
  formal_name = get_formal_names(m);
  Symbol t_return_prime = m->get_return_type();

  if (actual_type.size() != formal_type.size()) {
    classtable.semant_error(this) << "Method " << name << " called with wrong number of arguments." << endl;
  } else {
    for (size_t i = 0; i < actual_type.size(); ++i) {
      if (!classtable.sub_class(actual_type[i], formal_type[i])) {
          classtable.semant_error(this) << "In call of method " << name << ", type " << actual_type[i]
                                        << " of parameter " << formal_name[i] << " does not conform to declared type "
                                        << formal_type[i] << "." << endl;
      } else {
          classtable.semant_debug(this) << "In call of method " << name << ", type " << actual_type[i]
                                        << " of parameter " << formal_name[i] << " does not conform to declared type "
                                        << formal_type[i] << "." << endl;
      }
    }
  }
  if (t_return_prime == SELF_TYPE) {
    type = t0;
  } else {
    type = t_return_prime;
  }
}

void cond_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];

  pred->type_check(classtable);
  then_exp->type_check(classtable);
  else_exp->type_check(classtable);

  if (pred->type != Bool) {
    classtable.semant_error(this) << "Predicate of 'if' does not have type Bool." << endl;
  }
  type = classtable.lca_query(then_exp->get_type(),
                              else_exp->get_type());
}

void loop_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  pred->type_check(classtable);
  body->type_check(classtable);
  if (pred->type != Bool) {
    classtable.semant_error(this) << "Loop condition does not have type Bool." << endl;
  }
  // Should be void
  type = Object;
}

void typcase_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  expr->type_check(classtable);
  std::vector<Symbol> types;
  for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
    Case case_i= cases->nth(i);
    case_i->type_check(classtable);
    types.push_back(case_i->get_type());
  }

  Symbol res = types.at(0);
  for (size_t i = 1; i < types.size(); ++i) {
    res = classtable.lca_query(types.at(i), res);
  }
  type  = res;
}

void block_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  for(int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->type_check(classtable);
  }
  type = body->nth(body->len() - 1)->get_type();
}

void let_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];

  init->type_check(classtable);
  if (init->get_type() != No_type) {
    Symbol lhs = type_decl;
    Symbol rhs = init->get_type();
    if (!classtable.sub_class(rhs, lhs)) {
      classtable.semant_error(this) << "Inferred type " << rhs << " of initialization of "
                                    << identifier << " does not conform to identifier's declared type "
                                    << lhs << "." << endl;
      throw std::exception();
    } else {
      classtable.semant_debug(this) << "Inferred type " << rhs << " of initialization of "
                                    << identifier << " does not conform to identifier's declared type "
                                    << lhs << "." << endl;
    }
  }
  body->type_check(classtable);
  type = body->get_type();
}

void plus_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  e1->type_check(classtable);
  e2->type_check(classtable);
  if (e1->type != Int || e2->type != Int) {
    classtable.semant_error(this) << "non-Int arguments: " << e1->type
                                  << " + " << e2->type << endl;
  }
  type = Int;
}

void sub_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  e1->type_check(classtable);
  e2->type_check(classtable);
  if (e1->type != Int || e2->type != Int) {
    classtable.semant_error(this) << "non-Int arguments: " << e1->type
                                  << " - " << e2->type << endl;
  }
  type = Int;
}

void mul_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  e1->type_check(classtable);
  e2->type_check(classtable);
  if (e1->type != Int || e2->type != Int) {
    classtable.semant_error(this) << "non-Int arguments: " << e1->type
                                  << " * " << e2->type << endl;
  }
  type = Int;
}

void divide_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  e1->type_check(classtable);
  e2->type_check(classtable);
  if (e1->type != Int || e2->type != Int) {
    classtable.semant_error(this) << "non-Int arguments: " << e1->type
                                  << " / " << e2->type << endl;
  }
  type = Int;
}

void neg_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  e1->type_check(classtable);
  if (e1->type != Int) {
    classtable.semant_error(this) << "Argument of '~' has type " << e1->type << " instead of Int." << endl;
  }
  type = Int;
  
}

void lt_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  e1->type_check(classtable);
  e2->type_check(classtable);
  if (e1->type != Int || e2->type != Int) {
    classtable.semant_error(this) << "non-Int arguments: " << e1->type
                                  << " < " << e2->type << endl;
  }
  type = Bool;
}


void eq_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  e1->type_check(classtable);
  e2->type_check(classtable);
  if (e1->type == Int  || e2->type == Int  ||
      e1->type == Bool || e2->type == Bool ||
      e1->type == Str  || e2->type == Str) {
    if (e1->type != e2->type) {
      classtable.semant_error(this) << "Illegal comparison with a basic type." << endl;
    }
  }
  type = Bool;
}

void leq_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  e1->type_check(classtable);
  e2->type_check(classtable);
  if (e1->type != Int || e2->type != Int) {
    classtable.semant_error(this) << "non-Int arguments: " << e1->type
                                  << " <= " << e2->type << endl;
  }
  type = Bool;
}

void comp_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  e1->type_check(classtable);
  if (e1->type != Bool) {
    classtable.semant_error(this) << "Argument of 'not' has type " << e1->type << " instead of Bool." << endl;
  }
  type = Bool;
}

void int_const_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  type = Int;
}

void bool_const_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  type = Bool;
}

void string_const_class::type_check(ClassTable& classtable)
{
  type = Str;
  scope_t scp = classtable.scopetable[this];
}

void new__class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  type = type_name;
}

void isvoid_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  e1->type_check(classtable);
  type = Bool;
}

void no_expr_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  type = No_type;
}

void object_class::type_check(ClassTable& classtable)
{
  scope_t scp = classtable.scopetable[this];
  type = lookup(name, classtable, this);
}

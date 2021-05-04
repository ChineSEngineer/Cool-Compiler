
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

#include <stack>
#include <vector>
#include <unordered_set>
#include <stdexcept>
#include <sstream>
#include <algorithm>
using std::stack;
using std::pair;
using std::vector;
using std::max;
using std::stringstream;
using std::unordered_set;
using namespace std::placeholders;

#define RED   "\033[31m"      /* Red */
#define RESET "\033[0m"
#define CRED(str) std::cout << RED << str << RESET << std::endl;
#define EQ_TEST "equality_test"
#define DP_ABORT "_dispatch_abort"
#define CS_ABORT "_case_abort"
#define CS_ABORT_2 "_case_abort2"
// #define cout (cout << NORMAL)
// #define red_cout (cout << RED)

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;
int curr_lineno;
char* cgen_filename;

int heyi_debug = 0;
//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(const char *dest_reg, int offset, const char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(const char *source_reg, int offset, const char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(const char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(const char *dest_reg, const char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(const char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(const char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(const char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(const char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(const char *dest_reg, const char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(const char *dest, const char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(const char *dest, const char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(const char *dest, const char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(const char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(const char *address,ostream &s)
{ s << JAL << address << endl; }


static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(const char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(const char *src1, const char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(const char *src1, const char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(const char *src1, const char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(const char *src1, const char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(const char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(const char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(const char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(const char *dest, const char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(const char *source, const char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(const char *source, ostream &s)
{
  /*if (source != A1)*/ emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

static void emit_attr_ref(Symbol attr_type, ostream& s) {
  s << WORD;
  if (attr_type == Int) {
    inttable.lookup_string("0")->code_ref(s);
  } else if (attr_type == Str) {
    stringtable.lookup_string("")->code_ref(s);
  } else if (attr_type == Bool) {
    falsebool.code_ref(s);
  } else {
    s << 0;
  }
  s << endl;
}

static void emit_attr_ref_2(Symbol attr_type, ostream& s) {
  if (attr_type == Int) {
    inttable.lookup_string("0")->code_ref(s);
  } else if (attr_type == Str) {
    stringtable.lookup_string("")->code_ref(s);
  } else if (attr_type == Bool) {
    falsebool.code_ref(s);
  } else {
    s << 0;
  }
}


static void emit_protobj_table(Symbol class_name, vector<Symbol> attr_types,
                               int tag, ostream& s) {
  s << WORD << "-1" << endl;
  emit_protobj_ref(class_name, s); s << LABEL
        << WORD << tag << endl
        << WORD << (DEFAULT_OBJFIELDS + attr_types.size()) << endl
        << WORD; emit_disptable_ref(class_name, s); s << endl;
  for (Symbol attr_type : attr_types) {
    emit_attr_ref(attr_type, s);
  }
}

static void emit_method_ref_word(Symbol classname, Symbol methodname, ostream& s) {
  s << WORD;
  emit_method_ref(classname, methodname, s);
  s << endl;
}

static void emit_jal_func(Symbol classname, Symbol methodname, ostream &s)
{
  s << JAL;
  emit_method_ref(classname, methodname, s);
  s << endl;
}

static void emit_method_push(int tmp_word, ostream& str) {
  int ar_length = tmp_word * 4;
  emit_addiu(SP, SP, -ar_length , str);
  emit_store(FP, tmp_word, SP, str);
  emit_store(SELF, tmp_word - 1, SP, str);
  emit_store(RA, tmp_word - 2, SP, str);
  emit_addiu(FP, SP, 4, str);
  emit_move(SELF, ACC, str);
}

static void emit_method_pop(int n_param, int tmp_word, ostream& str) { 
  int pop_length = (tmp_word + n_param) * 4;
  emit_load(FP, tmp_word, SP, str);
  emit_load(SELF, tmp_word - 1, SP, str);
  emit_load(RA, tmp_word - 2, SP, str);
  emit_addiu(SP, SP, pop_length, str);
  emit_return(str);
}

class VarBinding {
public:
  virtual void update_var(char* reg, ostream&) = 0;
  virtual void read_var(ostream&) = 0;
};

class FormalBinding : public VarBinding {
public:
  int offset;
  MethodP current;
  unordered_map<tree_node*, int>& tmp_word;
  FormalBinding(int offset_,
                MethodP current_,
                unordered_map<tree_node*, int>& tmp_word_)
      : offset { offset_ }
      , current { current_ }
      , tmp_word { tmp_word_ } {}
  virtual void update_var(char* reg, ostream& str) {
    int n_tmp = tmp_word.at(current) + 3;
    emit_store(reg, offset + n_tmp, FP, str);
  }
  virtual void read_var(ostream& str) {
    int n_tmp = tmp_word.at(current) + 3;
    emit_load(ACC, offset + n_tmp , FP, str);
  }
};

class LocalBinding : public VarBinding {
public:
  int offset;
  LocalBinding(int local_count) : offset { local_count } {}
  virtual void update_var(char* reg, ostream& str) {
    // if (offset <= 6) {
    //   string temp_reg = string("s") + std::to_string(offset);
    //   emit_move(temp_reg.c_str(), reg, str);
    // } else {
    emit_store(reg, offset, FP, str);
    // }
  }
  virtual void read_var(ostream& str) {
    // if (offset <= 6) {
    //   string temp_reg = string("s") + std::to_string(offset);
    //   emit_move(ACC, temp_reg.c_str(), str);
    // } else {
    emit_load(ACC, offset, FP, str);
    // }
  }     
};


class AttrBinding : public VarBinding {
public:
  int offset;
  AttrBinding(int param_count) : offset { param_count } {}
  virtual void update_var(char* reg, ostream& str) {
    emit_store(reg, offset + DEFAULT_OBJFIELDS, SELF, str);
  }
  virtual void read_var(ostream& str) {
    emit_load(ACC, offset + DEFAULT_OBJFIELDS, SELF, str);
  }
};

class VarContext {
public:
  unordered_map<tree_node*, int> tmp_word;
  bind_t bind_table;

private:
  int local_count;
  int param_count;
  int attr_count;
  vector<int> attr_count_helper;

public:
  VarContext() : local_count{0}
               , param_count{0}
               , attr_count{0}
               , attr_count_helper() {}
  void enter_method() {
    local_count = 0;
    param_count = 0;
  }
  void enter_class() {
    attr_count_helper.push_back(0);
  }
  void exit_class() {
    attr_count -= attr_count_helper.back();
    attr_count_helper.pop_back();
  }
  void enter_scope() {
    bind_table.enterscope();
  }
  void exit_scope() {
    bind_table.exitscope();
  }
  void add_new_local(Symbol name, int nt) {
    bind_table.addid(name, new LocalBinding(nt));
    local_count += 1;
  }
  void add_new_formal(MethodP current, Symbol name) {
    // if (heyi_debug == 1) {
    //   cout << "[add_new_formal]" << current->get_name() << endl; 
    // }
    bind_table.addid(name, new FormalBinding(param_count, current, tmp_word));
    param_count += 1;
  }
  void add_new_attr(Symbol name) {
    bind_table.addid(name, new AttrBinding(attr_count));
    attr_count += 1;
    attr_count_helper.back() += 1;
  }
};

class DispBinding {
public:
  int offset;
  Class_ current;
  DispBinding(int offset_, Class_ current_)
      : offset { offset_ }
      , current { current_ } {}
  // load dispatch table from $ao
  void static_dispatch(Symbol type_name, ostream& str) {
    stringstream ss;
    emit_disptable_ref(type_name, ss);
    emit_load_address(T1, ss.str().c_str(), str);
    emit_load(T1, offset, T1, str);
    emit_jalr(T1, str);
  }
  void dispatch(ostream& str) {
    emit_load(T1, 2, ACC, str);
    emit_load(T1, offset, T1, str);
    emit_jalr(T1, str);
  }
};

class DispContext {
public:
  int method_count;
  disp_t disp_table;

  vector<int> method_count_helper;
  DispContext() : method_count{0} {}
  void enter_scope() {
    disp_table.enterscope();
  }
  void exit_scope() {
    disp_table.exitscope();
  }
  void enter_class() {
    method_count_helper.push_back(0);
  }
  void exit_class() {
    method_count -= method_count_helper.back();
    method_count_helper.pop_back();
  }
  void add_new_method(Symbol name, Class_ current) {
    DispBinding* db = disp_table.lookup(name);
    if (db == nullptr) {
      disp_table.addid(name, new DispBinding(method_count, current));
      method_count += 1;
      method_count_helper.back() += 1;
    } else {
      int count = db->offset;
      disp_table.addid(name, new DispBinding(count, current));
    } 
  }
};


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD << "String_dispTab"; 


 /***** Add dispatch information for class String ******/

      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl()) {
    l->hd()->code_def(s,stringclasstag);
  }
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD << "Int_dispTab"; 

 /***** Add dispatch information for class Int ******/

      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD << "Bool_dispTab";

 /***** Add dispatch information for class Bool ******/

      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}


//////////////////////////////////////////////////////////////////////////////
//
//  ClassInfoCollector
//
//////////////////////////////////////////////////////////////////////////////
struct ClassInfoCollector {
  SymbolTable<Symbol, Attr> m_attr_table;
  SimpleTable<Symbol, AttrP> m_attrs;
  SimpleTable<Symbol, pair<Class_, MethodP>> m_methods;
  unordered_map<CgenNodeP, SymbolTable<Symbol, Attr>> m_attr;
  unordered_map<Symbol, CgenNodeP> m_class;
  unordered_map<tree_node*, bind_t> bind;
  unordered_map<Class_, disp_t> disp;

  VarContext vc;
  DispContext dc;
  int m_label = 0;
  // Feature current_feature;
  CgenNodeP current_class;

  int label() {
    return m_label++;
  }

  void set_node_bind(tree_node* node) {
    bind[node] = vc.bind_table;
  }
  bind_t get_node_bind(tree_node* node) {
    return bind.at(node);
  }


  void operator()(CgenNodeP node, int state) {
    if (state == 2) {
      dc.exit_class();
      vc.exit_class();
      vc.exit_scope();
      dc.exit_scope();
      m_methods.exitscope(node->get_name());
      m_attr_table.exitscope();
      m_attrs.exitscope(node->get_name());
      return;
    } else if (state == 1) {
      m_attr_table.enterscope();
      m_attrs.enterscope();
      m_methods.enterscope();
      vc.enter_scope();
      dc.enter_scope();
      vc.enter_class();
      dc.enter_class();

      current_class = node;
      cgen_filename = node->filename->get_string();
      Features fs = node->features;
      for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
        Feature f = fs->nth(i);
        // current_feature = f;
        // tmp_word[f] = 0;
        if (f->is_method()) {
          auto pp = std::make_pair(node, dynamic_cast<MethodP>(f));
          m_methods.addid(pp);
        }
        if (f->is_attr()) {
          m_attr_table.addid(f->get_name(), dynamic_cast<AttrP>(f));
          m_attrs.addid(dynamic_cast<AttrP>(f));
        }
      }

      m_attr[node] = m_attr_table;
      m_class[node->get_name()] = node;
      
      node->collect(*this);
    }
  }
};



//////////////////////////////////////////////////////////////////////////////
//
//  AssemblyCoder
//
//////////////////////////////////////////////////////////////////////////////
struct AssemblyCoder {
private:
  ostream& m_str;
  ClassInfoCollector m_collector;
public:
  AssemblyCoder(ClassInfoCollector& collector, ostream& str)
        : m_str { str }
        , m_collector { collector } {}
  void operator()(CgenNodeP node, int state) {
    if (state == 2) {
      return;
    }
    if (node->basic()) {
      return;
    }
    m_collector.m_class[SELF_TYPE] = node;
    cgen_filename = node->filename->get_string();
    Features fs = node->features;
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
      Feature f = fs->nth(i);
      if (f->is_method()) {
        MethodP m = dynamic_cast<MethodP>(f);
        int tmp_word = m_collector.vc.tmp_word[m] + 3;

        emit_method_ref(node->get_name(), m->get_name(), m_str); m_str << LABEL;
        // cout << RED << node->get_name() << "." << m->get_name() << " ====== " << 4 * (m_collector.vc.tmp_word[m] + 3) << " " <<  RESET << endl;
        emit_method_push(tmp_word, m_str);
        m->expr->code(m_collector, 0, m_str);
        int n_param = m->formals->len();
        emit_method_pop(n_param, tmp_word, m_str);
      }
    }
  }
};

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

void CgenClassTable::code_name_table()
{
  str << CLASSNAMETAB << LABEL;
  nodes.resize(size);
  for(List<CgenNode> *l = nds; l; l = l->tl()) {
    nodes[l->hd()->tag()] = l->hd();
  }
  for (CgenNodeP np : nodes) {
    char* classname = np->get_name()->get_string();
    str << WORD; stringtable.lookup_string(classname)->code_ref(str); str << endl;
  }
}

void CgenClassTable::code_object_table()
{
  str << CLASSOBJTAB << LABEL;
  for (CgenNodeP np : nodes) {
    Symbol classname = np->get_name();
    str << WORD; emit_protobj_ref(classname, str); str << endl;
    str << WORD; emit_init_ref(classname, str); str << endl;
  }
}

void CgenClassTable::code_dispatch_table(ClassInfoCollector& collector) {
  for (CgenNodeP node : nodes) {
    {
      emit_disptable_ref(node->get_name(), str);
      str << LABEL;
      vector<pair<Class_, MethodP>> method_pairs = collector.m_methods.collect(node->get_name());
      vector<Symbol> method_names;
      unordered_set<Symbol> ust;
      for (size_t i = 0; i < method_pairs.size(); ++i) {
        pair<Class_, MethodP> method_pair = method_pairs.at(i);
        Symbol method_name = method_pair.second->get_name();
        Symbol class_name = method_pair.first->get_name();
        if (ust.find(method_name) != ust.end()) {
          continue;
        } else {
          ust.insert(method_name);
        }
        // for (size_t j = 0; j < method_pairs.size(); ++j) {
        for (size_t j = method_pairs.size(); j-- > 0;) {
          pair<Class_, MethodP> method_pair_2 = method_pairs.at(j);
          Symbol method_name_2 = method_pair_2.second->get_name();
          Symbol class_name_2 = method_pair_2.first->get_name();
          if (method_name_2 == method_name) {
            emit_method_ref_word(class_name_2, method_name, str);
            break;
          }
        }
      }
    }
  }
}

void CgenClassTable::code_prototype_object(ClassInfoCollector& collector) {
  for (CgenNodeP node : nodes) {
    vector<AttrP> attrs = collector.m_attrs.collect(node->get_name());
    vector<Symbol> attr_types;
    for (size_t i = 0; i < attrs.size(); ++i) {
      AttrP attr = attrs.at(i);
      attr_types.push_back(attr->get_type_decl());
    } 
    Symbol node_name = node->get_name();
    emit_protobj_table(node_name, attr_types, node->tag(), str);
  }
}


void CgenClassTable::code_init_function(ClassInfoCollector& collector) {
  for(CgenNodeP node : nodes) {
    int tmp_word = collector.vc.tmp_word[node] + 3;
    collector.m_class[SELF_TYPE] = node;

    emit_init_ref(node->get_name(), str); str << LABEL;
    // cout << RED << node->get_name() << " ====== " << 4 * (collector.vc.tmp_word[node] + 3) << " " <<  RESET << endl;
    emit_method_push(tmp_word, str);
    if (node->get_name() != Object) {
      stringstream ss;
      emit_init_ref(node->get_parentnd()->get_name(), ss);
      emit_jal(ss.str().c_str(), str);
    }
    Features fs = node->features;
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
      Feature f = fs->nth(i);
      if (f->is_attr()) {
        AttrP attr = dynamic_cast<AttrP>(f);
        attr->init->code(collector, 0, str);
        bind_t bind_table = collector.get_node_bind(attr);
        if (!dynamic_cast<no_expr_class*>(attr->init)) {
          if (heyi_debug == 1) {
            cout << "<<< " << attr->get_name() << " >>> ==> ";
          }
          bind_table.lookup(attr->get_name())->update_var(ACC, str);
        }
      }
    }
    emit_move(ACC, SELF, str);
    emit_method_pop(0, tmp_word, str); // init_function zero parameter
  }
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   // objectclasstag = 0;
   // ioclasstag =   1;
   // intclasstag =    2;
   // boolclasstag =   3;
   // stringclasstag = 4;
   size = 0;

   enterscope();
   if (cgen_debug) cout << "#Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();
   
   dfs(std::bind(&CgenClassTable::retag, this, _1, _2));
   code();
   exitscope();
}


void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this)); // CgenNode(prim_slot) add this clss to classNameTab

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i)) {
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
  }
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}


void CgenClassTable::retag(CgenNodeP node, int state) {
  if (state == 2) {
    node->set_last_tag(size - 1);
    // cout << RED << node->get_name() << " --- " << node->tag() << "," << node->last_tag() << RESET << endl;
    return;
  } else if (state == 1) {
    node->set_tag(size);

    Symbol n = node->get_name();
    if (n == Bool) {
      boolclasstag = size;
    } else if (n == Str) {
      stringclasstag = size;
    } else if (n == Int) {
      intclasstag = size;
    } else if (n == IO) {
      ioclasstag = size;
    } else if (n == Object) {
      objectclasstag = size;
    }

    size++;
    return;
  }
}



void CgenClassTable::dfs(const std::function<void(CgenNodeP, int)> func) {
  dfs(func, root());
}

void CgenClassTable::dfs(const std::function<void(CgenNodeP, int)> func, CgenNodeP cur)
{
  func(cur, 1);
  for (List<CgenNode> *l = cur->get_children(); l;l = l->tl()) {
    dfs(func, l->hd());
  }
  func(cur, 2);
}




void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
  if (cgen_debug) cout << "#-----------------------coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "#-----------------------choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "#-----------------------coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
  if (cgen_debug) cout << "#-----------------------coding name table" << endl;
  code_name_table();

  if (cgen_debug) cout << "#-----------------------coding object table" << endl;
  code_object_table();

  ClassInfoCollector collector;
  dfs(std::ref(collector));

  if (cgen_debug) cout << "#-----------------------coding dispatch table" << endl;
  code_dispatch_table(collector);

  if (cgen_debug) cout << "#-----------------------coding prototype object" << endl;
  code_prototype_object(collector);

  if (cgen_debug) cout << "#-----------------------coding global text" << endl;
  code_global_text();

  if (cgen_debug) cout << "#-----------------------coding init function" << endl;
  code_init_function(collector);

  if (cgen_debug) cout << "#-----------------------coding assembly" << endl;
  AssemblyCoder coder(collector, str);
  dfs(std::ref(coder));


//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   m_tag(-1),
   m_last_tag(-1),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   if (name != SELF_TYPE &&
       name != prim_slot &&
       name != No_class) {
     stringtable.add_string(name->get_string());          // Add class name to string table
   }
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

// void method_class::code(ClassInfoCollector& c, ostream& s) {
//   // emit_addiu(SP,SP, ,s);
//   // emit_store(FP,0,SP,s);
//   // emit_store(SELF,0,SP,s);
//   // emit_store(RA,0,SP,s);
//   // emit_move(SELF, ACC, s);
// 
//   expr->code(c,s);
// 
//   // emit_load(FP,0,SP,s);
//   // emit_load(SELF,0,SP,s);
//   // emit_load(RA,0,SP,s);
//   // emit_addiu(SP,SP, ,s);
// }
// 
// void attr_class::code(ClassInfoCollector& c, ostream &s) {
//   // Symbol name;
//   // Symbol type_decl;
//   // Expression init;
//   init->code(c, s);
// }


void assign_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Symbol name;
  // Expression expr;
  bind_t bind_table = c.get_node_bind(this);

  expr->code(c, nt, s);
  VarBinding* vb = bind_table.lookup(name);
  if (vb == nullptr) {
    throw std::runtime_error(string("VarBinding in assign not found: ") + name->get_string());
  }
  if (heyi_debug == 1) {
    cout << "<<< " << name << " >>> ==> ";
  }
  vb->update_var(ACC, s);
  
}

void static_dispatch_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expression expr;
  // Symbol type_name;
  // Symbol name;
  // Expressions actual;
  bind_t bind_table = c.get_node_bind(this);

  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(c, nt, s);
    emit_push(ACC, s);
  }
  expr->code(c, nt, s);
  int label = c.label();
  emit_bne(ACC, ZERO, label, s);
  emit_load_string(ACC, stringtable.lookup_string(cgen_filename), s);
  emit_load_imm(T1, line_number, s);
  emit_jal(DP_ABORT, s);
  emit_label_def(label, s);

  CgenNodeP disp_class = c.m_class[expr->get_type()];
  DispBinding* dp = c.disp[disp_class].lookup(name);
  if (dp == nullptr) {
    throw std::runtime_error(string("DispBinding in static dispatch not found: ") + name->get_string());
  }
  dp->static_dispatch(type_name, s);
}

void dispatch_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expression expr;
  // Symbol name;
  // Expressions actual;
  bind_t bind_table = c.get_node_bind(this);

  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(c, nt, s);
    emit_push(ACC, s);
  }
  expr->code(c, nt, s);
  int label = c.label();
  emit_bne(ACC, ZERO, label, s);
  emit_load_string(ACC, stringtable.lookup_string(cgen_filename), s);
  emit_load_imm(T1, line_number, s);
  emit_jal(DP_ABORT, s);
  emit_label_def(label, s);

  CgenNodeP disp_class = c.m_class.at(expr->get_type());
  DispBinding* dp = c.disp[disp_class].lookup(name);
  if (dp == nullptr) {
    throw std::runtime_error(string("DispBinding in dispatch not found: ") + name->get_string());
  }
  dp->dispatch(s);
}

void cond_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expression pred;
  // Expression then_exp;
  // Expression else_exp;
  bind_t bind_table = c.get_node_bind(this);

  int true_label = c.label();
  int false_label = c.label();

  pred->code(c, nt, s);
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_beqz(T1, false_label, s);
  then_exp->code(c, nt, s);
  emit_branch(true_label,s);
  emit_label_def(false_label, s);
  else_exp->code(c, nt, s);
  emit_label_def(true_label, s);
}

void loop_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expression pred;
  // Expression body;
  bind_t bind_table = c.get_node_bind(this);
  int start_label = c.label();
  int stop_label = c.label();
  emit_label_def(start_label, s);
  pred->code(c, nt, s);
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_beq(T1, ZERO, stop_label, s);
  body->code(c, nt, s);
  emit_branch(start_label, s);
  emit_label_def(stop_label, s);
  emit_move(ACC, ZERO, s);
}

void typcase_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expression expr;
  // Cases cases;
  bind_t bind_table = c.get_node_bind(this);

  expr->code(c, nt, s);

  int continue_label = c.label();
  int abort_label = c.label();
  vector<int> labels;
  vector<Case> cases_v;
  for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
    cases_v.push_back(cases->nth(i));
    labels.push_back(c.label());
  }
  labels.push_back(abort_label);
  auto cmp = [&](const Case c1, const Case c2) {
    Symbol type1 = c1->get_type_decl();
    Symbol type2 = c2->get_type_decl();
    CgenNodeP node1 = c.m_class[type1];
    CgenNodeP node2 = c.m_class[type2];
    return node1->tag() > node2->tag();
  };
  std::sort(cases_v.begin(), cases_v.end(), cmp);

  int cur = 0;
  emit_bne(ACC, ZERO, labels[cur], s);
  emit_load_string(ACC, stringtable.lookup_string(cgen_filename), s);
  emit_load_imm(T1, line_number, s);
  emit_jal(CS_ABORT_2, s);
  for (Case ca : cases_v) {
    CgenNodeP node = c.m_class[ca->get_type_decl()];
    emit_label_def(labels[cur++], s);
    emit_load(T2, 0, ACC, s);
    emit_blti(T2, node->tag(), labels[cur], s);
    emit_bgti(T2, node->last_tag(), labels[cur], s);
    ca->code(c, nt, s);
    emit_branch(continue_label, s);
  }
  emit_label_def(abort_label, s);
  emit_jal(CS_ABORT, s);
  emit_label_def(continue_label, s);
}

void branch_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Symbol name;
  // Symbol type_decl;
  // Expression expr;
  bind_t bind_table = c.get_node_bind(this);

  VarBinding* vb = bind_table.lookup(name);
  if (vb == nullptr) {
    throw std::runtime_error(string("VarBinding in let not found: ") + name->get_string());
  }
  vb->update_var(ACC, s);
  expr->code(c, nt + 1, s);
}

void block_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expressions body;
  bind_t bind_table = c.get_node_bind(this);

  for(int i = body->first(); body->more(i); i = body->next(i))
    body->nth(i)->code(c, nt, s);
}

void let_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Symbol identifier;
  // Symbol type_decl;
  // Expression init;
  // Expression body;
  if (dynamic_cast<no_expr_class*>(init)) {
    stringstream ss;
    emit_attr_ref_2(type_decl, ss);
    emit_load_address(ACC, ss.str().c_str(), s);
  } else {
    init->code(c, nt, s);
  }

  bind_t bind_table = c.get_node_bind(this);
  VarBinding* vb = bind_table.lookup(identifier);
  if (vb == nullptr) {
    throw std::runtime_error(string("VarBinding in let not found: ") + identifier->get_string());
  }
  vb->update_var(ACC, s);

  body->code(c, nt + 1, s);
}

void plus_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expression e1;
  // Expression e2;
  bind_t bind_table = c.get_node_bind(this);

  e1->code(c, nt, s);
  emit_store(ACC, nt, FP, s);
  e2->code(c, nt + 1, s);
  emit_jal_func(Object, ::copy, s);
  emit_fetch_int(T2, ACC, s);
  emit_load(T1, nt, FP, s);
  emit_fetch_int(T1, T1, s);
  emit_add(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
}

void sub_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expression e1;
  // Expression e2;
  bind_t bind_table = c.get_node_bind(this);

  e1->code(c, nt, s);
  emit_store(ACC, nt, FP, s);
  e2->code(c, nt + 1, s);
  emit_jal_func(Object, ::copy, s);
  emit_fetch_int(T2, ACC, s);
  emit_load(T1, nt, FP, s);
  emit_fetch_int(T1, T1, s);
  emit_sub(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
}

void mul_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expression e1;
  // Expression e2;
  bind_t bind_table = c.get_node_bind(this);

  e1->code(c, nt, s);
  emit_store(ACC, nt, FP, s);
  e2->code(c, nt + 1, s);
  emit_jal_func(Object, ::copy, s);
  emit_fetch_int(T2, ACC, s);
  emit_load(T1, nt, FP, s);
  emit_fetch_int(T1, T1, s);
  emit_mul(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
}

void divide_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expression e1;
  // Expression e2;
  bind_t bind_table = c.get_node_bind(this);

  e1->code(c, nt, s);
  emit_store(ACC, nt, FP, s);
  e2->code(c, nt + 1, s);
  emit_jal_func(Object, ::copy, s);
  emit_fetch_int(T2, ACC, s);
  emit_load(T1, nt, FP, s);
  emit_fetch_int(T1, T1, s);
  emit_div(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
}

void neg_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expression e1;
  bind_t bind_table = c.get_node_bind(this);

  e1->code(c, nt, s);
  emit_jal_func(Object, ::copy, s);
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);
  emit_store_int(T1, ACC, s);
}

void lt_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expression e1;
  // Expression e2;
  bind_t bind_table = c.get_node_bind(this);

  e1->code(c, nt, s);
  emit_store(ACC, nt, FP, s);
  e2->code(c, nt + 1, s);
  emit_fetch_int(T2, ACC, s);
  emit_load(T1, nt, FP,s);
  emit_fetch_int(T1, T1, s);
  emit_load_bool(ACC, BoolConst(1), s);
  int label = c.label();
  emit_blt(T1, T2, label, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(label, s);
}

void eq_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expression e1;
  // Expression e2;
  bind_t bind_table = c.get_node_bind(this);

  e1->code(c, nt, s);
  emit_store(ACC, nt, FP, s);
  e2->code(c, nt + 1, s);
  emit_move(T2, ACC, s);
  emit_load(T1, nt, FP,s);
  emit_load_bool(ACC, BoolConst(1), s);
  int label = c.label();
  emit_beq(T1, T2, label, s);
  emit_load_bool(A1, BoolConst(0), s);
  emit_jal(EQ_TEST, s);
  emit_label_def(label, s);
}

void leq_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expression e1;
  // Expression e2;
  bind_t bind_table = c.get_node_bind(this);

  e1->code(c, nt, s);
  emit_store(ACC, nt, FP, s);
  e2->code(c, nt + 1, s);
  emit_fetch_int(T2, ACC, s);
  emit_load(T1, nt, FP,s);
  emit_fetch_int(T1, T1, s);
  emit_load_bool(ACC, BoolConst(1), s);
  int label = c.label();
  emit_bleq(T1, T2, label, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(label, s);
}

void comp_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expression e1;
  bind_t bind_table = c.get_node_bind(this);
  
  int false_to_true_label = c.label();
  e1->code(c, nt, s);
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_load_bool(ACC, BoolConst(1), s);
  emit_beqz(T1, false_to_true_label, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(false_to_true_label, s);
}

void int_const_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Symbol token;
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  bind_t bind_table = c.get_node_bind(this);

  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Symbol token;
  bind_t bind_table = c.get_node_bind(this);

  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ClassInfoCollector& c, int nt, ostream &s) {
   // Boolean val;
  bind_t bind_table = c.get_node_bind(this);

  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Symbol type_name;
  bind_t bind_table = c.get_node_bind(this);
  if (type_name == SELF_TYPE) {
    emit_load_address(T1, CLASSOBJTAB, s);
    emit_load(T2, 0, SELF, s);
    emit_sll(T2, T2, 3, s);
    emit_addu(T1, T1, T2, s);
    // emit_move(S1, T1, s);
    emit_store(T1, nt, FP, s);
    emit_load(ACC, 0, T1, s);
    emit_jal_func(Object, ::copy, s);
    emit_load(T1, nt, FP, s);
    emit_load(T1, 1, T1, s);
    emit_jalr(T1, s);
  } else {
    stringstream ss_prot, ss_init;
    emit_protobj_ref(type_name, ss_prot);
    emit_load_address(ACC, ss_prot.str().c_str(), s);
    emit_jal_func(Object, ::copy, s);
    emit_init_ref(type_name, ss_init);
    emit_jal(ss_init.str().c_str(), s);
  }
}

void isvoid_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Expression e1;
  bind_t bind_table = c.get_node_bind(this);
  int void_label = c.label();
  e1->code(c, nt, s);
  emit_move(T1, ACC, s);
  emit_load_bool(ACC, BoolConst(1), s);
  emit_beqz(T1, void_label, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(void_label, s);
}

void no_expr_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  bind_t bind_table = c.get_node_bind(this);

}

void object_class::code(ClassInfoCollector& c, int nt, ostream &s) {
  // Symbol name;
  bind_t bind_table = c.get_node_bind(this);
  
  if (name == self) {
    emit_move(ACC, SELF, s);
  } else {
    VarBinding* vb = bind_table.lookup(name);
    if (vb == nullptr) {
      throw std::runtime_error(string("VarBinding in object_class not found: ") + name->get_string());
    }
    vb->read_var(s);
  }
}

//******************************************************************
//
//   
//   
//  Collect next temporary space 
//   
//   
//
//*****************************************************************

void class__class::collect(ClassInfoCollector& c) {
  // Symbol name;
  // Symbol parent;
  // Features features;
  // Symbol filenam());
  c.set_node_bind(this);

  c.vc.tmp_word[this] = 0;
  for(int i = features->first(); features->more(i); i = features->next(i)) {
    Feature f = features->nth(i);
    if (f->is_attr()) {
      c.vc.add_new_attr(f->get_name());
    } else if (f->is_method()) {
      c.dc.add_new_method(f->get_name(), c.current_class);
    }
  }
  for(int i = features->first(); features->more(i); i = features->next(i)) {
    Feature f = features->nth(i);
    if (f->is_attr()) {
      int feature_res = f->collect(c, 0);
      c.vc.tmp_word[this] += feature_res;
    }
  }
  for(int i = features->first(); features->more(i); i = features->next(i)) {
    Feature f = features->nth(i);
    if (f->is_method()) {
      int feature_res = f->collect(c, 0);
      c.vc.tmp_word[f] = feature_res; 
    }
  }
  c.disp[c.current_class] = c.dc.disp_table;
}

int method_class::collect(ClassInfoCollector& c, int nt) {
  // Symbol name;
  // Formals formals;
  // Symbol return_type;
  // Expression expr;
  c.set_node_bind(this);
  c.vc.enter_scope();
  c.vc.enter_method();

  // c.dc.add_new_method(name, c.current_class);
  vector<Formal> fs;
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    fs.push_back(formals->nth(i));
  }
  for (int i = fs.size() - 1; i >= 0; --i) {
    c.vc.add_new_formal(this, fs[i]->get_name());
  }

  int res = expr->collect(c, nt);

  c.vc.exit_scope();
  return res;
}

int attr_class::collect(ClassInfoCollector& c, int nt) {
  // Symbol name;
  // Symbol type_decl;
  // Expression init;
  c.set_node_bind(this);
  int res = init->collect(c, nt);
  return res;
}


int assign_class::collect(ClassInfoCollector& c, int nt) {
  // Symbol name;
  // Expression expr;
  c.set_node_bind(this);

  int res = expr->collect(c, nt);
  return res;
}

int static_dispatch_class::collect(ClassInfoCollector& c, int nt) {
  // Expression expr;
  // Symbol type_name;
  // Symbol name;
  // Expressions actual;
  c.set_node_bind(this);

  int res = 0;
  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression actual_parameter = actual->nth(i);
    int actual_res = actual_parameter->collect(c, nt);
    res = max(res, actual_res);
  }
  int expr_res = expr->collect(c, nt);
  return max(res, expr_res);
}

int dispatch_class::collect(ClassInfoCollector& c, int nt) {
  // Expression expr;
  // Symbol name;
  // Expressions actual;
  c.set_node_bind(this);

  int res = 0;
  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression actual_parameter = actual->nth(i);
    int actual_res = actual_parameter->collect(c, nt);
    res = max(res, actual_res);
    // push parameter
  }
  int expr_res = expr->collect(c, nt);
  return max(res, expr_res);
}

int cond_class::collect(ClassInfoCollector& c, int nt) {
  // Expression pred;
  // Expression then_exp;
  // Expression else_exp;
  c.set_node_bind(this);

  int pred_res = pred->collect(c, nt);
  int then_res = then_exp->collect(c, nt);
  int else_res = else_exp->collect(c, nt);
  return max(pred_res, max(then_res, else_res));
}

int loop_class::collect(ClassInfoCollector& c, int nt) {
  // Expression pred;
  // Expression body;
  c.set_node_bind(this);

  int pred_res = pred->collect(c, nt);
  int body_res = body->collect(c, nt);
  return max(pred_res, body_res);
}

int typcase_class::collect(ClassInfoCollector& c, int nt) {
  // Expression expr;
  // Cases cases;
  c.set_node_bind(this);

  int res = expr->collect(c, nt);
  for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
    int tmp_res = cases->nth(i)->collect(c, nt);
    res = max(tmp_res, res);
  }
  return res;
}

int branch_class::collect(ClassInfoCollector& c, int nt) {
  // Symbol name;
  // Symbol type_decl;
  // Expression expr;

  c.vc.enter_scope();
  c.vc.add_new_local(name, nt);
  c.set_node_bind(this);

  int res = expr->collect(c, nt + 1);

  c.vc.exit_scope();
  return res + 1;
}

int block_class::collect(ClassInfoCollector& c, int nt) {
  // Expressions body;
  c.set_node_bind(this);

  int res = 0;
  for(int i = body->first(); body->more(i); i = body->next(i)) {
    Expression expr = body->nth(i);
    int tmp_res = expr->collect(c, nt);
    res = max(res, tmp_res);
  }
  return res;
}

int let_class::collect(ClassInfoCollector& c, int nt) {
  // Symbol identifier;
  // Symbol type_decl;
  // Expression init;
  // Expression body;
  int init_res = init->collect(c, nt);

  c.vc.enter_scope();
  c.vc.add_new_local(identifier, nt);
  c.set_node_bind(this);

  int body_res = body->collect(c, nt + 1);

  c.vc.exit_scope();
  return max(init_res, body_res + 1);
}

int plus_class::collect(ClassInfoCollector& c, int nt) {
  // Expression e1;
  // Expression e2;
  c.set_node_bind(this);

  int e1_res = e1->collect(c, nt);
  int e2_res = e2->collect(c, nt + 1);
  return max(e1_res, e2_res + 1);
}

int sub_class::collect(ClassInfoCollector& c, int nt) {
  // Expression e1;
  // Expression e2;
  c.set_node_bind(this);

  int e1_res = e1->collect(c, nt);
  int e2_res = e2->collect(c, nt + 1);
  return max(e1_res, e2_res + 1);
}

int mul_class::collect(ClassInfoCollector& c, int nt) {
  // Expression e1;
  // Expression e2;
  c.set_node_bind(this);

  int e1_res = e1->collect(c, nt);
  int e2_res = e2->collect(c, nt + 1);
  return max(e1_res, e2_res + 1);
}

int divide_class::collect(ClassInfoCollector& c, int nt) {
  // Expression e1;
  // Expression e2;
  c.set_node_bind(this);

  int e1_res = e1->collect(c, nt);
  int e2_res = e2->collect(c, nt + 1);
  return max(e1_res, e2_res + 1);
}

int neg_class::collect(ClassInfoCollector& c, int nt) {
  // Expression e1;
  c.set_node_bind(this);

  int res = e1->collect(c, nt);
  return res;
}

int lt_class::collect(ClassInfoCollector& c, int nt) {
  // Expression e1;
  // Expression e2;
  c.set_node_bind(this);

  int e1_res = e1->collect(c, nt);
  int e2_res = e2->collect(c, nt + 1);
  return max(e1_res, e2_res + 1);
}

int eq_class::collect(ClassInfoCollector& c, int nt) {
  // Expression e1;
  // Expression e2;
  c.set_node_bind(this);

  int e1_res = e1->collect(c, nt);
  int e2_res = e2->collect(c, nt + 1);
  return max(e1_res, e2_res + 1);
}

int leq_class::collect(ClassInfoCollector& c, int nt) {
  // Expression e1;
  // Expression e2;
  c.set_node_bind(this);

  int e1_res = e1->collect(c, nt);
  int e2_res = e2->collect(c, nt + 1);
  return max(e1_res, e2_res + 1);
}

int comp_class::collect(ClassInfoCollector& c, int nt) {
  // Expression e1;
  c.set_node_bind(this);

  int res = e1->collect(c, nt);
  return res;
}

int int_const_class::collect(ClassInfoCollector& c, int nt) {
  // Symbol token;
  c.set_node_bind(this);

  return 0;
}

int string_const_class::collect(ClassInfoCollector& c, int nt) {
  // Symbol token;
  c.set_node_bind(this);

  return 0;
}

int bool_const_class::collect(ClassInfoCollector& c, int nt) {
   // Boolean val;
  c.set_node_bind(this);

  return 0;
}

int new__class::collect(ClassInfoCollector& c, int nt) {
  // Symbol type_name;
  c.set_node_bind(this);
  if (type_name == SELF_TYPE) {
    return 1;
  }

  return 0;
}

int isvoid_class::collect(ClassInfoCollector& c, int nt) {
  // Expression e1;
  c.set_node_bind(this);

  int res = e1->collect(c, nt);

  return res;
}

int no_expr_class::collect(ClassInfoCollector& c, int nt) {
  c.set_node_bind(this);

  return 0;
}

int object_class::collect(ClassInfoCollector& c, int nt) {
  // Symbol name;
  c.set_node_bind(this);

  return 0;
}


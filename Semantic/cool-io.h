//
// See copyright.h for copyright notice and limitation of liability
// and disclaimer of warranty provisions.
//
#include "copyright.h"

#ifndef COOL_IO_H
#define COOL_IO_H

//
// Cool files include this header to use the standard library's
// IO streams.  
//

# include <iostream>

using std::ostream;
using std::cout;
using std::cerr;
using std::endl;

# include <fstream>

using std::ofstream;

# include <iomanip>

using std::oct;
using std::dec;
using std::setw;
using std::setfill;

//Including the entire std namespace doesn't work well because of conflicts
//between e.g. std::plus and the plus AST node.
//using namespace std;


#endif //COOL_IO_H

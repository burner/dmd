
// Compiler implementation of the D programming language
// Copyright (c) 2004-2007 by Digital Mars
// All Rights Reserved
// written by Walter Bright
// http://www.digitalmars.com
// License for redistribution is by either the Artistic License
// in artistic.txt, or the GNU General Public License in gnu.txt.
// See the included readme.txt for details.


#include <stdio.h>
#include <stddef.h>
#include <time.h>
#include <assert.h>

#include "mars.h"
#include "module.h"
#include "mtype.h"
#include "declaration.h"
#include "statement.h"
#include "enum.h"
#include "aggregate.h"
#include "init.h"
#include "attrib.h"
#include "id.h"
#include "import.h"
#include "template.h"

#include "rmem.h"
#include "cc.h"
#include "global.h"
#include "oper.h"
#include "code.h"
#include "type.h"
#include "dt.h"
#include "cv4.h"
#include "cgcv.h"
#include "outbuf.h"
#include "irstate.h"

/****************************
 * Emit symbolic debug info in Dwarf2 format.
 */

void toDebug(TypedefDeclaration *tdd)
{
    //printf("TypedefDeclaration::toDebug('%s')\n", tdd->toChars());
}


void toDebug(EnumDeclaration *ed)
{
    //printf("EnumDeclaration::toDebug('%s')\n", ed->toChars());
}


void toDebug(StructDeclaration *sd)
{
}


void toDebug(ClassDeclaration *cd)
{
}

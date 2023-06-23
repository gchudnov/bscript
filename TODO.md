- TODO: start horizontal execution:
        - evaluate 2
        - evaluate 2 + 2

https://toylang.com/
https://www.freecodecamp.org/news/the-programming-language-pipeline-91d3f449c919/

https://github.com/faiface/generics/blob/master/go/ast/ast.go
https://softwareengineering.stackexchange.com/questions/334167/how-are-generics-implemented-in-a-modern-compiler

+ books on compiler design


-------------
Creating and populating symbol tables for each scope

A symbol table contains a record of all the names that are declared for a scope. There is
one symbol table for each scope. A symbol table provides a means of looking up symbols
by their name to obtain information about them. If a variable was declared, the symbol
table lookup returns a record with all the information known about that symbol: where
it was declared, what its data type is, whether it is public or private, and so on. All this
information can be found in the syntax tree. If we also place it in a table, the goal is to
access the information directly, from anywhere else that information is needed.

The traditional implementation of a symbol table is a hash table, which provides a very
fast information lookup. For example, `symtable[sym]`

-------------
Adding semantic attributes to syntax trees

information is stored in extra fields in tree nodes, commonly called semantic attributes.

scope -> symbolTable

An important method in this class, `symbolTable::insert()`, issues a
semantic error if the symbol is already in the table. Otherwise, insert() allocates a
symbol table entry and inserts it.

symbolTable::lookup(scope: String): SymbolTableEntry

SymbolTableEntry -- contains several data fields and no code

SymbolTableEntry::isConst: Boolean -- ???

**Every node in the syntax tree needs to know what symbol table it belongs to.**

```java
void mkSymTables(symtab curr) {
   stab = curr;
   switch (sym) {
   case "ClassDecl": curr = new symtab("class", curr);
   break;
   case "MethodDecl": curr = new symtab("method", curr);
   break;
   }
   for (int i=0; i<nkids; i++) kid[i].mkSymTables(curr);
}
```

The root of the entire parse tree starts with a global symbol table with predefined symbols such as `System` and `java`.

`global_st = symtab("global");` a global symbol table

```java
void semantic(tree root) {
   symtab out_st, System_st;
   global_st = symtab("global");
   out_st = symtab("class");
   System_st = symtab("class");
   out_st.insert("println", false);
   System_st.insert("out", false, out_st);
   global_st.insert("System", false, System_st);
   root.mkSymTables(global_st);
   root.populateSymTables();
   root.checkSymTables();
   global_st.print();
}

///
public class symtab_entry {
   String sym;
   symtab parent_st, st;
   boolean isConst;
   symtab_entry(String s, symtab p, boolean iC) {
   sym = s; parent_st = p, isConst = iC; }
   symtab_entry(String s, symtab p, boolean iC, symtab t) {
   sym = s; parent_st = p; isConst = iC; st = t; }
}

void insert(String s, Boolean iC, symtab sub) {
   if (t.containsKey(s)) {
      j0.semerror("redeclaration of " + s);
   } else {
      sub.parent = this;
      t.put(s, new symtab_entry(s, this, iC, sub));
   }
}

void insert(String s, Boolean iC) {
   if (t.containsKey(s)) {
      j0.semerror("redeclaration of " + s);
   } else {
      t.put(s, new symtab_entry(s, this, iC));
   }
}
```

Populating (inserting symbols into) symbol tables can be done during the same tree
traversal in which those symbol tables are created. However, the code is simpler in a
separate traversal. **Every node knows what symbol table it lives within**. The challenge is to
identify which nodes introduce symbols.

**isConst** is a classic example of a **synthesized** attribute. Its calculation rules depend
on whether a node is a leaf (following the base case) or an internal node (using the
recursion step):

Base case: For tokens, literals are isConst=true and everything else is
isConst=false.
• Recursion step: For internal nodes, isConst is computed from children, but only
through the expression grammar, where expressions have values.

**const functions?** e.g. `constexpr int add(const int, const int) { return a + b; })` -- a function that can be evaluated at compile time

///
## Types
Frequently, our compiler will need to do things such as compare the types of two variables to see whether they are compatible.
Program source code represents types with string data, which is incorporated in our syntax tree.

The type information associated with any name or value in your language can be represented within a new class named `typeinfo`.
Complex types have additional information as needed. For example, a type whose basetype indicates that it is an array has an additional `element_type`.
  - For Jzero, we will add `arraytype`, `methodtype`, and `classtype` as subclasses of `typeinfo`.
  - In addition to the basetype member, the typeinfo class has methods to facilitate debugging. Types need to be able to print themselves in a human-readable format.
  - arrays
  - Method types contain a list of zero or more parameters and a return type
    - `parameters` could be an array of `typeinfo`. A separate class is defined for parameters here to allow languages to include parameter names along with their types to represent methods.

```java
// arrays
public class arraytype extends typeinfo {
   typeinfo element_type;
   public arraytype(typeinfo t) {
      basetype = "array"; element_type = t; 
   }
}

// Method types contain a list of zero or more parameters and a return type
public class methodtype extends typeinfo {
   parameter [] parameters;
   typeinfo return_type;
   methodtype(parameter [] p, typeinfo rt){
      parameters = p; return_type = rt;
   }
}

// parameter
public class parameter {
   String name;
   typeinfo param_type;
   parameter(String s, typeinfo t) { name=s; param_type=t; }
}

// class
public class classtype extends typeinfo {
   String name;
   symtab st;
   parameter [] methods;
   parameter [] fields;
   typeinfo [] constrs;
}

// use

class tree { . . .
   typeinfo typ; . . . }

class symtab_entry { . . .
   typeinfo typ; . . . }
```

**Assigning type information to declared variables:**
Type information is constructed during a tree traversal and then stored with its associated variables in the symbol table.

**Runtime type checks and type inference:**
A descriptor is a struct that contains a value plus an extra word of memory that encodes its type, called the `d-word`.

**how to perform type checks for the arrays, parameters, and return types of method calls:**
The recursive call to check_types() on the arrays' element types

**Checking the type at return statements**:

```java
method populateSymTables()
   case sym of {
      . . .
      "MethodDecl": {
      stab.insert(kids[1].kids[2].kids[1].tok.text, ,
      kids[1].stab, kids[1].kids[2].typ)
      kids[1].stab.insert("return", , ,
      kids[1].kids[1].typ)
   }

///
...
   Case "ReturnStmt":
      symtab_entry ste;
      if ((ste=stab.lookup("return")) == null)
         j0.semerror("stab did not find a returntype");
         typeinfo rt = ste.typ;
         if (kids[0].typ != null)
         typ = check_types(rt, kids[0].typ);
      else {
         if (!rt.str().equals("void"))
         j0.semerror("void return from non-void method");
         typ = rt;
      }
   break;
```

**Checking types at instance creation:**

```
method semantic(root)
   ...
   root.mkSymTables(global_st)
   root.populateSymTables()
   root.checkSymTables()
   root.mkcls()
   root.checktype()
```

The `mkcls()` method stands for make class. When it sees a class declaration, it looks up the class name and goes through the class symbol table, putting entries into the correct category. There is one list for fields, one for methods, and one for constructors.


## Phases
- **Creating and populating symbol tables for each scope**
- **Checking for undeclared variables**
   - To find undeclared variables, check the symbol table on each variable that's used for assignment or dereferencing.
- **Finding redeclared variables**
  - report an error if the same variable is declared again in the same scope.
    - Inserting symbols into the symbol table. A symbol table lookup is performed before insertion. If the symbol is already present, a redeclaration error is reported.
    - Identifying redeclaration errors occurs most naturally while the symbol table is being populated; that is, when an attempt is being made to insert a declaration.
- **Checking Base Types**
  - is a key aspect of semantic analysis that must be performed before you can generate code.
  - A byproduct of checking the types is to add type information to the syntax tree.
  - Determining the type at each syntax tree node
    - he class for syntax tree nodes has an attribute to store that node's type, if there is one.
  - Runtime type checks and type inference
- **Checking method calls**
  - The parameters and return type of a method are called its **signature**.
  - the method is looked up in the symbol table and its type is retrieved.
  - If there are actual parameters in the call, they are checked against the formal parameters via a call to the `cksig()` method.
- **Checking the type at return statements**
  - The type of the expressions in the method's return statements must match the type's declared return type.
    These two locations are quite some distance apart in the syntax tree.
  - The symbol table is the most convenient way to connect remote locations.
    - We can insert a dummy symbol into the symbol table that can hold a function's return type.
      - The dummy symbol named `return` is ideal.
- **Checking types at instance creation**
- **Checking types at instance accesses**
  - Instance accesses refer to references to the fields and methods of an object.
  - Implicit accesses are handled by regular symbol table lookups in the current scope, which will automatically try to enclose scopes, including the class scope where the current object's class methods and variables can be found.

p.171

Symbol + Type -- do we need them at all in our code base?

**Mangling names**

A name such as `foo`, if it is found to be in package scope for a package `bar`, is written out in the generated code as `bar__foo`.

all methods are non-static and method calls always have an implicit first parameter named `self`, which is a reference to the object that the method has been invoked on.

**Testing and debugging symbol tables**

To output the symbol table, you need to output information for the table and then visit all the children, not just look one up by name.


**Symbol-Table Management**
- An essential function of a compiler is to record the variabe names used in the source program and collect information about various attributes of each name.
- The symbol table is a data structure containing a record for each variable name, with elds for the attributes of the name.
- READ: p278 in `Engineering_a_Compiler_2nd_Edition`.


**Building a Symbol Table**
The symbol table defines two interface routines for the rest of the compiler.
   1. `LookUp(name)` returns the record stored in the table at h(name) if one
   exists. Otherwise, it returns a value indicating that name was not
   found.
   2. `Insert(name,record)` stores the information in record in the table at
   h(name). It may expand the table to accommodate the record for name.

**Scope Building**
The compiler needs a call that initializes a new symbol table for a scope and one that finalizes
the table for a scope.
   1. `InitializeScope()` increments the current level and creates a new
   symbol table for that level. It links the new table to the enclosing level’s
   table and updates the current level pointer used by LookUp and
   Insert.
   2. `FinalizeScope()` changes the current-level pointer so that it points to
   the table for the scope surrounding the current level and then decrements
   the current level. If the compiler needs to preserve the level-by-level
   tables for later use, FinalizeScope can either leave the table intact in
   memory or write the table to external media and reclaim its space.
   To account for lexical scoping, the parser calls InitializeScope each time
   it enters a new lexical scope and FinalizeScope each time it exits a lexical scope.

In reality, compilers build multiple symbol tables that they use for different purposes.

In principle, every procedure call gives rise to a new `ar` (activation record).




- TODO: 
 *      |                +- Decl +- MethodDecl // TODO: refactor declarations (?), see cpp2
 *      |                |       +- StructDecl
 *      |                |       +- VarDecl
 *      |                |       +- TypeDecl


TODO: check an example of small languages


- TODO: add generics? (a primitive ones)

https://mytechshares.com/2022/04/10/gopher-should-know-goast/
https://dev.to/nakabonne/take-a-walk-the-go-ast-e02
https://blog.bradleygore.com/2022/04/18/ast-in-go-p1/
https://yuroyoro.github.io/goast-viewer/index.html

https://github.com/golang/exp/tree/master/typeparams/example


- TODO: How to init a struct in AST?? ^^^


- TODO: Add Comment to AST

- TODO: NOT CLEAR HOW TO REPRESENT A VECTOR

- TODO: HOW TO ADD constraints to generics?

- TODO: fix tests
- TODO: example:
  - add(int, int) -- +
  - add(double, int) --- +
  - add(string, int) -- +
  - add[T] -- +
  - someFunc[T, U] -- +

- TODO: reduce size of AST by extracting extra params to meta
- TODO: move some operations to functions

- TODO: add RetType(methodId), DeclType(expr) ??? -- not clear if we need it at all


https://eed3si9n.com/intro-to-scala-3-macros/
https://softwaremill.com/scala-3-macros-tips-and-tricks/


https://github.com/biboudis/dotty/blob/613a4193b3c705a21209f7d63ef79ad77be02d7e/library/src/scala/tasty/reflect/TreeAccumulator.scala#L15

- TODO: it is not possible distinguish between TypeRef("auto") and type-ref that references variable (see scala ast)

- TODO: allow operations on BigDecimal from java: + > < ...

- TODO: add code that checks for dead conditions: (xxx == false) --- add an option 'check' ?? or do we want to check during building / running phase ??
        ^ comparison must be inside of IF

- TODO: add unreachable code check
- TODO: do not allow to redefine the var in the same block | OR is that OK?? shadowing ???
- TODO: do not allow to redefine function in the same block, see: bscript/builder/src/main/scala/com/github/gchudnov/bscript/builder/state/Meta.scala
- TODO: add stdout | printf

/*
https://yuroyoro.github.io/goast-viewer/index.html

package main

import (
	"fmt"
)

type B struct {
	K3 string
}

type A struct {
	K1 int
	K2 B
}

func main() {
	s := A{
		K1: 123,
		K2: B{
			K3: "Alice",
		},
	}

	fmt.Println(s)
}


     0  *ast.File {
     1  .  Doc: nil
     2  .  Package: foo:1:1
     3  .  Name: *ast.Ident {
     4  .  .  NamePos: foo:1:9
     5  .  .  Name: "main"
     6  .  .  Obj: nil
     7  .  }
     8  .  Decls: []ast.Decl (len = 4) {
     9  .  .  0: *ast.GenDecl {
    10  .  .  .  Doc: nil
    11  .  .  .  TokPos: foo:3:1
    12  .  .  .  Tok: import
    13  .  .  .  Lparen: foo:3:8
    14  .  .  .  Specs: []ast.Spec (len = 1) {
    15  .  .  .  .  0: *ast.ImportSpec {
    16  .  .  .  .  .  Doc: nil
    17  .  .  .  .  .  Name: nil
    18  .  .  .  .  .  Path: *ast.BasicLit {
    19  .  .  .  .  .  .  ValuePos: foo:4:2
    20  .  .  .  .  .  .  Kind: STRING
    21  .  .  .  .  .  .  Value: "\"fmt\""
    22  .  .  .  .  .  }
    23  .  .  .  .  .  Comment: nil
    24  .  .  .  .  .  EndPos: -
    25  .  .  .  .  }
    26  .  .  .  }
    27  .  .  .  Rparen: foo:5:1
    28  .  .  }
    29  .  .  1: *ast.GenDecl {
    30  .  .  .  Doc: nil
    31  .  .  .  TokPos: foo:7:1
    32  .  .  .  Tok: type
    33  .  .  .  Lparen: -
    34  .  .  .  Specs: []ast.Spec (len = 1) {
    35  .  .  .  .  0: *ast.TypeSpec {
    36  .  .  .  .  .  Doc: nil
    37  .  .  .  .  .  Name: *ast.Ident {
    38  .  .  .  .  .  .  NamePos: foo:7:6
    39  .  .  .  .  .  .  Name: "B"
    40  .  .  .  .  .  .  Obj: *ast.Object {
    41  .  .  .  .  .  .  .  Kind: type
    42  .  .  .  .  .  .  .  Name: "B"
    43  .  .  .  .  .  .  .  Decl: *(obj @ 35)
    44  .  .  .  .  .  .  .  Data: nil
    45  .  .  .  .  .  .  .  Type: nil
    46  .  .  .  .  .  .  }
    47  .  .  .  .  .  }
    48  .  .  .  .  .  Assign: -
    49  .  .  .  .  .  Type: *ast.StructType {
    50  .  .  .  .  .  .  Struct: foo:7:8
    51  .  .  .  .  .  .  Fields: *ast.FieldList {
    52  .  .  .  .  .  .  .  Opening: foo:7:15
    53  .  .  .  .  .  .  .  List: []*ast.Field (len = 1) {
    54  .  .  .  .  .  .  .  .  0: *ast.Field {
    55  .  .  .  .  .  .  .  .  .  Doc: nil
    56  .  .  .  .  .  .  .  .  .  Names: []*ast.Ident (len = 1) {
    57  .  .  .  .  .  .  .  .  .  .  0: *ast.Ident {
    58  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:8:2
    59  .  .  .  .  .  .  .  .  .  .  .  Name: "K3"
    60  .  .  .  .  .  .  .  .  .  .  .  Obj: *ast.Object {
    61  .  .  .  .  .  .  .  .  .  .  .  .  Kind: var
    62  .  .  .  .  .  .  .  .  .  .  .  .  Name: "K3"
    63  .  .  .  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 54)
    64  .  .  .  .  .  .  .  .  .  .  .  .  Data: nil
    65  .  .  .  .  .  .  .  .  .  .  .  .  Type: nil
    66  .  .  .  .  .  .  .  .  .  .  .  }
    67  .  .  .  .  .  .  .  .  .  .  }
    68  .  .  .  .  .  .  .  .  .  }
    69  .  .  .  .  .  .  .  .  .  Type: *ast.Ident {
    70  .  .  .  .  .  .  .  .  .  .  NamePos: foo:8:5
    71  .  .  .  .  .  .  .  .  .  .  Name: "string"
    72  .  .  .  .  .  .  .  .  .  .  Obj: nil
    73  .  .  .  .  .  .  .  .  .  }
    74  .  .  .  .  .  .  .  .  .  Tag: nil
    75  .  .  .  .  .  .  .  .  .  Comment: nil
    76  .  .  .  .  .  .  .  .  }
    77  .  .  .  .  .  .  .  }
    78  .  .  .  .  .  .  .  Closing: foo:9:1
    79  .  .  .  .  .  .  }
    80  .  .  .  .  .  .  Incomplete: false
    81  .  .  .  .  .  }
    82  .  .  .  .  .  Comment: nil
    83  .  .  .  .  }
    84  .  .  .  }
    85  .  .  .  Rparen: -
    86  .  .  }
    87  .  .  2: *ast.GenDecl {
    88  .  .  .  Doc: nil
    89  .  .  .  TokPos: foo:11:1
    90  .  .  .  Tok: type
    91  .  .  .  Lparen: -
    92  .  .  .  Specs: []ast.Spec (len = 1) {
    93  .  .  .  .  0: *ast.TypeSpec {
    94  .  .  .  .  .  Doc: nil
    95  .  .  .  .  .  Name: *ast.Ident {
    96  .  .  .  .  .  .  NamePos: foo:11:6
    97  .  .  .  .  .  .  Name: "A"
    98  .  .  .  .  .  .  Obj: *ast.Object {
    99  .  .  .  .  .  .  .  Kind: type
   100  .  .  .  .  .  .  .  Name: "A"
   101  .  .  .  .  .  .  .  Decl: *(obj @ 93)
   102  .  .  .  .  .  .  .  Data: nil
   103  .  .  .  .  .  .  .  Type: nil
   104  .  .  .  .  .  .  }
   105  .  .  .  .  .  }
   106  .  .  .  .  .  Assign: -
   107  .  .  .  .  .  Type: *ast.StructType {
   108  .  .  .  .  .  .  Struct: foo:11:8
   109  .  .  .  .  .  .  Fields: *ast.FieldList {
   110  .  .  .  .  .  .  .  Opening: foo:11:15
   111  .  .  .  .  .  .  .  List: []*ast.Field (len = 2) {
   112  .  .  .  .  .  .  .  .  0: *ast.Field {
   113  .  .  .  .  .  .  .  .  .  Doc: nil
   114  .  .  .  .  .  .  .  .  .  Names: []*ast.Ident (len = 1) {
   115  .  .  .  .  .  .  .  .  .  .  0: *ast.Ident {
   116  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:12:2
   117  .  .  .  .  .  .  .  .  .  .  .  Name: "K1"
   118  .  .  .  .  .  .  .  .  .  .  .  Obj: *ast.Object {
   119  .  .  .  .  .  .  .  .  .  .  .  .  Kind: var
   120  .  .  .  .  .  .  .  .  .  .  .  .  Name: "K1"
   121  .  .  .  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 112)
   122  .  .  .  .  .  .  .  .  .  .  .  .  Data: nil
   123  .  .  .  .  .  .  .  .  .  .  .  .  Type: nil
   124  .  .  .  .  .  .  .  .  .  .  .  }
   125  .  .  .  .  .  .  .  .  .  .  }
   126  .  .  .  .  .  .  .  .  .  }
   127  .  .  .  .  .  .  .  .  .  Type: *ast.Ident {
   128  .  .  .  .  .  .  .  .  .  .  NamePos: foo:12:5
   129  .  .  .  .  .  .  .  .  .  .  Name: "int"
   130  .  .  .  .  .  .  .  .  .  .  Obj: nil
   131  .  .  .  .  .  .  .  .  .  }
   132  .  .  .  .  .  .  .  .  .  Tag: nil
   133  .  .  .  .  .  .  .  .  .  Comment: nil
   134  .  .  .  .  .  .  .  .  }
   135  .  .  .  .  .  .  .  .  1: *ast.Field {
   136  .  .  .  .  .  .  .  .  .  Doc: nil
   137  .  .  .  .  .  .  .  .  .  Names: []*ast.Ident (len = 1) {
   138  .  .  .  .  .  .  .  .  .  .  0: *ast.Ident {
   139  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:13:2
   140  .  .  .  .  .  .  .  .  .  .  .  Name: "K2"
   141  .  .  .  .  .  .  .  .  .  .  .  Obj: *ast.Object {
   142  .  .  .  .  .  .  .  .  .  .  .  .  Kind: var
   143  .  .  .  .  .  .  .  .  .  .  .  .  Name: "K2"
   144  .  .  .  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 135)
   145  .  .  .  .  .  .  .  .  .  .  .  .  Data: nil
   146  .  .  .  .  .  .  .  .  .  .  .  .  Type: nil
   147  .  .  .  .  .  .  .  .  .  .  .  }
   148  .  .  .  .  .  .  .  .  .  .  }
   149  .  .  .  .  .  .  .  .  .  }
   150  .  .  .  .  .  .  .  .  .  Type: *ast.Ident {
   151  .  .  .  .  .  .  .  .  .  .  NamePos: foo:13:5
   152  .  .  .  .  .  .  .  .  .  .  Name: "B"
   153  .  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 40)
   154  .  .  .  .  .  .  .  .  .  }
   155  .  .  .  .  .  .  .  .  .  Tag: nil
   156  .  .  .  .  .  .  .  .  .  Comment: nil
   157  .  .  .  .  .  .  .  .  }
   158  .  .  .  .  .  .  .  }
   159  .  .  .  .  .  .  .  Closing: foo:14:1
   160  .  .  .  .  .  .  }
   161  .  .  .  .  .  .  Incomplete: false
   162  .  .  .  .  .  }
   163  .  .  .  .  .  Comment: nil
   164  .  .  .  .  }
   165  .  .  .  }
   166  .  .  .  Rparen: -
   167  .  .  }
   168  .  .  3: *ast.FuncDecl {
   169  .  .  .  Doc: nil
   170  .  .  .  Recv: nil
   171  .  .  .  Name: *ast.Ident {
   172  .  .  .  .  NamePos: foo:16:6
   173  .  .  .  .  Name: "main"
   174  .  .  .  .  Obj: *ast.Object {
   175  .  .  .  .  .  Kind: func
   176  .  .  .  .  .  Name: "main"
   177  .  .  .  .  .  Decl: *(obj @ 168)
   178  .  .  .  .  .  Data: nil
   179  .  .  .  .  .  Type: nil
   180  .  .  .  .  }
   181  .  .  .  }
   182  .  .  .  Type: *ast.FuncType {
   183  .  .  .  .  Func: foo:16:1
   184  .  .  .  .  Params: *ast.FieldList {
   185  .  .  .  .  .  Opening: foo:16:10
   186  .  .  .  .  .  List: nil
   187  .  .  .  .  .  Closing: foo:16:11
   188  .  .  .  .  }
   189  .  .  .  .  Results: nil
   190  .  .  .  }
   191  .  .  .  Body: *ast.BlockStmt {
   192  .  .  .  .  Lbrace: foo:16:13
   193  .  .  .  .  List: []ast.Stmt (len = 2) {
   194  .  .  .  .  .  0: *ast.AssignStmt {
   195  .  .  .  .  .  .  Lhs: []ast.Expr (len = 1) {
   196  .  .  .  .  .  .  .  0: *ast.Ident {
   197  .  .  .  .  .  .  .  .  NamePos: foo:17:2
   198  .  .  .  .  .  .  .  .  Name: "s"
   199  .  .  .  .  .  .  .  .  Obj: *ast.Object {
   200  .  .  .  .  .  .  .  .  .  Kind: var
   201  .  .  .  .  .  .  .  .  .  Name: "s"
   202  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 194)
   203  .  .  .  .  .  .  .  .  .  Data: nil
   204  .  .  .  .  .  .  .  .  .  Type: nil
   205  .  .  .  .  .  .  .  .  }
   206  .  .  .  .  .  .  .  }
   207  .  .  .  .  .  .  }
   208  .  .  .  .  .  .  TokPos: foo:17:4
   209  .  .  .  .  .  .  Tok: :=
   210  .  .  .  .  .  .  Rhs: []ast.Expr (len = 1) {
   211  .  .  .  .  .  .  .  0: *ast.CompositeLit {
   212  .  .  .  .  .  .  .  .  Type: *ast.Ident {
   213  .  .  .  .  .  .  .  .  .  NamePos: foo:17:7
   214  .  .  .  .  .  .  .  .  .  Name: "A"
   215  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 98)
   216  .  .  .  .  .  .  .  .  }
   217  .  .  .  .  .  .  .  .  Lbrace: foo:17:8
   218  .  .  .  .  .  .  .  .  Elts: []ast.Expr (len = 2) {
   219  .  .  .  .  .  .  .  .  .  0: *ast.KeyValueExpr {
   220  .  .  .  .  .  .  .  .  .  .  Key: *ast.Ident {
   221  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:18:3
   222  .  .  .  .  .  .  .  .  .  .  .  Name: "K1"
   223  .  .  .  .  .  .  .  .  .  .  .  Obj: nil
   224  .  .  .  .  .  .  .  .  .  .  }
   225  .  .  .  .  .  .  .  .  .  .  Colon: foo:18:5
   226  .  .  .  .  .  .  .  .  .  .  Value: *ast.BasicLit {
   227  .  .  .  .  .  .  .  .  .  .  .  ValuePos: foo:18:7
   228  .  .  .  .  .  .  .  .  .  .  .  Kind: INT
   229  .  .  .  .  .  .  .  .  .  .  .  Value: "123"
   230  .  .  .  .  .  .  .  .  .  .  }
   231  .  .  .  .  .  .  .  .  .  }
   232  .  .  .  .  .  .  .  .  .  1: *ast.KeyValueExpr {
   233  .  .  .  .  .  .  .  .  .  .  Key: *ast.Ident {
   234  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:19:3
   235  .  .  .  .  .  .  .  .  .  .  .  Name: "K2"
   236  .  .  .  .  .  .  .  .  .  .  .  Obj: nil
   237  .  .  .  .  .  .  .  .  .  .  }
   238  .  .  .  .  .  .  .  .  .  .  Colon: foo:19:5
   239  .  .  .  .  .  .  .  .  .  .  Value: *ast.CompositeLit {
   240  .  .  .  .  .  .  .  .  .  .  .  Type: *ast.Ident {
   241  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:19:7
   242  .  .  .  .  .  .  .  .  .  .  .  .  Name: "B"
   243  .  .  .  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 40)
   244  .  .  .  .  .  .  .  .  .  .  .  }
   245  .  .  .  .  .  .  .  .  .  .  .  Lbrace: foo:19:8
   246  .  .  .  .  .  .  .  .  .  .  .  Elts: []ast.Expr (len = 1) {
   247  .  .  .  .  .  .  .  .  .  .  .  .  0: *ast.KeyValueExpr {
   248  .  .  .  .  .  .  .  .  .  .  .  .  .  Key: *ast.Ident {
   249  .  .  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:20:4
   250  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Name: "K3"
   251  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Obj: nil
   252  .  .  .  .  .  .  .  .  .  .  .  .  .  }
   253  .  .  .  .  .  .  .  .  .  .  .  .  .  Colon: foo:20:6
   254  .  .  .  .  .  .  .  .  .  .  .  .  .  Value: *ast.BasicLit {
   255  .  .  .  .  .  .  .  .  .  .  .  .  .  .  ValuePos: foo:20:8
   256  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Kind: STRING
   257  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Value: "\"Alice\""
   258  .  .  .  .  .  .  .  .  .  .  .  .  .  }
   259  .  .  .  .  .  .  .  .  .  .  .  .  }
   260  .  .  .  .  .  .  .  .  .  .  .  }
   261  .  .  .  .  .  .  .  .  .  .  .  Rbrace: foo:21:3
   262  .  .  .  .  .  .  .  .  .  .  .  Incomplete: false
   263  .  .  .  .  .  .  .  .  .  .  }
   264  .  .  .  .  .  .  .  .  .  }
   265  .  .  .  .  .  .  .  .  }
   266  .  .  .  .  .  .  .  .  Rbrace: foo:22:2
   267  .  .  .  .  .  .  .  .  Incomplete: false
   268  .  .  .  .  .  .  .  }
   269  .  .  .  .  .  .  }
   270  .  .  .  .  .  }
   271  .  .  .  .  .  1: *ast.ExprStmt {
   272  .  .  .  .  .  .  X: *ast.CallExpr {
   273  .  .  .  .  .  .  .  Fun: *ast.SelectorExpr {
   274  .  .  .  .  .  .  .  .  X: *ast.Ident {
   275  .  .  .  .  .  .  .  .  .  NamePos: foo:24:2
   276  .  .  .  .  .  .  .  .  .  Name: "fmt"
   277  .  .  .  .  .  .  .  .  .  Obj: nil
   278  .  .  .  .  .  .  .  .  }
   279  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
   280  .  .  .  .  .  .  .  .  .  NamePos: foo:24:6
   281  .  .  .  .  .  .  .  .  .  Name: "Println"
   282  .  .  .  .  .  .  .  .  .  Obj: nil
   283  .  .  .  .  .  .  .  .  }
   284  .  .  .  .  .  .  .  }
   285  .  .  .  .  .  .  .  Lparen: foo:24:13
   286  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
   287  .  .  .  .  .  .  .  .  0: *ast.Ident {
   288  .  .  .  .  .  .  .  .  .  NamePos: foo:24:14
   289  .  .  .  .  .  .  .  .  .  Name: "s"
   290  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 199)
   291  .  .  .  .  .  .  .  .  }
   292  .  .  .  .  .  .  .  }
   293  .  .  .  .  .  .  .  Ellipsis: -
   294  .  .  .  .  .  .  .  Rparen: foo:24:15
   295  .  .  .  .  .  .  }
   296  .  .  .  .  .  }
   297  .  .  .  .  }
   298  .  .  .  .  Rbrace: foo:25:1
   299  .  .  .  }
   300  .  .  }
   301  .  }
   302  .  Scope: *ast.Scope {
   303  .  .  Outer: nil
   304  .  .  Objects: map[string]*ast.Object (len = 3) {
   305  .  .  .  "B": *(obj @ 40)
   306  .  .  .  "A": *(obj @ 98)
   307  .  .  .  "main": *(obj @ 174)
   308  .  .  }
   309  .  }
   310  .  Imports: []*ast.ImportSpec (len = 1) {
   311  .  .  0: *(obj @ 15)
   312  .  }
   313  .  Unresolved: []*ast.Ident (len = 3) {
   314  .  .  0: *(obj @ 69)
   315  .  .  1: *(obj @ 127)
   316  .  .  2: *(obj @ 274)
   317  .  }
   318  .  Comments: nil
   319  }


/////


package main

import (
	"fmt"
)

func main() {
	ss := []int{1200, 1300, 144}

	fmt.Println(ss)
}



     0  *ast.File {
     1  .  Doc: nil
     2  .  Package: foo:1:1
     3  .  Name: *ast.Ident {
     4  .  .  NamePos: foo:1:9
     5  .  .  Name: "main"
     6  .  .  Obj: nil
     7  .  }
     8  .  Decls: []ast.Decl (len = 2) {
     9  .  .  0: *ast.GenDecl {
    10  .  .  .  Doc: nil
    11  .  .  .  TokPos: foo:3:1
    12  .  .  .  Tok: import
    13  .  .  .  Lparen: foo:3:8
    14  .  .  .  Specs: []ast.Spec (len = 1) {
    15  .  .  .  .  0: *ast.ImportSpec {
    16  .  .  .  .  .  Doc: nil
    17  .  .  .  .  .  Name: nil
    18  .  .  .  .  .  Path: *ast.BasicLit {
    19  .  .  .  .  .  .  ValuePos: foo:4:2
    20  .  .  .  .  .  .  Kind: STRING
    21  .  .  .  .  .  .  Value: "\"fmt\""
    22  .  .  .  .  .  }
    23  .  .  .  .  .  Comment: nil
    24  .  .  .  .  .  EndPos: -
    25  .  .  .  .  }
    26  .  .  .  }
    27  .  .  .  Rparen: foo:5:1
    28  .  .  }
    29  .  .  1: *ast.FuncDecl {
    30  .  .  .  Doc: nil
    31  .  .  .  Recv: nil
    32  .  .  .  Name: *ast.Ident {
    33  .  .  .  .  NamePos: foo:7:6
    34  .  .  .  .  Name: "main"
    35  .  .  .  .  Obj: *ast.Object {
    36  .  .  .  .  .  Kind: func
    37  .  .  .  .  .  Name: "main"
    38  .  .  .  .  .  Decl: *(obj @ 29)
    39  .  .  .  .  .  Data: nil
    40  .  .  .  .  .  Type: nil
    41  .  .  .  .  }
    42  .  .  .  }
    43  .  .  .  Type: *ast.FuncType {
    44  .  .  .  .  Func: foo:7:1
    45  .  .  .  .  Params: *ast.FieldList {
    46  .  .  .  .  .  Opening: foo:7:10
    47  .  .  .  .  .  List: nil
    48  .  .  .  .  .  Closing: foo:7:11
    49  .  .  .  .  }
    50  .  .  .  .  Results: nil
    51  .  .  .  }
    52  .  .  .  Body: *ast.BlockStmt {
    53  .  .  .  .  Lbrace: foo:7:13
    54  .  .  .  .  List: []ast.Stmt (len = 2) {
    55  .  .  .  .  .  0: *ast.AssignStmt {
    56  .  .  .  .  .  .  Lhs: []ast.Expr (len = 1) {
    57  .  .  .  .  .  .  .  0: *ast.Ident {
    58  .  .  .  .  .  .  .  .  NamePos: foo:8:2
    59  .  .  .  .  .  .  .  .  Name: "ss"
    60  .  .  .  .  .  .  .  .  Obj: *ast.Object {
    61  .  .  .  .  .  .  .  .  .  Kind: var
    62  .  .  .  .  .  .  .  .  .  Name: "ss"
    63  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 55)
    64  .  .  .  .  .  .  .  .  .  Data: nil
    65  .  .  .  .  .  .  .  .  .  Type: nil
    66  .  .  .  .  .  .  .  .  }
    67  .  .  .  .  .  .  .  }
    68  .  .  .  .  .  .  }
    69  .  .  .  .  .  .  TokPos: foo:8:5
    70  .  .  .  .  .  .  Tok: :=
    71  .  .  .  .  .  .  Rhs: []ast.Expr (len = 1) {
    72  .  .  .  .  .  .  .  0: *ast.CompositeLit {
    73  .  .  .  .  .  .  .  .  Type: *ast.ArrayType {
    74  .  .  .  .  .  .  .  .  .  Lbrack: foo:8:8
    75  .  .  .  .  .  .  .  .  .  Len: nil
    76  .  .  .  .  .  .  .  .  .  Elt: *ast.Ident {
    77  .  .  .  .  .  .  .  .  .  .  NamePos: foo:8:10
    78  .  .  .  .  .  .  .  .  .  .  Name: "int"
    79  .  .  .  .  .  .  .  .  .  .  Obj: nil
    80  .  .  .  .  .  .  .  .  .  }
    81  .  .  .  .  .  .  .  .  }
    82  .  .  .  .  .  .  .  .  Lbrace: foo:8:13
    83  .  .  .  .  .  .  .  .  Elts: []ast.Expr (len = 3) {
    84  .  .  .  .  .  .  .  .  .  0: *ast.BasicLit {
    85  .  .  .  .  .  .  .  .  .  .  ValuePos: foo:8:14
    86  .  .  .  .  .  .  .  .  .  .  Kind: INT
    87  .  .  .  .  .  .  .  .  .  .  Value: "1200"
    88  .  .  .  .  .  .  .  .  .  }
    89  .  .  .  .  .  .  .  .  .  1: *ast.BasicLit {
    90  .  .  .  .  .  .  .  .  .  .  ValuePos: foo:8:20
    91  .  .  .  .  .  .  .  .  .  .  Kind: INT
    92  .  .  .  .  .  .  .  .  .  .  Value: "1300"
    93  .  .  .  .  .  .  .  .  .  }
    94  .  .  .  .  .  .  .  .  .  2: *ast.BasicLit {
    95  .  .  .  .  .  .  .  .  .  .  ValuePos: foo:8:26
    96  .  .  .  .  .  .  .  .  .  .  Kind: INT
    97  .  .  .  .  .  .  .  .  .  .  Value: "144"
    98  .  .  .  .  .  .  .  .  .  }
    99  .  .  .  .  .  .  .  .  }
   100  .  .  .  .  .  .  .  .  Rbrace: foo:8:29
   101  .  .  .  .  .  .  .  .  Incomplete: false
   102  .  .  .  .  .  .  .  }
   103  .  .  .  .  .  .  }
   104  .  .  .  .  .  }
   105  .  .  .  .  .  1: *ast.ExprStmt {
   106  .  .  .  .  .  .  X: *ast.CallExpr {
   107  .  .  .  .  .  .  .  Fun: *ast.SelectorExpr {
   108  .  .  .  .  .  .  .  .  X: *ast.Ident {
   109  .  .  .  .  .  .  .  .  .  NamePos: foo:10:2
   110  .  .  .  .  .  .  .  .  .  Name: "fmt"
   111  .  .  .  .  .  .  .  .  .  Obj: nil
   112  .  .  .  .  .  .  .  .  }
   113  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
   114  .  .  .  .  .  .  .  .  .  NamePos: foo:10:6
   115  .  .  .  .  .  .  .  .  .  Name: "Println"
   116  .  .  .  .  .  .  .  .  .  Obj: nil
   117  .  .  .  .  .  .  .  .  }
   118  .  .  .  .  .  .  .  }
   119  .  .  .  .  .  .  .  Lparen: foo:10:13
   120  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
   121  .  .  .  .  .  .  .  .  0: *ast.Ident {
   122  .  .  .  .  .  .  .  .  .  NamePos: foo:10:14
   123  .  .  .  .  .  .  .  .  .  Name: "ss"
   124  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 60)
   125  .  .  .  .  .  .  .  .  }
   126  .  .  .  .  .  .  .  }
   127  .  .  .  .  .  .  .  Ellipsis: -
   128  .  .  .  .  .  .  .  Rparen: foo:10:16
   129  .  .  .  .  .  .  }
   130  .  .  .  .  .  }
   131  .  .  .  .  }
   132  .  .  .  .  Rbrace: foo:11:1
   133  .  .  .  }
   134  .  .  }
   135  .  }
   136  .  Scope: *ast.Scope {
   137  .  .  Outer: nil
   138  .  .  Objects: map[string]*ast.Object (len = 1) {
   139  .  .  .  "main": *(obj @ 35)
   140  .  .  }
   141  .  }
   142  .  Imports: []*ast.ImportSpec (len = 1) {
   143  .  .  0: *(obj @ 15)
   144  .  }
   145  .  Unresolved: []*ast.Ident (len = 2) {
   146  .  .  0: *(obj @ 76)
   147  .  .  1: *(obj @ 108)
   148  .  }
   149  .  Comments: nil
   150  }


///////////////////////////////////////////////////////////

package main

import (
	"fmt"
)

func main() {
	m := map[string]int{
		"Alice": 123,
		"Bob":   456,
	}

	fmt.Println(m)
}



     0  *ast.File {
     1  .  Doc: nil
     2  .  Package: foo:1:1
     3  .  Name: *ast.Ident {
     4  .  .  NamePos: foo:1:9
     5  .  .  Name: "main"
     6  .  .  Obj: nil
     7  .  }
     8  .  Decls: []ast.Decl (len = 2) {
     9  .  .  0: *ast.GenDecl {
    10  .  .  .  Doc: nil
    11  .  .  .  TokPos: foo:3:1
    12  .  .  .  Tok: import
    13  .  .  .  Lparen: foo:3:8
    14  .  .  .  Specs: []ast.Spec (len = 1) {
    15  .  .  .  .  0: *ast.ImportSpec {
    16  .  .  .  .  .  Doc: nil
    17  .  .  .  .  .  Name: nil
    18  .  .  .  .  .  Path: *ast.BasicLit {
    19  .  .  .  .  .  .  ValuePos: foo:4:2
    20  .  .  .  .  .  .  Kind: STRING
    21  .  .  .  .  .  .  Value: "\"fmt\""
    22  .  .  .  .  .  }
    23  .  .  .  .  .  Comment: nil
    24  .  .  .  .  .  EndPos: -
    25  .  .  .  .  }
    26  .  .  .  }
    27  .  .  .  Rparen: foo:5:1
    28  .  .  }
    29  .  .  1: *ast.FuncDecl {
    30  .  .  .  Doc: nil
    31  .  .  .  Recv: nil
    32  .  .  .  Name: *ast.Ident {
    33  .  .  .  .  NamePos: foo:7:6
    34  .  .  .  .  Name: "main"
    35  .  .  .  .  Obj: *ast.Object {
    36  .  .  .  .  .  Kind: func
    37  .  .  .  .  .  Name: "main"
    38  .  .  .  .  .  Decl: *(obj @ 29)
    39  .  .  .  .  .  Data: nil
    40  .  .  .  .  .  Type: nil
    41  .  .  .  .  }
    42  .  .  .  }
    43  .  .  .  Type: *ast.FuncType {
    44  .  .  .  .  Func: foo:7:1
    45  .  .  .  .  Params: *ast.FieldList {
    46  .  .  .  .  .  Opening: foo:7:10
    47  .  .  .  .  .  List: nil
    48  .  .  .  .  .  Closing: foo:7:11
    49  .  .  .  .  }
    50  .  .  .  .  Results: nil
    51  .  .  .  }
    52  .  .  .  Body: *ast.BlockStmt {
    53  .  .  .  .  Lbrace: foo:7:13
    54  .  .  .  .  List: []ast.Stmt (len = 2) {
    55  .  .  .  .  .  0: *ast.AssignStmt {
    56  .  .  .  .  .  .  Lhs: []ast.Expr (len = 1) {
    57  .  .  .  .  .  .  .  0: *ast.Ident {
    58  .  .  .  .  .  .  .  .  NamePos: foo:8:2
    59  .  .  .  .  .  .  .  .  Name: "m"
    60  .  .  .  .  .  .  .  .  Obj: *ast.Object {
    61  .  .  .  .  .  .  .  .  .  Kind: var
    62  .  .  .  .  .  .  .  .  .  Name: "m"
    63  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 55)
    64  .  .  .  .  .  .  .  .  .  Data: nil
    65  .  .  .  .  .  .  .  .  .  Type: nil
    66  .  .  .  .  .  .  .  .  }
    67  .  .  .  .  .  .  .  }
    68  .  .  .  .  .  .  }
    69  .  .  .  .  .  .  TokPos: foo:8:4
    70  .  .  .  .  .  .  Tok: :=
    71  .  .  .  .  .  .  Rhs: []ast.Expr (len = 1) {
    72  .  .  .  .  .  .  .  0: *ast.CompositeLit {
    73  .  .  .  .  .  .  .  .  Type: *ast.MapType {
    74  .  .  .  .  .  .  .  .  .  Map: foo:8:7
    75  .  .  .  .  .  .  .  .  .  Key: *ast.Ident {
    76  .  .  .  .  .  .  .  .  .  .  NamePos: foo:8:11
    77  .  .  .  .  .  .  .  .  .  .  Name: "string"
    78  .  .  .  .  .  .  .  .  .  .  Obj: nil
    79  .  .  .  .  .  .  .  .  .  }
    80  .  .  .  .  .  .  .  .  .  Value: *ast.Ident {
    81  .  .  .  .  .  .  .  .  .  .  NamePos: foo:8:18
    82  .  .  .  .  .  .  .  .  .  .  Name: "int"
    83  .  .  .  .  .  .  .  .  .  .  Obj: nil
    84  .  .  .  .  .  .  .  .  .  }
    85  .  .  .  .  .  .  .  .  }
    86  .  .  .  .  .  .  .  .  Lbrace: foo:8:21
    87  .  .  .  .  .  .  .  .  Elts: []ast.Expr (len = 2) {
    88  .  .  .  .  .  .  .  .  .  0: *ast.KeyValueExpr {
    89  .  .  .  .  .  .  .  .  .  .  Key: *ast.BasicLit {
    90  .  .  .  .  .  .  .  .  .  .  .  ValuePos: foo:9:3
    91  .  .  .  .  .  .  .  .  .  .  .  Kind: STRING
    92  .  .  .  .  .  .  .  .  .  .  .  Value: "\"Alice\""
    93  .  .  .  .  .  .  .  .  .  .  }
    94  .  .  .  .  .  .  .  .  .  .  Colon: foo:9:10
    95  .  .  .  .  .  .  .  .  .  .  Value: *ast.BasicLit {
    96  .  .  .  .  .  .  .  .  .  .  .  ValuePos: foo:9:12
    97  .  .  .  .  .  .  .  .  .  .  .  Kind: INT
    98  .  .  .  .  .  .  .  .  .  .  .  Value: "123"
    99  .  .  .  .  .  .  .  .  .  .  }
   100  .  .  .  .  .  .  .  .  .  }
   101  .  .  .  .  .  .  .  .  .  1: *ast.KeyValueExpr {
   102  .  .  .  .  .  .  .  .  .  .  Key: *ast.BasicLit {
   103  .  .  .  .  .  .  .  .  .  .  .  ValuePos: foo:10:3
   104  .  .  .  .  .  .  .  .  .  .  .  Kind: STRING
   105  .  .  .  .  .  .  .  .  .  .  .  Value: "\"Bob\""
   106  .  .  .  .  .  .  .  .  .  .  }
   107  .  .  .  .  .  .  .  .  .  .  Colon: foo:10:8
   108  .  .  .  .  .  .  .  .  .  .  Value: *ast.BasicLit {
   109  .  .  .  .  .  .  .  .  .  .  .  ValuePos: foo:10:12
   110  .  .  .  .  .  .  .  .  .  .  .  Kind: INT
   111  .  .  .  .  .  .  .  .  .  .  .  Value: "456"
   112  .  .  .  .  .  .  .  .  .  .  }
   113  .  .  .  .  .  .  .  .  .  }
   114  .  .  .  .  .  .  .  .  }
   115  .  .  .  .  .  .  .  .  Rbrace: foo:11:2
   116  .  .  .  .  .  .  .  .  Incomplete: false
   117  .  .  .  .  .  .  .  }
   118  .  .  .  .  .  .  }
   119  .  .  .  .  .  }
   120  .  .  .  .  .  1: *ast.ExprStmt {
   121  .  .  .  .  .  .  X: *ast.CallExpr {
   122  .  .  .  .  .  .  .  Fun: *ast.SelectorExpr {
   123  .  .  .  .  .  .  .  .  X: *ast.Ident {
   124  .  .  .  .  .  .  .  .  .  NamePos: foo:13:2
   125  .  .  .  .  .  .  .  .  .  Name: "fmt"
   126  .  .  .  .  .  .  .  .  .  Obj: nil
   127  .  .  .  .  .  .  .  .  }
   128  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
   129  .  .  .  .  .  .  .  .  .  NamePos: foo:13:6
   130  .  .  .  .  .  .  .  .  .  Name: "Println"
   131  .  .  .  .  .  .  .  .  .  Obj: nil
   132  .  .  .  .  .  .  .  .  }
   133  .  .  .  .  .  .  .  }
   134  .  .  .  .  .  .  .  Lparen: foo:13:13
   135  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
   136  .  .  .  .  .  .  .  .  0: *ast.Ident {
   137  .  .  .  .  .  .  .  .  .  NamePos: foo:13:14
   138  .  .  .  .  .  .  .  .  .  Name: "m"
   139  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 60)
   140  .  .  .  .  .  .  .  .  }
   141  .  .  .  .  .  .  .  }
   142  .  .  .  .  .  .  .  Ellipsis: -
   143  .  .  .  .  .  .  .  Rparen: foo:13:15
   144  .  .  .  .  .  .  }
   145  .  .  .  .  .  }
   146  .  .  .  .  }
   147  .  .  .  .  Rbrace: foo:14:1
   148  .  .  .  }
   149  .  .  }
   150  .  }
   151  .  Scope: *ast.Scope {
   152  .  .  Outer: nil
   153  .  .  Objects: map[string]*ast.Object (len = 1) {
   154  .  .  .  "main": *(obj @ 35)
   155  .  .  }
   156  .  }
   157  .  Imports: []*ast.ImportSpec (len = 1) {
   158  .  .  0: *(obj @ 15)
   159  .  }
   160  .  Unresolved: []*ast.Ident (len = 3) {
   161  .  .  0: *(obj @ 75)
   162  .  .  1: *(obj @ 80)
   163  .  .  2: *(obj @ 123)
   164  .  }
   165  .  Comments: nil
   166  }


######################################################################

package main

import "fmt"

func main() {

	f := func() {
		fmt.Println("Hello")
	}

	f()
}

     0  *ast.File {
     1  .  Doc: nil
     2  .  Package: foo:1:1
     3  .  Name: *ast.Ident {
     4  .  .  NamePos: foo:1:9
     5  .  .  Name: "main"
     6  .  .  Obj: nil
     7  .  }
     8  .  Decls: []ast.Decl (len = 2) {
     9  .  .  0: *ast.GenDecl {
    10  .  .  .  Doc: nil
    11  .  .  .  TokPos: foo:3:1
    12  .  .  .  Tok: import
    13  .  .  .  Lparen: -
    14  .  .  .  Specs: []ast.Spec (len = 1) {
    15  .  .  .  .  0: *ast.ImportSpec {
    16  .  .  .  .  .  Doc: nil
    17  .  .  .  .  .  Name: nil
    18  .  .  .  .  .  Path: *ast.BasicLit {
    19  .  .  .  .  .  .  ValuePos: foo:3:8
    20  .  .  .  .  .  .  Kind: STRING
    21  .  .  .  .  .  .  Value: "\"fmt\""
    22  .  .  .  .  .  }
    23  .  .  .  .  .  Comment: nil
    24  .  .  .  .  .  EndPos: -
    25  .  .  .  .  }
    26  .  .  .  }
    27  .  .  .  Rparen: -
    28  .  .  }
    29  .  .  1: *ast.FuncDecl {
    30  .  .  .  Doc: nil
    31  .  .  .  Recv: nil
    32  .  .  .  Name: *ast.Ident {
    33  .  .  .  .  NamePos: foo:5:6
    34  .  .  .  .  Name: "main"
    35  .  .  .  .  Obj: *ast.Object {
    36  .  .  .  .  .  Kind: func
    37  .  .  .  .  .  Name: "main"
    38  .  .  .  .  .  Decl: *(obj @ 29)
    39  .  .  .  .  .  Data: nil
    40  .  .  .  .  .  Type: nil
    41  .  .  .  .  }
    42  .  .  .  }
    43  .  .  .  Type: *ast.FuncType {
    44  .  .  .  .  Func: foo:5:1
    45  .  .  .  .  Params: *ast.FieldList {
    46  .  .  .  .  .  Opening: foo:5:10
    47  .  .  .  .  .  List: nil
    48  .  .  .  .  .  Closing: foo:5:11
    49  .  .  .  .  }
    50  .  .  .  .  Results: nil
    51  .  .  .  }
    52  .  .  .  Body: *ast.BlockStmt {
    53  .  .  .  .  Lbrace: foo:5:13
    54  .  .  .  .  List: []ast.Stmt (len = 2) {
    55  .  .  .  .  .  0: *ast.AssignStmt {
    56  .  .  .  .  .  .  Lhs: []ast.Expr (len = 1) {
    57  .  .  .  .  .  .  .  0: *ast.Ident {
    58  .  .  .  .  .  .  .  .  NamePos: foo:7:2
    59  .  .  .  .  .  .  .  .  Name: "f"
    60  .  .  .  .  .  .  .  .  Obj: *ast.Object {
    61  .  .  .  .  .  .  .  .  .  Kind: var
    62  .  .  .  .  .  .  .  .  .  Name: "f"
    63  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 55)
    64  .  .  .  .  .  .  .  .  .  Data: nil
    65  .  .  .  .  .  .  .  .  .  Type: nil
    66  .  .  .  .  .  .  .  .  }
    67  .  .  .  .  .  .  .  }
    68  .  .  .  .  .  .  }
    69  .  .  .  .  .  .  TokPos: foo:7:4
    70  .  .  .  .  .  .  Tok: :=
    71  .  .  .  .  .  .  Rhs: []ast.Expr (len = 1) {
    72  .  .  .  .  .  .  .  0: *ast.FuncLit {
    73  .  .  .  .  .  .  .  .  Type: *ast.FuncType {
    74  .  .  .  .  .  .  .  .  .  Func: foo:7:7
    75  .  .  .  .  .  .  .  .  .  Params: *ast.FieldList {
    76  .  .  .  .  .  .  .  .  .  .  Opening: foo:7:11
    77  .  .  .  .  .  .  .  .  .  .  List: nil
    78  .  .  .  .  .  .  .  .  .  .  Closing: foo:7:12
    79  .  .  .  .  .  .  .  .  .  }
    80  .  .  .  .  .  .  .  .  .  Results: nil
    81  .  .  .  .  .  .  .  .  }
    82  .  .  .  .  .  .  .  .  Body: *ast.BlockStmt {
    83  .  .  .  .  .  .  .  .  .  Lbrace: foo:7:14
    84  .  .  .  .  .  .  .  .  .  List: []ast.Stmt (len = 1) {
    85  .  .  .  .  .  .  .  .  .  .  0: *ast.ExprStmt {
    86  .  .  .  .  .  .  .  .  .  .  .  X: *ast.CallExpr {
    87  .  .  .  .  .  .  .  .  .  .  .  .  Fun: *ast.SelectorExpr {
    88  .  .  .  .  .  .  .  .  .  .  .  .  .  X: *ast.Ident {
    89  .  .  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:8:3
    90  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Name: "fmt"
    91  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Obj: nil
    92  .  .  .  .  .  .  .  .  .  .  .  .  .  }
    93  .  .  .  .  .  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
    94  .  .  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:8:7
    95  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Name: "Println"
    96  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Obj: nil
    97  .  .  .  .  .  .  .  .  .  .  .  .  .  }
    98  .  .  .  .  .  .  .  .  .  .  .  .  }
    99  .  .  .  .  .  .  .  .  .  .  .  .  Lparen: foo:8:14
   100  .  .  .  .  .  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
   101  .  .  .  .  .  .  .  .  .  .  .  .  .  0: *ast.BasicLit {
   102  .  .  .  .  .  .  .  .  .  .  .  .  .  .  ValuePos: foo:8:15
   103  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Kind: STRING
   104  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Value: "\"Hello\""
   105  .  .  .  .  .  .  .  .  .  .  .  .  .  }
   106  .  .  .  .  .  .  .  .  .  .  .  .  }
   107  .  .  .  .  .  .  .  .  .  .  .  .  Ellipsis: -
   108  .  .  .  .  .  .  .  .  .  .  .  .  Rparen: foo:8:22
   109  .  .  .  .  .  .  .  .  .  .  .  }
   110  .  .  .  .  .  .  .  .  .  .  }
   111  .  .  .  .  .  .  .  .  .  }
   112  .  .  .  .  .  .  .  .  .  Rbrace: foo:9:2
   113  .  .  .  .  .  .  .  .  }
   114  .  .  .  .  .  .  .  }
   115  .  .  .  .  .  .  }
   116  .  .  .  .  .  }
   117  .  .  .  .  .  1: *ast.ExprStmt {
   118  .  .  .  .  .  .  X: *ast.CallExpr {
   119  .  .  .  .  .  .  .  Fun: *ast.Ident {
   120  .  .  .  .  .  .  .  .  NamePos: foo:11:2
   121  .  .  .  .  .  .  .  .  Name: "f"
   122  .  .  .  .  .  .  .  .  Obj: *(obj @ 60)
   123  .  .  .  .  .  .  .  }
   124  .  .  .  .  .  .  .  Lparen: foo:11:3
   125  .  .  .  .  .  .  .  Args: nil
   126  .  .  .  .  .  .  .  Ellipsis: -
   127  .  .  .  .  .  .  .  Rparen: foo:11:4
   128  .  .  .  .  .  .  }
   129  .  .  .  .  .  }
   130  .  .  .  .  }
   131  .  .  .  .  Rbrace: foo:12:1
   132  .  .  .  }
   133  .  .  }
   134  .  }
   135  .  Scope: *ast.Scope {
   136  .  .  Outer: nil
   137  .  .  Objects: map[string]*ast.Object (len = 1) {
   138  .  .  .  "main": *(obj @ 35)
   139  .  .  }
   140  .  }
   141  .  Imports: []*ast.ImportSpec (len = 1) {
   142  .  .  0: *(obj @ 15)
   143  .  }
   144  .  Unresolved: []*ast.Ident (len = 1) {
   145  .  .  0: *(obj @ 88)
   146  .  }
   147  .  Comments: nil
   148  }


###############################################################################

package main

import "fmt"

func sayHello(fn func() string) {
	// call the function
	// and print the result
	fmt.Println(fn())
}

func main() {

	f := func() string {
		return "Hello"
	}

	sayHello(f)
}


     0  *ast.File {
     1  .  Doc: nil
     2  .  Package: foo:1:1
     3  .  Name: *ast.Ident {
     4  .  .  NamePos: foo:1:9
     5  .  .  Name: "main"
     6  .  .  Obj: nil
     7  .  }
     8  .  Decls: []ast.Decl (len = 3) {
     9  .  .  0: *ast.GenDecl {
    10  .  .  .  Doc: nil
    11  .  .  .  TokPos: foo:3:1
    12  .  .  .  Tok: import
    13  .  .  .  Lparen: -
    14  .  .  .  Specs: []ast.Spec (len = 1) {
    15  .  .  .  .  0: *ast.ImportSpec {
    16  .  .  .  .  .  Doc: nil
    17  .  .  .  .  .  Name: nil
    18  .  .  .  .  .  Path: *ast.BasicLit {
    19  .  .  .  .  .  .  ValuePos: foo:3:8
    20  .  .  .  .  .  .  Kind: STRING
    21  .  .  .  .  .  .  Value: "\"fmt\""
    22  .  .  .  .  .  }
    23  .  .  .  .  .  Comment: nil
    24  .  .  .  .  .  EndPos: -
    25  .  .  .  .  }
    26  .  .  .  }
    27  .  .  .  Rparen: -
    28  .  .  }
    29  .  .  1: *ast.FuncDecl {
    30  .  .  .  Doc: nil
    31  .  .  .  Recv: nil
    32  .  .  .  Name: *ast.Ident {
    33  .  .  .  .  NamePos: foo:5:6
    34  .  .  .  .  Name: "sayHello"
    35  .  .  .  .  Obj: *ast.Object {
    36  .  .  .  .  .  Kind: func
    37  .  .  .  .  .  Name: "sayHello"
    38  .  .  .  .  .  Decl: *(obj @ 29)
    39  .  .  .  .  .  Data: nil
    40  .  .  .  .  .  Type: nil
    41  .  .  .  .  }
    42  .  .  .  }
    43  .  .  .  Type: *ast.FuncType {
    44  .  .  .  .  Func: foo:5:1
    45  .  .  .  .  Params: *ast.FieldList {
    46  .  .  .  .  .  Opening: foo:5:14
    47  .  .  .  .  .  List: []*ast.Field (len = 1) {
    48  .  .  .  .  .  .  0: *ast.Field {
    49  .  .  .  .  .  .  .  Doc: nil
    50  .  .  .  .  .  .  .  Names: []*ast.Ident (len = 1) {
    51  .  .  .  .  .  .  .  .  0: *ast.Ident {
    52  .  .  .  .  .  .  .  .  .  NamePos: foo:5:15
    53  .  .  .  .  .  .  .  .  .  Name: "fn"
    54  .  .  .  .  .  .  .  .  .  Obj: *ast.Object {
    55  .  .  .  .  .  .  .  .  .  .  Kind: var
    56  .  .  .  .  .  .  .  .  .  .  Name: "fn"
    57  .  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 48)
    58  .  .  .  .  .  .  .  .  .  .  Data: nil
    59  .  .  .  .  .  .  .  .  .  .  Type: nil
    60  .  .  .  .  .  .  .  .  .  }
    61  .  .  .  .  .  .  .  .  }
    62  .  .  .  .  .  .  .  }
    63  .  .  .  .  .  .  .  Type: *ast.FuncType {
    64  .  .  .  .  .  .  .  .  Func: foo:5:18
    65  .  .  .  .  .  .  .  .  Params: *ast.FieldList {
    66  .  .  .  .  .  .  .  .  .  Opening: foo:5:22
    67  .  .  .  .  .  .  .  .  .  List: nil
    68  .  .  .  .  .  .  .  .  .  Closing: foo:5:23
    69  .  .  .  .  .  .  .  .  }
    70  .  .  .  .  .  .  .  .  Results: *ast.FieldList {
    71  .  .  .  .  .  .  .  .  .  Opening: -
    72  .  .  .  .  .  .  .  .  .  List: []*ast.Field (len = 1) {
    73  .  .  .  .  .  .  .  .  .  .  0: *ast.Field {
    74  .  .  .  .  .  .  .  .  .  .  .  Doc: nil
    75  .  .  .  .  .  .  .  .  .  .  .  Names: nil
    76  .  .  .  .  .  .  .  .  .  .  .  Type: *ast.Ident {
    77  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:5:25
    78  .  .  .  .  .  .  .  .  .  .  .  .  Name: "string"
    79  .  .  .  .  .  .  .  .  .  .  .  .  Obj: nil
    80  .  .  .  .  .  .  .  .  .  .  .  }
    81  .  .  .  .  .  .  .  .  .  .  .  Tag: nil
    82  .  .  .  .  .  .  .  .  .  .  .  Comment: nil
    83  .  .  .  .  .  .  .  .  .  .  }
    84  .  .  .  .  .  .  .  .  .  }
    85  .  .  .  .  .  .  .  .  .  Closing: -
    86  .  .  .  .  .  .  .  .  }
    87  .  .  .  .  .  .  .  }
    88  .  .  .  .  .  .  .  Tag: nil
    89  .  .  .  .  .  .  .  Comment: nil
    90  .  .  .  .  .  .  }
    91  .  .  .  .  .  }
    92  .  .  .  .  .  Closing: foo:5:31
    93  .  .  .  .  }
    94  .  .  .  .  Results: nil
    95  .  .  .  }
    96  .  .  .  Body: *ast.BlockStmt {
    97  .  .  .  .  Lbrace: foo:5:33
    98  .  .  .  .  List: []ast.Stmt (len = 1) {
    99  .  .  .  .  .  0: *ast.ExprStmt {
   100  .  .  .  .  .  .  X: *ast.CallExpr {
   101  .  .  .  .  .  .  .  Fun: *ast.SelectorExpr {
   102  .  .  .  .  .  .  .  .  X: *ast.Ident {
   103  .  .  .  .  .  .  .  .  .  NamePos: foo:8:2
   104  .  .  .  .  .  .  .  .  .  Name: "fmt"
   105  .  .  .  .  .  .  .  .  .  Obj: nil
   106  .  .  .  .  .  .  .  .  }
   107  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
   108  .  .  .  .  .  .  .  .  .  NamePos: foo:8:6
   109  .  .  .  .  .  .  .  .  .  Name: "Println"
   110  .  .  .  .  .  .  .  .  .  Obj: nil
   111  .  .  .  .  .  .  .  .  }
   112  .  .  .  .  .  .  .  }
   113  .  .  .  .  .  .  .  Lparen: foo:8:13
   114  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
   115  .  .  .  .  .  .  .  .  0: *ast.CallExpr {
   116  .  .  .  .  .  .  .  .  .  Fun: *ast.Ident {
   117  .  .  .  .  .  .  .  .  .  .  NamePos: foo:8:14
   118  .  .  .  .  .  .  .  .  .  .  Name: "fn"
   119  .  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 54)
   120  .  .  .  .  .  .  .  .  .  }
   121  .  .  .  .  .  .  .  .  .  Lparen: foo:8:16
   122  .  .  .  .  .  .  .  .  .  Args: nil
   123  .  .  .  .  .  .  .  .  .  Ellipsis: -
   124  .  .  .  .  .  .  .  .  .  Rparen: foo:8:17
   125  .  .  .  .  .  .  .  .  }
   126  .  .  .  .  .  .  .  }
   127  .  .  .  .  .  .  .  Ellipsis: -
   128  .  .  .  .  .  .  .  Rparen: foo:8:18
   129  .  .  .  .  .  .  }
   130  .  .  .  .  .  }
   131  .  .  .  .  }
   132  .  .  .  .  Rbrace: foo:9:1
   133  .  .  .  }
   134  .  .  }
   135  .  .  2: *ast.FuncDecl {
   136  .  .  .  Doc: nil
   137  .  .  .  Recv: nil
   138  .  .  .  Name: *ast.Ident {
   139  .  .  .  .  NamePos: foo:11:6
   140  .  .  .  .  Name: "main"
   141  .  .  .  .  Obj: *ast.Object {
   142  .  .  .  .  .  Kind: func
   143  .  .  .  .  .  Name: "main"
   144  .  .  .  .  .  Decl: *(obj @ 135)
   145  .  .  .  .  .  Data: nil
   146  .  .  .  .  .  Type: nil
   147  .  .  .  .  }
   148  .  .  .  }
   149  .  .  .  Type: *ast.FuncType {
   150  .  .  .  .  Func: foo:11:1
   151  .  .  .  .  Params: *ast.FieldList {
   152  .  .  .  .  .  Opening: foo:11:10
   153  .  .  .  .  .  List: nil
   154  .  .  .  .  .  Closing: foo:11:11
   155  .  .  .  .  }
   156  .  .  .  .  Results: nil
   157  .  .  .  }
   158  .  .  .  Body: *ast.BlockStmt {
   159  .  .  .  .  Lbrace: foo:11:13
   160  .  .  .  .  List: []ast.Stmt (len = 2) {
   161  .  .  .  .  .  0: *ast.AssignStmt {
   162  .  .  .  .  .  .  Lhs: []ast.Expr (len = 1) {
   163  .  .  .  .  .  .  .  0: *ast.Ident {
   164  .  .  .  .  .  .  .  .  NamePos: foo:13:2
   165  .  .  .  .  .  .  .  .  Name: "f"
   166  .  .  .  .  .  .  .  .  Obj: *ast.Object {
   167  .  .  .  .  .  .  .  .  .  Kind: var
   168  .  .  .  .  .  .  .  .  .  Name: "f"
   169  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 161)
   170  .  .  .  .  .  .  .  .  .  Data: nil
   171  .  .  .  .  .  .  .  .  .  Type: nil
   172  .  .  .  .  .  .  .  .  }
   173  .  .  .  .  .  .  .  }
   174  .  .  .  .  .  .  }
   175  .  .  .  .  .  .  TokPos: foo:13:4
   176  .  .  .  .  .  .  Tok: :=
   177  .  .  .  .  .  .  Rhs: []ast.Expr (len = 1) {
   178  .  .  .  .  .  .  .  0: *ast.FuncLit {
   179  .  .  .  .  .  .  .  .  Type: *ast.FuncType {
   180  .  .  .  .  .  .  .  .  .  Func: foo:13:7
   181  .  .  .  .  .  .  .  .  .  Params: *ast.FieldList {
   182  .  .  .  .  .  .  .  .  .  .  Opening: foo:13:11
   183  .  .  .  .  .  .  .  .  .  .  List: nil
   184  .  .  .  .  .  .  .  .  .  .  Closing: foo:13:12
   185  .  .  .  .  .  .  .  .  .  }
   186  .  .  .  .  .  .  .  .  .  Results: *ast.FieldList {
   187  .  .  .  .  .  .  .  .  .  .  Opening: -
   188  .  .  .  .  .  .  .  .  .  .  List: []*ast.Field (len = 1) {
   189  .  .  .  .  .  .  .  .  .  .  .  0: *ast.Field {
   190  .  .  .  .  .  .  .  .  .  .  .  .  Doc: nil
   191  .  .  .  .  .  .  .  .  .  .  .  .  Names: nil
   192  .  .  .  .  .  .  .  .  .  .  .  .  Type: *ast.Ident {
   193  .  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:13:14
   194  .  .  .  .  .  .  .  .  .  .  .  .  .  Name: "string"
   195  .  .  .  .  .  .  .  .  .  .  .  .  .  Obj: nil
   196  .  .  .  .  .  .  .  .  .  .  .  .  }
   197  .  .  .  .  .  .  .  .  .  .  .  .  Tag: nil
   198  .  .  .  .  .  .  .  .  .  .  .  .  Comment: nil
   199  .  .  .  .  .  .  .  .  .  .  .  }
   200  .  .  .  .  .  .  .  .  .  .  }
   201  .  .  .  .  .  .  .  .  .  .  Closing: -
   202  .  .  .  .  .  .  .  .  .  }
   203  .  .  .  .  .  .  .  .  }
   204  .  .  .  .  .  .  .  .  Body: *ast.BlockStmt {
   205  .  .  .  .  .  .  .  .  .  Lbrace: foo:13:21
   206  .  .  .  .  .  .  .  .  .  List: []ast.Stmt (len = 1) {
   207  .  .  .  .  .  .  .  .  .  .  0: *ast.ReturnStmt {
   208  .  .  .  .  .  .  .  .  .  .  .  Return: foo:14:3
   209  .  .  .  .  .  .  .  .  .  .  .  Results: []ast.Expr (len = 1) {
   210  .  .  .  .  .  .  .  .  .  .  .  .  0: *ast.BasicLit {
   211  .  .  .  .  .  .  .  .  .  .  .  .  .  ValuePos: foo:14:10
   212  .  .  .  .  .  .  .  .  .  .  .  .  .  Kind: STRING
   213  .  .  .  .  .  .  .  .  .  .  .  .  .  Value: "\"Hello\""
   214  .  .  .  .  .  .  .  .  .  .  .  .  }
   215  .  .  .  .  .  .  .  .  .  .  .  }
   216  .  .  .  .  .  .  .  .  .  .  }
   217  .  .  .  .  .  .  .  .  .  }
   218  .  .  .  .  .  .  .  .  .  Rbrace: foo:15:2
   219  .  .  .  .  .  .  .  .  }
   220  .  .  .  .  .  .  .  }
   221  .  .  .  .  .  .  }
   222  .  .  .  .  .  }
   223  .  .  .  .  .  1: *ast.ExprStmt {
   224  .  .  .  .  .  .  X: *ast.CallExpr {
   225  .  .  .  .  .  .  .  Fun: *ast.Ident {
   226  .  .  .  .  .  .  .  .  NamePos: foo:17:2
   227  .  .  .  .  .  .  .  .  Name: "sayHello"
   228  .  .  .  .  .  .  .  .  Obj: *(obj @ 35)
   229  .  .  .  .  .  .  .  }
   230  .  .  .  .  .  .  .  Lparen: foo:17:10
   231  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
   232  .  .  .  .  .  .  .  .  0: *ast.Ident {
   233  .  .  .  .  .  .  .  .  .  NamePos: foo:17:11
   234  .  .  .  .  .  .  .  .  .  Name: "f"
   235  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 166)
   236  .  .  .  .  .  .  .  .  }
   237  .  .  .  .  .  .  .  }
   238  .  .  .  .  .  .  .  Ellipsis: -
   239  .  .  .  .  .  .  .  Rparen: foo:17:12
   240  .  .  .  .  .  .  }
   241  .  .  .  .  .  }
   242  .  .  .  .  }
   243  .  .  .  .  Rbrace: foo:18:1
   244  .  .  .  }
   245  .  .  }
   246  .  }
   247  .  Scope: *ast.Scope {
   248  .  .  Outer: nil
   249  .  .  Objects: map[string]*ast.Object (len = 2) {
   250  .  .  .  "sayHello": *(obj @ 35)
   251  .  .  .  "main": *(obj @ 141)
   252  .  .  }
   253  .  }
   254  .  Imports: []*ast.ImportSpec (len = 1) {
   255  .  .  0: *(obj @ 15)
   256  .  }
   257  .  Unresolved: []*ast.Ident (len = 3) {
   258  .  .  0: *(obj @ 76)
   259  .  .  1: *(obj @ 102)
   260  .  .  2: *(obj @ 192)
   261  .  }
   262  .  Comments: []*ast.CommentGroup (len = 1) {
   263  .  .  0: *ast.CommentGroup {
   264  .  .  .  List: []*ast.Comment (len = 2) {
   265  .  .  .  .  0: *ast.Comment {
   266  .  .  .  .  .  Slash: foo:6:2
   267  .  .  .  .  .  Text: "// call the function"
   268  .  .  .  .  }
   269  .  .  .  .  1: *ast.Comment {
   270  .  .  .  .  .  Slash: foo:7:2
   271  .  .  .  .  .  Text: "// and print the result"
   272  .  .  .  .  }
   273  .  .  .  }
   274  .  .  }
   275  .  }
   276  }

###########################################################################

package main

import "fmt"

func sayHello(fn func()) {
	fn()
}

func main() {
	name := "Alice"

	sayHello(func() {
		fmt.Printf("Hello %s\n", name)
	})

}

     0  *ast.File {
     1  .  Doc: nil
     2  .  Package: foo:1:1
     3  .  Name: *ast.Ident {
     4  .  .  NamePos: foo:1:9
     5  .  .  Name: "main"
     6  .  .  Obj: nil
     7  .  }
     8  .  Decls: []ast.Decl (len = 3) {
     9  .  .  0: *ast.GenDecl {
    10  .  .  .  Doc: nil
    11  .  .  .  TokPos: foo:3:1
    12  .  .  .  Tok: import
    13  .  .  .  Lparen: -
    14  .  .  .  Specs: []ast.Spec (len = 1) {
    15  .  .  .  .  0: *ast.ImportSpec {
    16  .  .  .  .  .  Doc: nil
    17  .  .  .  .  .  Name: nil
    18  .  .  .  .  .  Path: *ast.BasicLit {
    19  .  .  .  .  .  .  ValuePos: foo:3:8
    20  .  .  .  .  .  .  Kind: STRING
    21  .  .  .  .  .  .  Value: "\"fmt\""
    22  .  .  .  .  .  }
    23  .  .  .  .  .  Comment: nil
    24  .  .  .  .  .  EndPos: -
    25  .  .  .  .  }
    26  .  .  .  }
    27  .  .  .  Rparen: -
    28  .  .  }
    29  .  .  1: *ast.FuncDecl {
    30  .  .  .  Doc: nil
    31  .  .  .  Recv: nil
    32  .  .  .  Name: *ast.Ident {
    33  .  .  .  .  NamePos: foo:5:6
    34  .  .  .  .  Name: "sayHello"
    35  .  .  .  .  Obj: *ast.Object {
    36  .  .  .  .  .  Kind: func
    37  .  .  .  .  .  Name: "sayHello"
    38  .  .  .  .  .  Decl: *(obj @ 29)
    39  .  .  .  .  .  Data: nil
    40  .  .  .  .  .  Type: nil
    41  .  .  .  .  }
    42  .  .  .  }
    43  .  .  .  Type: *ast.FuncType {
    44  .  .  .  .  Func: foo:5:1
    45  .  .  .  .  Params: *ast.FieldList {
    46  .  .  .  .  .  Opening: foo:5:14
    47  .  .  .  .  .  List: []*ast.Field (len = 1) {
    48  .  .  .  .  .  .  0: *ast.Field {
    49  .  .  .  .  .  .  .  Doc: nil
    50  .  .  .  .  .  .  .  Names: []*ast.Ident (len = 1) {
    51  .  .  .  .  .  .  .  .  0: *ast.Ident {
    52  .  .  .  .  .  .  .  .  .  NamePos: foo:5:15
    53  .  .  .  .  .  .  .  .  .  Name: "fn"
    54  .  .  .  .  .  .  .  .  .  Obj: *ast.Object {
    55  .  .  .  .  .  .  .  .  .  .  Kind: var
    56  .  .  .  .  .  .  .  .  .  .  Name: "fn"
    57  .  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 48)
    58  .  .  .  .  .  .  .  .  .  .  Data: nil
    59  .  .  .  .  .  .  .  .  .  .  Type: nil
    60  .  .  .  .  .  .  .  .  .  }
    61  .  .  .  .  .  .  .  .  }
    62  .  .  .  .  .  .  .  }
    63  .  .  .  .  .  .  .  Type: *ast.FuncType {
    64  .  .  .  .  .  .  .  .  Func: foo:5:18
    65  .  .  .  .  .  .  .  .  Params: *ast.FieldList {
    66  .  .  .  .  .  .  .  .  .  Opening: foo:5:22
    67  .  .  .  .  .  .  .  .  .  List: nil
    68  .  .  .  .  .  .  .  .  .  Closing: foo:5:23
    69  .  .  .  .  .  .  .  .  }
    70  .  .  .  .  .  .  .  .  Results: nil
    71  .  .  .  .  .  .  .  }
    72  .  .  .  .  .  .  .  Tag: nil
    73  .  .  .  .  .  .  .  Comment: nil
    74  .  .  .  .  .  .  }
    75  .  .  .  .  .  }
    76  .  .  .  .  .  Closing: foo:5:24
    77  .  .  .  .  }
    78  .  .  .  .  Results: nil
    79  .  .  .  }
    80  .  .  .  Body: *ast.BlockStmt {
    81  .  .  .  .  Lbrace: foo:5:26
    82  .  .  .  .  List: []ast.Stmt (len = 1) {
    83  .  .  .  .  .  0: *ast.ExprStmt {
    84  .  .  .  .  .  .  X: *ast.CallExpr {
    85  .  .  .  .  .  .  .  Fun: *ast.Ident {
    86  .  .  .  .  .  .  .  .  NamePos: foo:6:2
    87  .  .  .  .  .  .  .  .  Name: "fn"
    88  .  .  .  .  .  .  .  .  Obj: *(obj @ 54)
    89  .  .  .  .  .  .  .  }
    90  .  .  .  .  .  .  .  Lparen: foo:6:4
    91  .  .  .  .  .  .  .  Args: nil
    92  .  .  .  .  .  .  .  Ellipsis: -
    93  .  .  .  .  .  .  .  Rparen: foo:6:5
    94  .  .  .  .  .  .  }
    95  .  .  .  .  .  }
    96  .  .  .  .  }
    97  .  .  .  .  Rbrace: foo:7:1
    98  .  .  .  }
    99  .  .  }
   100  .  .  2: *ast.FuncDecl {
   101  .  .  .  Doc: nil
   102  .  .  .  Recv: nil
   103  .  .  .  Name: *ast.Ident {
   104  .  .  .  .  NamePos: foo:9:6
   105  .  .  .  .  Name: "main"
   106  .  .  .  .  Obj: *ast.Object {
   107  .  .  .  .  .  Kind: func
   108  .  .  .  .  .  Name: "main"
   109  .  .  .  .  .  Decl: *(obj @ 100)
   110  .  .  .  .  .  Data: nil
   111  .  .  .  .  .  Type: nil
   112  .  .  .  .  }
   113  .  .  .  }
   114  .  .  .  Type: *ast.FuncType {
   115  .  .  .  .  Func: foo:9:1
   116  .  .  .  .  Params: *ast.FieldList {
   117  .  .  .  .  .  Opening: foo:9:10
   118  .  .  .  .  .  List: nil
   119  .  .  .  .  .  Closing: foo:9:11
   120  .  .  .  .  }
   121  .  .  .  .  Results: nil
   122  .  .  .  }
   123  .  .  .  Body: *ast.BlockStmt {
   124  .  .  .  .  Lbrace: foo:9:13
   125  .  .  .  .  List: []ast.Stmt (len = 2) {
   126  .  .  .  .  .  0: *ast.AssignStmt {
   127  .  .  .  .  .  .  Lhs: []ast.Expr (len = 1) {
   128  .  .  .  .  .  .  .  0: *ast.Ident {
   129  .  .  .  .  .  .  .  .  NamePos: foo:10:2
   130  .  .  .  .  .  .  .  .  Name: "name"
   131  .  .  .  .  .  .  .  .  Obj: *ast.Object {
   132  .  .  .  .  .  .  .  .  .  Kind: var
   133  .  .  .  .  .  .  .  .  .  Name: "name"
   134  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 126)
   135  .  .  .  .  .  .  .  .  .  Data: nil
   136  .  .  .  .  .  .  .  .  .  Type: nil
   137  .  .  .  .  .  .  .  .  }
   138  .  .  .  .  .  .  .  }
   139  .  .  .  .  .  .  }
   140  .  .  .  .  .  .  TokPos: foo:10:7
   141  .  .  .  .  .  .  Tok: :=
   142  .  .  .  .  .  .  Rhs: []ast.Expr (len = 1) {
   143  .  .  .  .  .  .  .  0: *ast.BasicLit {
   144  .  .  .  .  .  .  .  .  ValuePos: foo:10:10
   145  .  .  .  .  .  .  .  .  Kind: STRING
   146  .  .  .  .  .  .  .  .  Value: "\"Alice\""
   147  .  .  .  .  .  .  .  }
   148  .  .  .  .  .  .  }
   149  .  .  .  .  .  }
   150  .  .  .  .  .  1: *ast.ExprStmt {
   151  .  .  .  .  .  .  X: *ast.CallExpr {
   152  .  .  .  .  .  .  .  Fun: *ast.Ident {
   153  .  .  .  .  .  .  .  .  NamePos: foo:12:2
   154  .  .  .  .  .  .  .  .  Name: "sayHello"
   155  .  .  .  .  .  .  .  .  Obj: *(obj @ 35)
   156  .  .  .  .  .  .  .  }
   157  .  .  .  .  .  .  .  Lparen: foo:12:10
   158  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
   159  .  .  .  .  .  .  .  .  0: *ast.FuncLit {
   160  .  .  .  .  .  .  .  .  .  Type: *ast.FuncType {
   161  .  .  .  .  .  .  .  .  .  .  Func: foo:12:11
   162  .  .  .  .  .  .  .  .  .  .  Params: *ast.FieldList {
   163  .  .  .  .  .  .  .  .  .  .  .  Opening: foo:12:15
   164  .  .  .  .  .  .  .  .  .  .  .  List: nil
   165  .  .  .  .  .  .  .  .  .  .  .  Closing: foo:12:16
   166  .  .  .  .  .  .  .  .  .  .  }
   167  .  .  .  .  .  .  .  .  .  .  Results: nil
   168  .  .  .  .  .  .  .  .  .  }
   169  .  .  .  .  .  .  .  .  .  Body: *ast.BlockStmt {
   170  .  .  .  .  .  .  .  .  .  .  Lbrace: foo:12:18
   171  .  .  .  .  .  .  .  .  .  .  List: []ast.Stmt (len = 1) {
   172  .  .  .  .  .  .  .  .  .  .  .  0: *ast.ExprStmt {
   173  .  .  .  .  .  .  .  .  .  .  .  .  X: *ast.CallExpr {
   174  .  .  .  .  .  .  .  .  .  .  .  .  .  Fun: *ast.SelectorExpr {
   175  .  .  .  .  .  .  .  .  .  .  .  .  .  .  X: *ast.Ident {
   176  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:13:3
   177  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Name: "fmt"
   178  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Obj: nil
   179  .  .  .  .  .  .  .  .  .  .  .  .  .  .  }
   180  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
   181  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:13:7
   182  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Name: "Printf"
   183  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Obj: nil
   184  .  .  .  .  .  .  .  .  .  .  .  .  .  .  }
   185  .  .  .  .  .  .  .  .  .  .  .  .  .  }
   186  .  .  .  .  .  .  .  .  .  .  .  .  .  Lparen: foo:13:13
   187  .  .  .  .  .  .  .  .  .  .  .  .  .  Args: []ast.Expr (len = 2) {
   188  .  .  .  .  .  .  .  .  .  .  .  .  .  .  0: *ast.BasicLit {
   189  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  ValuePos: foo:13:14
   190  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Kind: STRING
   191  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Value: "\"Hello %s\\n\""
   192  .  .  .  .  .  .  .  .  .  .  .  .  .  .  }
   193  .  .  .  .  .  .  .  .  .  .  .  .  .  .  1: *ast.Ident {
   194  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:13:28
   195  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Name: "name"
   196  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 131)
   197  .  .  .  .  .  .  .  .  .  .  .  .  .  .  }
   198  .  .  .  .  .  .  .  .  .  .  .  .  .  }
   199  .  .  .  .  .  .  .  .  .  .  .  .  .  Ellipsis: -
   200  .  .  .  .  .  .  .  .  .  .  .  .  .  Rparen: foo:13:32
   201  .  .  .  .  .  .  .  .  .  .  .  .  }
   202  .  .  .  .  .  .  .  .  .  .  .  }
   203  .  .  .  .  .  .  .  .  .  .  }
   204  .  .  .  .  .  .  .  .  .  .  Rbrace: foo:14:2
   205  .  .  .  .  .  .  .  .  .  }
   206  .  .  .  .  .  .  .  .  }
   207  .  .  .  .  .  .  .  }
   208  .  .  .  .  .  .  .  Ellipsis: -
   209  .  .  .  .  .  .  .  Rparen: foo:14:3
   210  .  .  .  .  .  .  }
   211  .  .  .  .  .  }
   212  .  .  .  .  }
   213  .  .  .  .  Rbrace: foo:16:1
   214  .  .  .  }
   215  .  .  }
   216  .  }
   217  .  Scope: *ast.Scope {
   218  .  .  Outer: nil
   219  .  .  Objects: map[string]*ast.Object (len = 2) {
   220  .  .  .  "sayHello": *(obj @ 35)
   221  .  .  .  "main": *(obj @ 106)
   222  .  .  }
   223  .  }
   224  .  Imports: []*ast.ImportSpec (len = 1) {
   225  .  .  0: *(obj @ 15)
   226  .  }
   227  .  Unresolved: []*ast.Ident (len = 1) {
   228  .  .  0: *(obj @ 175)
   229  .  }
   230  .  Comments: nil
   231  }

###############################################################

package main

import "fmt"

func returnTwo() (string, string) {
	return "hello", "world"
}

func takeTwo(a string, b string) {
	fmt.Println(a, b)
}

func main() {
	takeTwo(returnTwo())
}


     0  *ast.File {
     1  .  Doc: nil
     2  .  Package: foo:1:1
     3  .  Name: *ast.Ident {
     4  .  .  NamePos: foo:1:9
     5  .  .  Name: "main"
     6  .  .  Obj: nil
     7  .  }
     8  .  Decls: []ast.Decl (len = 4) {
     9  .  .  0: *ast.GenDecl {
    10  .  .  .  Doc: nil
    11  .  .  .  TokPos: foo:3:1
    12  .  .  .  Tok: import
    13  .  .  .  Lparen: -
    14  .  .  .  Specs: []ast.Spec (len = 1) {
    15  .  .  .  .  0: *ast.ImportSpec {
    16  .  .  .  .  .  Doc: nil
    17  .  .  .  .  .  Name: nil
    18  .  .  .  .  .  Path: *ast.BasicLit {
    19  .  .  .  .  .  .  ValuePos: foo:3:8
    20  .  .  .  .  .  .  Kind: STRING
    21  .  .  .  .  .  .  Value: "\"fmt\""
    22  .  .  .  .  .  }
    23  .  .  .  .  .  Comment: nil
    24  .  .  .  .  .  EndPos: -
    25  .  .  .  .  }
    26  .  .  .  }
    27  .  .  .  Rparen: -
    28  .  .  }
    29  .  .  1: *ast.FuncDecl {
    30  .  .  .  Doc: nil
    31  .  .  .  Recv: nil
    32  .  .  .  Name: *ast.Ident {
    33  .  .  .  .  NamePos: foo:5:6
    34  .  .  .  .  Name: "returnTwo"
    35  .  .  .  .  Obj: *ast.Object {
    36  .  .  .  .  .  Kind: func
    37  .  .  .  .  .  Name: "returnTwo"
    38  .  .  .  .  .  Decl: *(obj @ 29)
    39  .  .  .  .  .  Data: nil
    40  .  .  .  .  .  Type: nil
    41  .  .  .  .  }
    42  .  .  .  }
    43  .  .  .  Type: *ast.FuncType {
    44  .  .  .  .  Func: foo:5:1
    45  .  .  .  .  Params: *ast.FieldList {
    46  .  .  .  .  .  Opening: foo:5:15
    47  .  .  .  .  .  List: nil
    48  .  .  .  .  .  Closing: foo:5:16
    49  .  .  .  .  }
    50  .  .  .  .  Results: *ast.FieldList {
    51  .  .  .  .  .  Opening: foo:5:18
    52  .  .  .  .  .  List: []*ast.Field (len = 2) {
    53  .  .  .  .  .  .  0: *ast.Field {
    54  .  .  .  .  .  .  .  Doc: nil
    55  .  .  .  .  .  .  .  Names: nil
    56  .  .  .  .  .  .  .  Type: *ast.Ident {
    57  .  .  .  .  .  .  .  .  NamePos: foo:5:19
    58  .  .  .  .  .  .  .  .  Name: "string"
    59  .  .  .  .  .  .  .  .  Obj: nil
    60  .  .  .  .  .  .  .  }
    61  .  .  .  .  .  .  .  Tag: nil
    62  .  .  .  .  .  .  .  Comment: nil
    63  .  .  .  .  .  .  }
    64  .  .  .  .  .  .  1: *ast.Field {
    65  .  .  .  .  .  .  .  Doc: nil
    66  .  .  .  .  .  .  .  Names: nil
    67  .  .  .  .  .  .  .  Type: *ast.Ident {
    68  .  .  .  .  .  .  .  .  NamePos: foo:5:27
    69  .  .  .  .  .  .  .  .  Name: "string"
    70  .  .  .  .  .  .  .  .  Obj: nil
    71  .  .  .  .  .  .  .  }
    72  .  .  .  .  .  .  .  Tag: nil
    73  .  .  .  .  .  .  .  Comment: nil
    74  .  .  .  .  .  .  }
    75  .  .  .  .  .  }
    76  .  .  .  .  .  Closing: foo:5:33
    77  .  .  .  .  }
    78  .  .  .  }
    79  .  .  .  Body: *ast.BlockStmt {
    80  .  .  .  .  Lbrace: foo:5:35
    81  .  .  .  .  List: []ast.Stmt (len = 1) {
    82  .  .  .  .  .  0: *ast.ReturnStmt {
    83  .  .  .  .  .  .  Return: foo:6:2
    84  .  .  .  .  .  .  Results: []ast.Expr (len = 2) {
    85  .  .  .  .  .  .  .  0: *ast.BasicLit {
    86  .  .  .  .  .  .  .  .  ValuePos: foo:6:9
    87  .  .  .  .  .  .  .  .  Kind: STRING
    88  .  .  .  .  .  .  .  .  Value: "\"hello\""
    89  .  .  .  .  .  .  .  }
    90  .  .  .  .  .  .  .  1: *ast.BasicLit {
    91  .  .  .  .  .  .  .  .  ValuePos: foo:6:18
    92  .  .  .  .  .  .  .  .  Kind: STRING
    93  .  .  .  .  .  .  .  .  Value: "\"world\""
    94  .  .  .  .  .  .  .  }
    95  .  .  .  .  .  .  }
    96  .  .  .  .  .  }
    97  .  .  .  .  }
    98  .  .  .  .  Rbrace: foo:7:1
    99  .  .  .  }
   100  .  .  }
   101  .  .  2: *ast.FuncDecl {
   102  .  .  .  Doc: nil
   103  .  .  .  Recv: nil
   104  .  .  .  Name: *ast.Ident {
   105  .  .  .  .  NamePos: foo:9:6
   106  .  .  .  .  Name: "takeTwo"
   107  .  .  .  .  Obj: *ast.Object {
   108  .  .  .  .  .  Kind: func
   109  .  .  .  .  .  Name: "takeTwo"
   110  .  .  .  .  .  Decl: *(obj @ 101)
   111  .  .  .  .  .  Data: nil
   112  .  .  .  .  .  Type: nil
   113  .  .  .  .  }
   114  .  .  .  }
   115  .  .  .  Type: *ast.FuncType {
   116  .  .  .  .  Func: foo:9:1
   117  .  .  .  .  Params: *ast.FieldList {
   118  .  .  .  .  .  Opening: foo:9:13
   119  .  .  .  .  .  List: []*ast.Field (len = 2) {
   120  .  .  .  .  .  .  0: *ast.Field {
   121  .  .  .  .  .  .  .  Doc: nil
   122  .  .  .  .  .  .  .  Names: []*ast.Ident (len = 1) {
   123  .  .  .  .  .  .  .  .  0: *ast.Ident {
   124  .  .  .  .  .  .  .  .  .  NamePos: foo:9:14
   125  .  .  .  .  .  .  .  .  .  Name: "a"
   126  .  .  .  .  .  .  .  .  .  Obj: *ast.Object {
   127  .  .  .  .  .  .  .  .  .  .  Kind: var
   128  .  .  .  .  .  .  .  .  .  .  Name: "a"
   129  .  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 120)
   130  .  .  .  .  .  .  .  .  .  .  Data: nil
   131  .  .  .  .  .  .  .  .  .  .  Type: nil
   132  .  .  .  .  .  .  .  .  .  }
   133  .  .  .  .  .  .  .  .  }
   134  .  .  .  .  .  .  .  }
   135  .  .  .  .  .  .  .  Type: *ast.Ident {
   136  .  .  .  .  .  .  .  .  NamePos: foo:9:16
   137  .  .  .  .  .  .  .  .  Name: "string"
   138  .  .  .  .  .  .  .  .  Obj: nil
   139  .  .  .  .  .  .  .  }
   140  .  .  .  .  .  .  .  Tag: nil
   141  .  .  .  .  .  .  .  Comment: nil
   142  .  .  .  .  .  .  }
   143  .  .  .  .  .  .  1: *ast.Field {
   144  .  .  .  .  .  .  .  Doc: nil
   145  .  .  .  .  .  .  .  Names: []*ast.Ident (len = 1) {
   146  .  .  .  .  .  .  .  .  0: *ast.Ident {
   147  .  .  .  .  .  .  .  .  .  NamePos: foo:9:24
   148  .  .  .  .  .  .  .  .  .  Name: "b"
   149  .  .  .  .  .  .  .  .  .  Obj: *ast.Object {
   150  .  .  .  .  .  .  .  .  .  .  Kind: var
   151  .  .  .  .  .  .  .  .  .  .  Name: "b"
   152  .  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 143)
   153  .  .  .  .  .  .  .  .  .  .  Data: nil
   154  .  .  .  .  .  .  .  .  .  .  Type: nil
   155  .  .  .  .  .  .  .  .  .  }
   156  .  .  .  .  .  .  .  .  }
   157  .  .  .  .  .  .  .  }
   158  .  .  .  .  .  .  .  Type: *ast.Ident {
   159  .  .  .  .  .  .  .  .  NamePos: foo:9:26
   160  .  .  .  .  .  .  .  .  Name: "string"
   161  .  .  .  .  .  .  .  .  Obj: nil
   162  .  .  .  .  .  .  .  }
   163  .  .  .  .  .  .  .  Tag: nil
   164  .  .  .  .  .  .  .  Comment: nil
   165  .  .  .  .  .  .  }
   166  .  .  .  .  .  }
   167  .  .  .  .  .  Closing: foo:9:32
   168  .  .  .  .  }
   169  .  .  .  .  Results: nil
   170  .  .  .  }
   171  .  .  .  Body: *ast.BlockStmt {
   172  .  .  .  .  Lbrace: foo:9:34
   173  .  .  .  .  List: []ast.Stmt (len = 1) {
   174  .  .  .  .  .  0: *ast.ExprStmt {
   175  .  .  .  .  .  .  X: *ast.CallExpr {
   176  .  .  .  .  .  .  .  Fun: *ast.SelectorExpr {
   177  .  .  .  .  .  .  .  .  X: *ast.Ident {
   178  .  .  .  .  .  .  .  .  .  NamePos: foo:10:2
   179  .  .  .  .  .  .  .  .  .  Name: "fmt"
   180  .  .  .  .  .  .  .  .  .  Obj: nil
   181  .  .  .  .  .  .  .  .  }
   182  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
   183  .  .  .  .  .  .  .  .  .  NamePos: foo:10:6
   184  .  .  .  .  .  .  .  .  .  Name: "Println"
   185  .  .  .  .  .  .  .  .  .  Obj: nil
   186  .  .  .  .  .  .  .  .  }
   187  .  .  .  .  .  .  .  }
   188  .  .  .  .  .  .  .  Lparen: foo:10:13
   189  .  .  .  .  .  .  .  Args: []ast.Expr (len = 2) {
   190  .  .  .  .  .  .  .  .  0: *ast.Ident {
   191  .  .  .  .  .  .  .  .  .  NamePos: foo:10:14
   192  .  .  .  .  .  .  .  .  .  Name: "a"
   193  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 126)
   194  .  .  .  .  .  .  .  .  }
   195  .  .  .  .  .  .  .  .  1: *ast.Ident {
   196  .  .  .  .  .  .  .  .  .  NamePos: foo:10:17
   197  .  .  .  .  .  .  .  .  .  Name: "b"
   198  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 149)
   199  .  .  .  .  .  .  .  .  }
   200  .  .  .  .  .  .  .  }
   201  .  .  .  .  .  .  .  Ellipsis: -
   202  .  .  .  .  .  .  .  Rparen: foo:10:18
   203  .  .  .  .  .  .  }
   204  .  .  .  .  .  }
   205  .  .  .  .  }
   206  .  .  .  .  Rbrace: foo:11:1
   207  .  .  .  }
   208  .  .  }
   209  .  .  3: *ast.FuncDecl {
   210  .  .  .  Doc: nil
   211  .  .  .  Recv: nil
   212  .  .  .  Name: *ast.Ident {
   213  .  .  .  .  NamePos: foo:13:6
   214  .  .  .  .  Name: "main"
   215  .  .  .  .  Obj: *ast.Object {
   216  .  .  .  .  .  Kind: func
   217  .  .  .  .  .  Name: "main"
   218  .  .  .  .  .  Decl: *(obj @ 209)
   219  .  .  .  .  .  Data: nil
   220  .  .  .  .  .  Type: nil
   221  .  .  .  .  }
   222  .  .  .  }
   223  .  .  .  Type: *ast.FuncType {
   224  .  .  .  .  Func: foo:13:1
   225  .  .  .  .  Params: *ast.FieldList {
   226  .  .  .  .  .  Opening: foo:13:10
   227  .  .  .  .  .  List: nil
   228  .  .  .  .  .  Closing: foo:13:11
   229  .  .  .  .  }
   230  .  .  .  .  Results: nil
   231  .  .  .  }
   232  .  .  .  Body: *ast.BlockStmt {
   233  .  .  .  .  Lbrace: foo:13:13
   234  .  .  .  .  List: []ast.Stmt (len = 1) {
   235  .  .  .  .  .  0: *ast.ExprStmt {
   236  .  .  .  .  .  .  X: *ast.CallExpr {
   237  .  .  .  .  .  .  .  Fun: *ast.Ident {
   238  .  .  .  .  .  .  .  .  NamePos: foo:14:2
   239  .  .  .  .  .  .  .  .  Name: "takeTwo"
   240  .  .  .  .  .  .  .  .  Obj: *(obj @ 107)
   241  .  .  .  .  .  .  .  }
   242  .  .  .  .  .  .  .  Lparen: foo:14:9
   243  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
   244  .  .  .  .  .  .  .  .  0: *ast.CallExpr {
   245  .  .  .  .  .  .  .  .  .  Fun: *ast.Ident {
   246  .  .  .  .  .  .  .  .  .  .  NamePos: foo:14:10
   247  .  .  .  .  .  .  .  .  .  .  Name: "returnTwo"
   248  .  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 35)
   249  .  .  .  .  .  .  .  .  .  }
   250  .  .  .  .  .  .  .  .  .  Lparen: foo:14:19
   251  .  .  .  .  .  .  .  .  .  Args: nil
   252  .  .  .  .  .  .  .  .  .  Ellipsis: -
   253  .  .  .  .  .  .  .  .  .  Rparen: foo:14:20
   254  .  .  .  .  .  .  .  .  }
   255  .  .  .  .  .  .  .  }
   256  .  .  .  .  .  .  .  Ellipsis: -
   257  .  .  .  .  .  .  .  Rparen: foo:14:21
   258  .  .  .  .  .  .  }
   259  .  .  .  .  .  }
   260  .  .  .  .  }
   261  .  .  .  .  Rbrace: foo:15:1
   262  .  .  .  }
   263  .  .  }
   264  .  }
   265  .  Scope: *ast.Scope {
   266  .  .  Outer: nil
   267  .  .  Objects: map[string]*ast.Object (len = 3) {
   268  .  .  .  "returnTwo": *(obj @ 35)
   269  .  .  .  "takeTwo": *(obj @ 107)
   270  .  .  .  "main": *(obj @ 215)
   271  .  .  }
   272  .  }
   273  .  Imports: []*ast.ImportSpec (len = 1) {
   274  .  .  0: *(obj @ 15)
   275  .  }
   276  .  Unresolved: []*ast.Ident (len = 5) {
   277  .  .  0: *(obj @ 56)
   278  .  .  1: *(obj @ 67)
   279  .  .  2: *(obj @ 135)
   280  .  .  3: *(obj @ 158)
   281  .  .  4: *(obj @ 177)
   282  .  }
   283  .  Comments: nil
   284  }
 
##############################################################################

package main

import "fmt"

func sayHello(names ...string) {
	for _, n := range names {
		fmt.Printf("Hello %s\n", n)
	}
}

func main() {
	sayHello("Alice", "Bob", "Carol")
}


     0  *ast.File {
     1  .  Doc: nil
     2  .  Package: foo:1:1
     3  .  Name: *ast.Ident {
     4  .  .  NamePos: foo:1:9
     5  .  .  Name: "main"
     6  .  .  Obj: nil
     7  .  }
     8  .  Decls: []ast.Decl (len = 3) {
     9  .  .  0: *ast.GenDecl {
    10  .  .  .  Doc: nil
    11  .  .  .  TokPos: foo:3:1
    12  .  .  .  Tok: import
    13  .  .  .  Lparen: -
    14  .  .  .  Specs: []ast.Spec (len = 1) {
    15  .  .  .  .  0: *ast.ImportSpec {
    16  .  .  .  .  .  Doc: nil
    17  .  .  .  .  .  Name: nil
    18  .  .  .  .  .  Path: *ast.BasicLit {
    19  .  .  .  .  .  .  ValuePos: foo:3:8
    20  .  .  .  .  .  .  Kind: STRING
    21  .  .  .  .  .  .  Value: "\"fmt\""
    22  .  .  .  .  .  }
    23  .  .  .  .  .  Comment: nil
    24  .  .  .  .  .  EndPos: -
    25  .  .  .  .  }
    26  .  .  .  }
    27  .  .  .  Rparen: -
    28  .  .  }
    29  .  .  1: *ast.FuncDecl {
    30  .  .  .  Doc: nil
    31  .  .  .  Recv: nil
    32  .  .  .  Name: *ast.Ident {
    33  .  .  .  .  NamePos: foo:5:6
    34  .  .  .  .  Name: "sayHello"
    35  .  .  .  .  Obj: *ast.Object {
    36  .  .  .  .  .  Kind: func
    37  .  .  .  .  .  Name: "sayHello"
    38  .  .  .  .  .  Decl: *(obj @ 29)
    39  .  .  .  .  .  Data: nil
    40  .  .  .  .  .  Type: nil
    41  .  .  .  .  }
    42  .  .  .  }
    43  .  .  .  Type: *ast.FuncType {
    44  .  .  .  .  Func: foo:5:1
    45  .  .  .  .  Params: *ast.FieldList {
    46  .  .  .  .  .  Opening: foo:5:14
    47  .  .  .  .  .  List: []*ast.Field (len = 1) {
    48  .  .  .  .  .  .  0: *ast.Field {
    49  .  .  .  .  .  .  .  Doc: nil
    50  .  .  .  .  .  .  .  Names: []*ast.Ident (len = 1) {
    51  .  .  .  .  .  .  .  .  0: *ast.Ident {
    52  .  .  .  .  .  .  .  .  .  NamePos: foo:5:15
    53  .  .  .  .  .  .  .  .  .  Name: "names"
    54  .  .  .  .  .  .  .  .  .  Obj: *ast.Object {
    55  .  .  .  .  .  .  .  .  .  .  Kind: var
    56  .  .  .  .  .  .  .  .  .  .  Name: "names"
    57  .  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 48)
    58  .  .  .  .  .  .  .  .  .  .  Data: nil
    59  .  .  .  .  .  .  .  .  .  .  Type: nil
    60  .  .  .  .  .  .  .  .  .  }
    61  .  .  .  .  .  .  .  .  }
    62  .  .  .  .  .  .  .  }
    63  .  .  .  .  .  .  .  Type: *ast.Ellipsis {
    64  .  .  .  .  .  .  .  .  Ellipsis: foo:5:21
    65  .  .  .  .  .  .  .  .  Elt: *ast.Ident {
    66  .  .  .  .  .  .  .  .  .  NamePos: foo:5:24
    67  .  .  .  .  .  .  .  .  .  Name: "string"
    68  .  .  .  .  .  .  .  .  .  Obj: nil
    69  .  .  .  .  .  .  .  .  }
    70  .  .  .  .  .  .  .  }
    71  .  .  .  .  .  .  .  Tag: nil
    72  .  .  .  .  .  .  .  Comment: nil
    73  .  .  .  .  .  .  }
    74  .  .  .  .  .  }
    75  .  .  .  .  .  Closing: foo:5:30
    76  .  .  .  .  }
    77  .  .  .  .  Results: nil
    78  .  .  .  }
    79  .  .  .  Body: *ast.BlockStmt {
    80  .  .  .  .  Lbrace: foo:5:32
    81  .  .  .  .  List: []ast.Stmt (len = 1) {
    82  .  .  .  .  .  0: *ast.RangeStmt {
    83  .  .  .  .  .  .  For: foo:6:2
    84  .  .  .  .  .  .  Key: *ast.Ident {
    85  .  .  .  .  .  .  .  NamePos: foo:6:6
    86  .  .  .  .  .  .  .  Name: "_"
    87  .  .  .  .  .  .  .  Obj: *ast.Object {
    88  .  .  .  .  .  .  .  .  Kind: var
    89  .  .  .  .  .  .  .  .  Name: "_"
    90  .  .  .  .  .  .  .  .  Decl: *ast.AssignStmt {
    91  .  .  .  .  .  .  .  .  .  Lhs: []ast.Expr (len = 2) {
    92  .  .  .  .  .  .  .  .  .  .  0: *(obj @ 84)
    93  .  .  .  .  .  .  .  .  .  .  1: *ast.Ident {
    94  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:6:9
    95  .  .  .  .  .  .  .  .  .  .  .  Name: "n"
    96  .  .  .  .  .  .  .  .  .  .  .  Obj: *ast.Object {
    97  .  .  .  .  .  .  .  .  .  .  .  .  Kind: var
    98  .  .  .  .  .  .  .  .  .  .  .  .  Name: "n"
    99  .  .  .  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 90)
   100  .  .  .  .  .  .  .  .  .  .  .  .  Data: nil
   101  .  .  .  .  .  .  .  .  .  .  .  .  Type: nil
   102  .  .  .  .  .  .  .  .  .  .  .  }
   103  .  .  .  .  .  .  .  .  .  .  }
   104  .  .  .  .  .  .  .  .  .  }
   105  .  .  .  .  .  .  .  .  .  TokPos: foo:6:11
   106  .  .  .  .  .  .  .  .  .  Tok: :=
   107  .  .  .  .  .  .  .  .  .  Rhs: []ast.Expr (len = 1) {
   108  .  .  .  .  .  .  .  .  .  .  0: *ast.UnaryExpr {
   109  .  .  .  .  .  .  .  .  .  .  .  OpPos: foo:6:14
   110  .  .  .  .  .  .  .  .  .  .  .  Op: range
   111  .  .  .  .  .  .  .  .  .  .  .  X: *ast.Ident {
   112  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:6:20
   113  .  .  .  .  .  .  .  .  .  .  .  .  Name: "names"
   114  .  .  .  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 54)
   115  .  .  .  .  .  .  .  .  .  .  .  }
   116  .  .  .  .  .  .  .  .  .  .  }
   117  .  .  .  .  .  .  .  .  .  }
   118  .  .  .  .  .  .  .  .  }
   119  .  .  .  .  .  .  .  .  Data: nil
   120  .  .  .  .  .  .  .  .  Type: nil
   121  .  .  .  .  .  .  .  }
   122  .  .  .  .  .  .  }
   123  .  .  .  .  .  .  Value: *(obj @ 93)
   124  .  .  .  .  .  .  TokPos: foo:6:11
   125  .  .  .  .  .  .  Tok: :=
   126  .  .  .  .  .  .  X: *(obj @ 111)
   127  .  .  .  .  .  .  Body: *ast.BlockStmt {
   128  .  .  .  .  .  .  .  Lbrace: foo:6:26
   129  .  .  .  .  .  .  .  List: []ast.Stmt (len = 1) {
   130  .  .  .  .  .  .  .  .  0: *ast.ExprStmt {
   131  .  .  .  .  .  .  .  .  .  X: *ast.CallExpr {
   132  .  .  .  .  .  .  .  .  .  .  Fun: *ast.SelectorExpr {
   133  .  .  .  .  .  .  .  .  .  .  .  X: *ast.Ident {
   134  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:7:3
   135  .  .  .  .  .  .  .  .  .  .  .  .  Name: "fmt"
   136  .  .  .  .  .  .  .  .  .  .  .  .  Obj: nil
   137  .  .  .  .  .  .  .  .  .  .  .  }
   138  .  .  .  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
   139  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:7:7
   140  .  .  .  .  .  .  .  .  .  .  .  .  Name: "Printf"
   141  .  .  .  .  .  .  .  .  .  .  .  .  Obj: nil
   142  .  .  .  .  .  .  .  .  .  .  .  }
   143  .  .  .  .  .  .  .  .  .  .  }
   144  .  .  .  .  .  .  .  .  .  .  Lparen: foo:7:13
   145  .  .  .  .  .  .  .  .  .  .  Args: []ast.Expr (len = 2) {
   146  .  .  .  .  .  .  .  .  .  .  .  0: *ast.BasicLit {
   147  .  .  .  .  .  .  .  .  .  .  .  .  ValuePos: foo:7:14
   148  .  .  .  .  .  .  .  .  .  .  .  .  Kind: STRING
   149  .  .  .  .  .  .  .  .  .  .  .  .  Value: "\"Hello %s\\n\""
   150  .  .  .  .  .  .  .  .  .  .  .  }
   151  .  .  .  .  .  .  .  .  .  .  .  1: *ast.Ident {
   152  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:7:28
   153  .  .  .  .  .  .  .  .  .  .  .  .  Name: "n"
   154  .  .  .  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 96)
   155  .  .  .  .  .  .  .  .  .  .  .  }
   156  .  .  .  .  .  .  .  .  .  .  }
   157  .  .  .  .  .  .  .  .  .  .  Ellipsis: -
   158  .  .  .  .  .  .  .  .  .  .  Rparen: foo:7:29
   159  .  .  .  .  .  .  .  .  .  }
   160  .  .  .  .  .  .  .  .  }
   161  .  .  .  .  .  .  .  }
   162  .  .  .  .  .  .  .  Rbrace: foo:8:2
   163  .  .  .  .  .  .  }
   164  .  .  .  .  .  }
   165  .  .  .  .  }
   166  .  .  .  .  Rbrace: foo:9:1
   167  .  .  .  }
   168  .  .  }
   169  .  .  2: *ast.FuncDecl {
   170  .  .  .  Doc: nil
   171  .  .  .  Recv: nil
   172  .  .  .  Name: *ast.Ident {
   173  .  .  .  .  NamePos: foo:11:6
   174  .  .  .  .  Name: "main"
   175  .  .  .  .  Obj: *ast.Object {
   176  .  .  .  .  .  Kind: func
   177  .  .  .  .  .  Name: "main"
   178  .  .  .  .  .  Decl: *(obj @ 169)
   179  .  .  .  .  .  Data: nil
   180  .  .  .  .  .  Type: nil
   181  .  .  .  .  }
   182  .  .  .  }
   183  .  .  .  Type: *ast.FuncType {
   184  .  .  .  .  Func: foo:11:1
   185  .  .  .  .  Params: *ast.FieldList {
   186  .  .  .  .  .  Opening: foo:11:10
   187  .  .  .  .  .  List: nil
   188  .  .  .  .  .  Closing: foo:11:11
   189  .  .  .  .  }
   190  .  .  .  .  Results: nil
   191  .  .  .  }
   192  .  .  .  Body: *ast.BlockStmt {
   193  .  .  .  .  Lbrace: foo:11:13
   194  .  .  .  .  List: []ast.Stmt (len = 1) {
   195  .  .  .  .  .  0: *ast.ExprStmt {
   196  .  .  .  .  .  .  X: *ast.CallExpr {
   197  .  .  .  .  .  .  .  Fun: *ast.Ident {
   198  .  .  .  .  .  .  .  .  NamePos: foo:12:2
   199  .  .  .  .  .  .  .  .  Name: "sayHello"
   200  .  .  .  .  .  .  .  .  Obj: *(obj @ 35)
   201  .  .  .  .  .  .  .  }
   202  .  .  .  .  .  .  .  Lparen: foo:12:10
   203  .  .  .  .  .  .  .  Args: []ast.Expr (len = 3) {
   204  .  .  .  .  .  .  .  .  0: *ast.BasicLit {
   205  .  .  .  .  .  .  .  .  .  ValuePos: foo:12:11
   206  .  .  .  .  .  .  .  .  .  Kind: STRING
   207  .  .  .  .  .  .  .  .  .  Value: "\"Alice\""
   208  .  .  .  .  .  .  .  .  }
   209  .  .  .  .  .  .  .  .  1: *ast.BasicLit {
   210  .  .  .  .  .  .  .  .  .  ValuePos: foo:12:20
   211  .  .  .  .  .  .  .  .  .  Kind: STRING
   212  .  .  .  .  .  .  .  .  .  Value: "\"Bob\""
   213  .  .  .  .  .  .  .  .  }
   214  .  .  .  .  .  .  .  .  2: *ast.BasicLit {
   215  .  .  .  .  .  .  .  .  .  ValuePos: foo:12:27
   216  .  .  .  .  .  .  .  .  .  Kind: STRING
   217  .  .  .  .  .  .  .  .  .  Value: "\"Carol\""
   218  .  .  .  .  .  .  .  .  }
   219  .  .  .  .  .  .  .  }
   220  .  .  .  .  .  .  .  Ellipsis: -
   221  .  .  .  .  .  .  .  Rparen: foo:12:34
   222  .  .  .  .  .  .  }
   223  .  .  .  .  .  }
   224  .  .  .  .  }
   225  .  .  .  .  Rbrace: foo:13:1
   226  .  .  .  }
   227  .  .  }
   228  .  }
   229  .  Scope: *ast.Scope {
   230  .  .  Outer: nil
   231  .  .  Objects: map[string]*ast.Object (len = 2) {
   232  .  .  .  "sayHello": *(obj @ 35)
   233  .  .  .  "main": *(obj @ 175)
   234  .  .  }
   235  .  }
   236  .  Imports: []*ast.ImportSpec (len = 1) {
   237  .  .  0: *(obj @ 15)
   238  .  }
   239  .  Unresolved: []*ast.Ident (len = 2) {
   240  .  .  0: *(obj @ 65)
   241  .  .  1: *(obj @ 133)
   242  .  }
   243  .  Comments: nil
   244  }

##############################################################

package main

import "fmt"

func main() {
	defer fmt.Println("goodbye")

	fmt.Println("hello")
}


     0  *ast.File {
     1  .  Doc: nil
     2  .  Package: foo:1:1
     3  .  Name: *ast.Ident {
     4  .  .  NamePos: foo:1:9
     5  .  .  Name: "main"
     6  .  .  Obj: nil
     7  .  }
     8  .  Decls: []ast.Decl (len = 2) {
     9  .  .  0: *ast.GenDecl {
    10  .  .  .  Doc: nil
    11  .  .  .  TokPos: foo:3:1
    12  .  .  .  Tok: import
    13  .  .  .  Lparen: -
    14  .  .  .  Specs: []ast.Spec (len = 1) {
    15  .  .  .  .  0: *ast.ImportSpec {
    16  .  .  .  .  .  Doc: nil
    17  .  .  .  .  .  Name: nil
    18  .  .  .  .  .  Path: *ast.BasicLit {
    19  .  .  .  .  .  .  ValuePos: foo:3:8
    20  .  .  .  .  .  .  Kind: STRING
    21  .  .  .  .  .  .  Value: "\"fmt\""
    22  .  .  .  .  .  }
    23  .  .  .  .  .  Comment: nil
    24  .  .  .  .  .  EndPos: -
    25  .  .  .  .  }
    26  .  .  .  }
    27  .  .  .  Rparen: -
    28  .  .  }
    29  .  .  1: *ast.FuncDecl {
    30  .  .  .  Doc: nil
    31  .  .  .  Recv: nil
    32  .  .  .  Name: *ast.Ident {
    33  .  .  .  .  NamePos: foo:5:6
    34  .  .  .  .  Name: "main"
    35  .  .  .  .  Obj: *ast.Object {
    36  .  .  .  .  .  Kind: func
    37  .  .  .  .  .  Name: "main"
    38  .  .  .  .  .  Decl: *(obj @ 29)
    39  .  .  .  .  .  Data: nil
    40  .  .  .  .  .  Type: nil
    41  .  .  .  .  }
    42  .  .  .  }
    43  .  .  .  Type: *ast.FuncType {
    44  .  .  .  .  Func: foo:5:1
    45  .  .  .  .  Params: *ast.FieldList {
    46  .  .  .  .  .  Opening: foo:5:10
    47  .  .  .  .  .  List: nil
    48  .  .  .  .  .  Closing: foo:5:11
    49  .  .  .  .  }
    50  .  .  .  .  Results: nil
    51  .  .  .  }
    52  .  .  .  Body: *ast.BlockStmt {
    53  .  .  .  .  Lbrace: foo:5:13
    54  .  .  .  .  List: []ast.Stmt (len = 2) {
    55  .  .  .  .  .  0: *ast.DeferStmt {
    56  .  .  .  .  .  .  Defer: foo:6:2
    57  .  .  .  .  .  .  Call: *ast.CallExpr {
    58  .  .  .  .  .  .  .  Fun: *ast.SelectorExpr {
    59  .  .  .  .  .  .  .  .  X: *ast.Ident {
    60  .  .  .  .  .  .  .  .  .  NamePos: foo:6:8
    61  .  .  .  .  .  .  .  .  .  Name: "fmt"
    62  .  .  .  .  .  .  .  .  .  Obj: nil
    63  .  .  .  .  .  .  .  .  }
    64  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
    65  .  .  .  .  .  .  .  .  .  NamePos: foo:6:12
    66  .  .  .  .  .  .  .  .  .  Name: "Println"
    67  .  .  .  .  .  .  .  .  .  Obj: nil
    68  .  .  .  .  .  .  .  .  }
    69  .  .  .  .  .  .  .  }
    70  .  .  .  .  .  .  .  Lparen: foo:6:19
    71  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
    72  .  .  .  .  .  .  .  .  0: *ast.BasicLit {
    73  .  .  .  .  .  .  .  .  .  ValuePos: foo:6:20
    74  .  .  .  .  .  .  .  .  .  Kind: STRING
    75  .  .  .  .  .  .  .  .  .  Value: "\"goodbye\""
    76  .  .  .  .  .  .  .  .  }
    77  .  .  .  .  .  .  .  }
    78  .  .  .  .  .  .  .  Ellipsis: -
    79  .  .  .  .  .  .  .  Rparen: foo:6:29
    80  .  .  .  .  .  .  }
    81  .  .  .  .  .  }
    82  .  .  .  .  .  1: *ast.ExprStmt {
    83  .  .  .  .  .  .  X: *ast.CallExpr {
    84  .  .  .  .  .  .  .  Fun: *ast.SelectorExpr {
    85  .  .  .  .  .  .  .  .  X: *ast.Ident {
    86  .  .  .  .  .  .  .  .  .  NamePos: foo:8:2
    87  .  .  .  .  .  .  .  .  .  Name: "fmt"
    88  .  .  .  .  .  .  .  .  .  Obj: nil
    89  .  .  .  .  .  .  .  .  }
    90  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
    91  .  .  .  .  .  .  .  .  .  NamePos: foo:8:6
    92  .  .  .  .  .  .  .  .  .  Name: "Println"
    93  .  .  .  .  .  .  .  .  .  Obj: nil
    94  .  .  .  .  .  .  .  .  }
    95  .  .  .  .  .  .  .  }
    96  .  .  .  .  .  .  .  Lparen: foo:8:13
    97  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
    98  .  .  .  .  .  .  .  .  0: *ast.BasicLit {
    99  .  .  .  .  .  .  .  .  .  ValuePos: foo:8:14
   100  .  .  .  .  .  .  .  .  .  Kind: STRING
   101  .  .  .  .  .  .  .  .  .  Value: "\"hello\""
   102  .  .  .  .  .  .  .  .  }
   103  .  .  .  .  .  .  .  }
   104  .  .  .  .  .  .  .  Ellipsis: -
   105  .  .  .  .  .  .  .  Rparen: foo:8:21
   106  .  .  .  .  .  .  }
   107  .  .  .  .  .  }
   108  .  .  .  .  }
   109  .  .  .  .  Rbrace: foo:9:1
   110  .  .  .  }
   111  .  .  }
   112  .  }
   113  .  Scope: *ast.Scope {
   114  .  .  Outer: nil
   115  .  .  Objects: map[string]*ast.Object (len = 1) {
   116  .  .  .  "main": *(obj @ 35)
   117  .  .  }
   118  .  }
   119  .  Imports: []*ast.ImportSpec (len = 1) {
   120  .  .  0: *(obj @ 15)
   121  .  }
   122  .  Unresolved: []*ast.Ident (len = 2) {
   123  .  .  0: *(obj @ 59)
   124  .  .  1: *(obj @ 85)
   125  .  }
   126  .  Comments: nil
   127  }

##########################################################3

package main

import "fmt"

func main() {
	x := 10

	if x > 10 {
		fmt.Println("THEN")	
	}

	fmt.Println("hello")
}


     0  *ast.File {
     1  .  Doc: nil
     2  .  Package: foo:1:1
     3  .  Name: *ast.Ident {
     4  .  .  NamePos: foo:1:9
     5  .  .  Name: "main"
     6  .  .  Obj: nil
     7  .  }
     8  .  Decls: []ast.Decl (len = 2) {
     9  .  .  0: *ast.GenDecl {
    10  .  .  .  Doc: nil
    11  .  .  .  TokPos: foo:3:1
    12  .  .  .  Tok: import
    13  .  .  .  Lparen: -
    14  .  .  .  Specs: []ast.Spec (len = 1) {
    15  .  .  .  .  0: *ast.ImportSpec {
    16  .  .  .  .  .  Doc: nil
    17  .  .  .  .  .  Name: nil
    18  .  .  .  .  .  Path: *ast.BasicLit {
    19  .  .  .  .  .  .  ValuePos: foo:3:8
    20  .  .  .  .  .  .  Kind: STRING
    21  .  .  .  .  .  .  Value: "\"fmt\""
    22  .  .  .  .  .  }
    23  .  .  .  .  .  Comment: nil
    24  .  .  .  .  .  EndPos: -
    25  .  .  .  .  }
    26  .  .  .  }
    27  .  .  .  Rparen: -
    28  .  .  }
    29  .  .  1: *ast.FuncDecl {
    30  .  .  .  Doc: nil
    31  .  .  .  Recv: nil
    32  .  .  .  Name: *ast.Ident {
    33  .  .  .  .  NamePos: foo:5:6
    34  .  .  .  .  Name: "main"
    35  .  .  .  .  Obj: *ast.Object {
    36  .  .  .  .  .  Kind: func
    37  .  .  .  .  .  Name: "main"
    38  .  .  .  .  .  Decl: *(obj @ 29)
    39  .  .  .  .  .  Data: nil
    40  .  .  .  .  .  Type: nil
    41  .  .  .  .  }
    42  .  .  .  }
    43  .  .  .  Type: *ast.FuncType {
    44  .  .  .  .  Func: foo:5:1
    45  .  .  .  .  Params: *ast.FieldList {
    46  .  .  .  .  .  Opening: foo:5:10
    47  .  .  .  .  .  List: nil
    48  .  .  .  .  .  Closing: foo:5:11
    49  .  .  .  .  }
    50  .  .  .  .  Results: nil
    51  .  .  .  }
    52  .  .  .  Body: *ast.BlockStmt {
    53  .  .  .  .  Lbrace: foo:5:13
    54  .  .  .  .  List: []ast.Stmt (len = 3) {
    55  .  .  .  .  .  0: *ast.AssignStmt {
    56  .  .  .  .  .  .  Lhs: []ast.Expr (len = 1) {
    57  .  .  .  .  .  .  .  0: *ast.Ident {
    58  .  .  .  .  .  .  .  .  NamePos: foo:6:2
    59  .  .  .  .  .  .  .  .  Name: "x"
    60  .  .  .  .  .  .  .  .  Obj: *ast.Object {
    61  .  .  .  .  .  .  .  .  .  Kind: var
    62  .  .  .  .  .  .  .  .  .  Name: "x"
    63  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 55)
    64  .  .  .  .  .  .  .  .  .  Data: nil
    65  .  .  .  .  .  .  .  .  .  Type: nil
    66  .  .  .  .  .  .  .  .  }
    67  .  .  .  .  .  .  .  }
    68  .  .  .  .  .  .  }
    69  .  .  .  .  .  .  TokPos: foo:6:4
    70  .  .  .  .  .  .  Tok: :=
    71  .  .  .  .  .  .  Rhs: []ast.Expr (len = 1) {
    72  .  .  .  .  .  .  .  0: *ast.BasicLit {
    73  .  .  .  .  .  .  .  .  ValuePos: foo:6:7
    74  .  .  .  .  .  .  .  .  Kind: INT
    75  .  .  .  .  .  .  .  .  Value: "10"
    76  .  .  .  .  .  .  .  }
    77  .  .  .  .  .  .  }
    78  .  .  .  .  .  }
    79  .  .  .  .  .  1: *ast.IfStmt {
    80  .  .  .  .  .  .  If: foo:8:2
    81  .  .  .  .  .  .  Init: nil
    82  .  .  .  .  .  .  Cond: *ast.BinaryExpr {
    83  .  .  .  .  .  .  .  X: *ast.Ident {
    84  .  .  .  .  .  .  .  .  NamePos: foo:8:5
    85  .  .  .  .  .  .  .  .  Name: "x"
    86  .  .  .  .  .  .  .  .  Obj: *(obj @ 60)
    87  .  .  .  .  .  .  .  }
    88  .  .  .  .  .  .  .  OpPos: foo:8:7
    89  .  .  .  .  .  .  .  Op: >
    90  .  .  .  .  .  .  .  Y: *ast.BasicLit {
    91  .  .  .  .  .  .  .  .  ValuePos: foo:8:9
    92  .  .  .  .  .  .  .  .  Kind: INT
    93  .  .  .  .  .  .  .  .  Value: "10"
    94  .  .  .  .  .  .  .  }
    95  .  .  .  .  .  .  }
    96  .  .  .  .  .  .  Body: *ast.BlockStmt {
    97  .  .  .  .  .  .  .  Lbrace: foo:8:12
    98  .  .  .  .  .  .  .  List: []ast.Stmt (len = 1) {
    99  .  .  .  .  .  .  .  .  0: *ast.ExprStmt {
   100  .  .  .  .  .  .  .  .  .  X: *ast.CallExpr {
   101  .  .  .  .  .  .  .  .  .  .  Fun: *ast.SelectorExpr {
   102  .  .  .  .  .  .  .  .  .  .  .  X: *ast.Ident {
   103  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:9:3
   104  .  .  .  .  .  .  .  .  .  .  .  .  Name: "fmt"
   105  .  .  .  .  .  .  .  .  .  .  .  .  Obj: nil
   106  .  .  .  .  .  .  .  .  .  .  .  }
   107  .  .  .  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
   108  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:9:7
   109  .  .  .  .  .  .  .  .  .  .  .  .  Name: "Println"
   110  .  .  .  .  .  .  .  .  .  .  .  .  Obj: nil
   111  .  .  .  .  .  .  .  .  .  .  .  }
   112  .  .  .  .  .  .  .  .  .  .  }
   113  .  .  .  .  .  .  .  .  .  .  Lparen: foo:9:14
   114  .  .  .  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
   115  .  .  .  .  .  .  .  .  .  .  .  0: *ast.BasicLit {
   116  .  .  .  .  .  .  .  .  .  .  .  .  ValuePos: foo:9:15
   117  .  .  .  .  .  .  .  .  .  .  .  .  Kind: STRING
   118  .  .  .  .  .  .  .  .  .  .  .  .  Value: "\"THEN\""
   119  .  .  .  .  .  .  .  .  .  .  .  }
   120  .  .  .  .  .  .  .  .  .  .  }
   121  .  .  .  .  .  .  .  .  .  .  Ellipsis: -
   122  .  .  .  .  .  .  .  .  .  .  Rparen: foo:9:21
   123  .  .  .  .  .  .  .  .  .  }
   124  .  .  .  .  .  .  .  .  }
   125  .  .  .  .  .  .  .  }
   126  .  .  .  .  .  .  .  Rbrace: foo:10:2
   127  .  .  .  .  .  .  }
   128  .  .  .  .  .  .  Else: nil
   129  .  .  .  .  .  }
   130  .  .  .  .  .  2: *ast.ExprStmt {
   131  .  .  .  .  .  .  X: *ast.CallExpr {
   132  .  .  .  .  .  .  .  Fun: *ast.SelectorExpr {
   133  .  .  .  .  .  .  .  .  X: *ast.Ident {
   134  .  .  .  .  .  .  .  .  .  NamePos: foo:12:2
   135  .  .  .  .  .  .  .  .  .  Name: "fmt"
   136  .  .  .  .  .  .  .  .  .  Obj: nil
   137  .  .  .  .  .  .  .  .  }
   138  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
   139  .  .  .  .  .  .  .  .  .  NamePos: foo:12:6
   140  .  .  .  .  .  .  .  .  .  Name: "Println"
   141  .  .  .  .  .  .  .  .  .  Obj: nil
   142  .  .  .  .  .  .  .  .  }
   143  .  .  .  .  .  .  .  }
   144  .  .  .  .  .  .  .  Lparen: foo:12:13
   145  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
   146  .  .  .  .  .  .  .  .  0: *ast.BasicLit {
   147  .  .  .  .  .  .  .  .  .  ValuePos: foo:12:14
   148  .  .  .  .  .  .  .  .  .  Kind: STRING
   149  .  .  .  .  .  .  .  .  .  Value: "\"hello\""
   150  .  .  .  .  .  .  .  .  }
   151  .  .  .  .  .  .  .  }
   152  .  .  .  .  .  .  .  Ellipsis: -
   153  .  .  .  .  .  .  .  Rparen: foo:12:21
   154  .  .  .  .  .  .  }
   155  .  .  .  .  .  }
   156  .  .  .  .  }
   157  .  .  .  .  Rbrace: foo:13:1
   158  .  .  .  }
   159  .  .  }
   160  .  }
   161  .  Scope: *ast.Scope {
   162  .  .  Outer: nil
   163  .  .  Objects: map[string]*ast.Object (len = 1) {
   164  .  .  .  "main": *(obj @ 35)
   165  .  .  }
   166  .  }
   167  .  Imports: []*ast.ImportSpec (len = 1) {
   168  .  .  0: *(obj @ 15)
   169  .  }
   170  .  Unresolved: []*ast.Ident (len = 2) {
   171  .  .  0: *(obj @ 102)
   172  .  .  1: *(obj @ 133)
   173  .  }
   174  .  Comments: nil
   175  }

///////////////////////////////////////////////////
package main

import (
	"fmt"
)

type B struct {
    K3 string
}

var b B

func main() {
    fmt.Println(b)
}



Dump
     0  *ast.File {
     1  .  Doc: nil
     2  .  Package: foo:1:1
     3  .  Name: *ast.Ident {
     4  .  .  NamePos: foo:1:9
     5  .  .  Name: "main"
     6  .  .  Obj: nil
     7  .  }
     8  .  Decls: []ast.Decl (len = 4) {
     9  .  .  0: *ast.GenDecl {
    10  .  .  .  Doc: nil
    11  .  .  .  TokPos: foo:3:1
    12  .  .  .  Tok: import
    13  .  .  .  Lparen: foo:3:8
    14  .  .  .  Specs: []ast.Spec (len = 1) {
    15  .  .  .  .  0: *ast.ImportSpec {
    16  .  .  .  .  .  Doc: nil
    17  .  .  .  .  .  Name: nil
    18  .  .  .  .  .  Path: *ast.BasicLit {
    19  .  .  .  .  .  .  ValuePos: foo:4:2
    20  .  .  .  .  .  .  Kind: STRING
    21  .  .  .  .  .  .  Value: "\"fmt\""
    22  .  .  .  .  .  }
    23  .  .  .  .  .  Comment: nil
    24  .  .  .  .  .  EndPos: -
    25  .  .  .  .  }
    26  .  .  .  }
    27  .  .  .  Rparen: foo:5:1
    28  .  .  }
    29  .  .  1: *ast.GenDecl {
    30  .  .  .  Doc: nil
    31  .  .  .  TokPos: foo:7:1
    32  .  .  .  Tok: type
    33  .  .  .  Lparen: -
    34  .  .  .  Specs: []ast.Spec (len = 1) {
    35  .  .  .  .  0: *ast.TypeSpec {
    36  .  .  .  .  .  Doc: nil
    37  .  .  .  .  .  Name: *ast.Ident {
    38  .  .  .  .  .  .  NamePos: foo:7:6
    39  .  .  .  .  .  .  Name: "B"
    40  .  .  .  .  .  .  Obj: *ast.Object {
    41  .  .  .  .  .  .  .  Kind: type
    42  .  .  .  .  .  .  .  Name: "B"
    43  .  .  .  .  .  .  .  Decl: *(obj @ 35)
    44  .  .  .  .  .  .  .  Data: nil
    45  .  .  .  .  .  .  .  Type: nil
    46  .  .  .  .  .  .  }
    47  .  .  .  .  .  }
    48  .  .  .  .  .  Assign: -
    49  .  .  .  .  .  Type: *ast.StructType {
    50  .  .  .  .  .  .  Struct: foo:7:8
    51  .  .  .  .  .  .  Fields: *ast.FieldList {
    52  .  .  .  .  .  .  .  Opening: foo:7:15
    53  .  .  .  .  .  .  .  List: []*ast.Field (len = 1) {
    54  .  .  .  .  .  .  .  .  0: *ast.Field {
    55  .  .  .  .  .  .  .  .  .  Doc: nil
    56  .  .  .  .  .  .  .  .  .  Names: []*ast.Ident (len = 1) {
    57  .  .  .  .  .  .  .  .  .  .  0: *ast.Ident {
    58  .  .  .  .  .  .  .  .  .  .  .  NamePos: foo:8:5
    59  .  .  .  .  .  .  .  .  .  .  .  Name: "K3"
    60  .  .  .  .  .  .  .  .  .  .  .  Obj: *ast.Object {
    61  .  .  .  .  .  .  .  .  .  .  .  .  Kind: var
    62  .  .  .  .  .  .  .  .  .  .  .  .  Name: "K3"
    63  .  .  .  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 54)
    64  .  .  .  .  .  .  .  .  .  .  .  .  Data: nil
    65  .  .  .  .  .  .  .  .  .  .  .  .  Type: nil
    66  .  .  .  .  .  .  .  .  .  .  .  }
    67  .  .  .  .  .  .  .  .  .  .  }
    68  .  .  .  .  .  .  .  .  .  }
    69  .  .  .  .  .  .  .  .  .  Type: *ast.Ident {
    70  .  .  .  .  .  .  .  .  .  .  NamePos: foo:8:8
    71  .  .  .  .  .  .  .  .  .  .  Name: "string"
    72  .  .  .  .  .  .  .  .  .  .  Obj: nil
    73  .  .  .  .  .  .  .  .  .  }
    74  .  .  .  .  .  .  .  .  .  Tag: nil
    75  .  .  .  .  .  .  .  .  .  Comment: nil
    76  .  .  .  .  .  .  .  .  }
    77  .  .  .  .  .  .  .  }
    78  .  .  .  .  .  .  .  Closing: foo:9:1
    79  .  .  .  .  .  .  }
    80  .  .  .  .  .  .  Incomplete: false
    81  .  .  .  .  .  }
    82  .  .  .  .  .  Comment: nil
    83  .  .  .  .  }
    84  .  .  .  }
    85  .  .  .  Rparen: -
    86  .  .  }
    87  .  .  2: *ast.GenDecl {
    88  .  .  .  Doc: nil
    89  .  .  .  TokPos: foo:11:1
    90  .  .  .  Tok: var
    91  .  .  .  Lparen: -
    92  .  .  .  Specs: []ast.Spec (len = 1) {
    93  .  .  .  .  0: *ast.ValueSpec {
    94  .  .  .  .  .  Doc: nil
    95  .  .  .  .  .  Names: []*ast.Ident (len = 1) {
    96  .  .  .  .  .  .  0: *ast.Ident {
    97  .  .  .  .  .  .  .  NamePos: foo:11:5
    98  .  .  .  .  .  .  .  Name: "b"
    99  .  .  .  .  .  .  .  Obj: *ast.Object {
   100  .  .  .  .  .  .  .  .  Kind: var
   101  .  .  .  .  .  .  .  .  Name: "b"
   102  .  .  .  .  .  .  .  .  Decl: *(obj @ 93)
   103  .  .  .  .  .  .  .  .  Data: 0
   104  .  .  .  .  .  .  .  .  Type: nil
   105  .  .  .  .  .  .  .  }
   106  .  .  .  .  .  .  }
   107  .  .  .  .  .  }
   108  .  .  .  .  .  Type: *ast.Ident {
   109  .  .  .  .  .  .  NamePos: foo:11:7
   110  .  .  .  .  .  .  Name: "B"
   111  .  .  .  .  .  .  Obj: *(obj @ 40)
   112  .  .  .  .  .  }
   113  .  .  .  .  .  Values: nil
   114  .  .  .  .  .  Comment: nil
   115  .  .  .  .  }
   116  .  .  .  }
   117  .  .  .  Rparen: -
   118  .  .  }
   119  .  .  3: *ast.FuncDecl {
   120  .  .  .  Doc: nil
   121  .  .  .  Recv: nil
   122  .  .  .  Name: *ast.Ident {
   123  .  .  .  .  NamePos: foo:13:6
   124  .  .  .  .  Name: "main"
   125  .  .  .  .  Obj: *ast.Object {
   126  .  .  .  .  .  Kind: func
   127  .  .  .  .  .  Name: "main"
   128  .  .  .  .  .  Decl: *(obj @ 119)
   129  .  .  .  .  .  Data: nil
   130  .  .  .  .  .  Type: nil
   131  .  .  .  .  }
   132  .  .  .  }
   133  .  .  .  Type: *ast.FuncType {
   134  .  .  .  .  Func: foo:13:1
   135  .  .  .  .  Params: *ast.FieldList {
   136  .  .  .  .  .  Opening: foo:13:10
   137  .  .  .  .  .  List: nil
   138  .  .  .  .  .  Closing: foo:13:11
   139  .  .  .  .  }
   140  .  .  .  .  Results: nil
   141  .  .  .  }
   142  .  .  .  Body: *ast.BlockStmt {
   143  .  .  .  .  Lbrace: foo:13:13
   144  .  .  .  .  List: []ast.Stmt (len = 1) {
   145  .  .  .  .  .  0: *ast.ExprStmt {
   146  .  .  .  .  .  .  X: *ast.CallExpr {
   147  .  .  .  .  .  .  .  Fun: *ast.SelectorExpr {
   148  .  .  .  .  .  .  .  .  X: *ast.Ident {
   149  .  .  .  .  .  .  .  .  .  NamePos: foo:14:5
   150  .  .  .  .  .  .  .  .  .  Name: "fmt"
   151  .  .  .  .  .  .  .  .  .  Obj: nil
   152  .  .  .  .  .  .  .  .  }
   153  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
   154  .  .  .  .  .  .  .  .  .  NamePos: foo:14:9
   155  .  .  .  .  .  .  .  .  .  Name: "Println"
   156  .  .  .  .  .  .  .  .  .  Obj: nil
   157  .  .  .  .  .  .  .  .  }
   158  .  .  .  .  .  .  .  }
   159  .  .  .  .  .  .  .  Lparen: foo:14:16
   160  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
   161  .  .  .  .  .  .  .  .  0: *ast.Ident {
   162  .  .  .  .  .  .  .  .  .  NamePos: foo:14:17
   163  .  .  .  .  .  .  .  .  .  Name: "b"
   164  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 99)
   165  .  .  .  .  .  .  .  .  }
   166  .  .  .  .  .  .  .  }
   167  .  .  .  .  .  .  .  Ellipsis: -
   168  .  .  .  .  .  .  .  Rparen: foo:14:18
   169  .  .  .  .  .  .  }
   170  .  .  .  .  .  }
   171  .  .  .  .  }
   172  .  .  .  .  Rbrace: foo:15:1
   173  .  .  .  }
   174  .  .  }
   175  .  }
   176  .  Scope: *ast.Scope {
   177  .  .  Outer: nil
   178  .  .  Objects: map[string]*ast.Object (len = 3) {
   179  .  .  .  "main": *(obj @ 125)
   180  .  .  .  "B": *(obj @ 40)
   181  .  .  .  "b": *(obj @ 99)
   182  .  .  }
   183  .  }
   184  .  Imports: []*ast.ImportSpec (len = 1) {
   185  .  .  0: *(obj @ 15)
   186  .  }
   187  .  Unresolved: []*ast.Ident (len = 2) {
   188  .  .  0: *(obj @ 69)
   189  .  .  1: *(obj @ 148)
   190  .  }
   191  .  Comments: nil
   192  }


///////////////////////////////////////////////////


package main

import (
	"fmt"
)

var b string

func main() {
    fmt.Println(b)
}


*ast.File {
     1  .  Doc: nil
     2  .  Package: foo:1:1
     3  .  Name: *ast.Ident {
     4  .  .  NamePos: foo:1:9
     5  .  .  Name: "main"
     6  .  .  Obj: nil
     7  .  }
     8  .  Decls: []ast.Decl (len = 3) {
     9  .  .  0: *ast.GenDecl {
    10  .  .  .  Doc: nil
    11  .  .  .  TokPos: foo:3:1
    12  .  .  .  Tok: import
    13  .  .  .  Lparen: foo:3:8
    14  .  .  .  Specs: []ast.Spec (len = 1) {
    15  .  .  .  .  0: *ast.ImportSpec {
    16  .  .  .  .  .  Doc: nil
    17  .  .  .  .  .  Name: nil
    18  .  .  .  .  .  Path: *ast.BasicLit {
    19  .  .  .  .  .  .  ValuePos: foo:4:2
    20  .  .  .  .  .  .  Kind: STRING
    21  .  .  .  .  .  .  Value: "\"fmt\""
    22  .  .  .  .  .  }
    23  .  .  .  .  .  Comment: nil
    24  .  .  .  .  .  EndPos: -
    25  .  .  .  .  }
    26  .  .  .  }
    27  .  .  .  Rparen: foo:5:1
    28  .  .  }
    29  .  .  1: *ast.GenDecl {
    30  .  .  .  Doc: nil
    31  .  .  .  TokPos: foo:7:1
    32  .  .  .  Tok: var
    33  .  .  .  Lparen: -
    34  .  .  .  Specs: []ast.Spec (len = 1) {
    35  .  .  .  .  0: *ast.ValueSpec {
    36  .  .  .  .  .  Doc: nil
    37  .  .  .  .  .  Names: []*ast.Ident (len = 1) {
    38  .  .  .  .  .  .  0: *ast.Ident {
    39  .  .  .  .  .  .  .  NamePos: foo:7:5
    40  .  .  .  .  .  .  .  Name: "b"
    41  .  .  .  .  .  .  .  Obj: *ast.Object {
    42  .  .  .  .  .  .  .  .  Kind: var
    43  .  .  .  .  .  .  .  .  Name: "b"
    44  .  .  .  .  .  .  .  .  Decl: *(obj @ 35)
    45  .  .  .  .  .  .  .  .  Data: 0
    46  .  .  .  .  .  .  .  .  Type: nil
    47  .  .  .  .  .  .  .  }
    48  .  .  .  .  .  .  }
    49  .  .  .  .  .  }
    50  .  .  .  .  .  Type: *ast.Ident {
    51  .  .  .  .  .  .  NamePos: foo:7:7
    52  .  .  .  .  .  .  Name: "string"
    53  .  .  .  .  .  .  Obj: nil
    54  .  .  .  .  .  }
    55  .  .  .  .  .  Values: nil
    56  .  .  .  .  .  Comment: nil
    57  .  .  .  .  }
    58  .  .  .  }
    59  .  .  .  Rparen: -
    60  .  .  }
    61  .  .  2: *ast.FuncDecl {
    62  .  .  .  Doc: nil
    63  .  .  .  Recv: nil
    64  .  .  .  Name: *ast.Ident {
    65  .  .  .  .  NamePos: foo:9:6
    66  .  .  .  .  Name: "main"
    67  .  .  .  .  Obj: *ast.Object {
    68  .  .  .  .  .  Kind: func
    69  .  .  .  .  .  Name: "main"
    70  .  .  .  .  .  Decl: *(obj @ 61)
    71  .  .  .  .  .  Data: nil
    72  .  .  .  .  .  Type: nil
    73  .  .  .  .  }
    74  .  .  .  }
    75  .  .  .  Type: *ast.FuncType {
    76  .  .  .  .  Func: foo:9:1
    77  .  .  .  .  Params: *ast.FieldList {
    78  .  .  .  .  .  Opening: foo:9:10
    79  .  .  .  .  .  List: nil
    80  .  .  .  .  .  Closing: foo:9:11
    81  .  .  .  .  }
    82  .  .  .  .  Results: nil
    83  .  .  .  }
    84  .  .  .  Body: *ast.BlockStmt {
    85  .  .  .  .  Lbrace: foo:9:13
    86  .  .  .  .  List: []ast.Stmt (len = 1) {
    87  .  .  .  .  .  0: *ast.ExprStmt {
    88  .  .  .  .  .  .  X: *ast.CallExpr {
    89  .  .  .  .  .  .  .  Fun: *ast.SelectorExpr {
    90  .  .  .  .  .  .  .  .  X: *ast.Ident {
    91  .  .  .  .  .  .  .  .  .  NamePos: foo:10:5
    92  .  .  .  .  .  .  .  .  .  Name: "fmt"
    93  .  .  .  .  .  .  .  .  .  Obj: nil
    94  .  .  .  .  .  .  .  .  }
    95  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
    96  .  .  .  .  .  .  .  .  .  NamePos: foo:10:9
    97  .  .  .  .  .  .  .  .  .  Name: "Println"
    98  .  .  .  .  .  .  .  .  .  Obj: nil
    99  .  .  .  .  .  .  .  .  }
   100  .  .  .  .  .  .  .  }
   101  .  .  .  .  .  .  .  Lparen: foo:10:16
   102  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
   103  .  .  .  .  .  .  .  .  0: *ast.Ident {
   104  .  .  .  .  .  .  .  .  .  NamePos: foo:10:17
   105  .  .  .  .  .  .  .  .  .  Name: "b"
   106  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 41)
   107  .  .  .  .  .  .  .  .  }
   108  .  .  .  .  .  .  .  }
   109  .  .  .  .  .  .  .  Ellipsis: -
   110  .  .  .  .  .  .  .  Rparen: foo:10:18
   111  .  .  .  .  .  .  }
   112  .  .  .  .  .  }
   113  .  .  .  .  }
   114  .  .  .  .  Rbrace: foo:11:1
   115  .  .  .  }
   116  .  .  }
   117  .  }
   118  .  Scope: *ast.Scope {
   119  .  .  Outer: nil
   120  .  .  Objects: map[string]*ast.Object (len = 2) {
   121  .  .  .  "b": *(obj @ 41)
   122  .  .  .  "main": *(obj @ 67)
   123  .  .  }
   124  .  }
   125  .  Imports: []*ast.ImportSpec (len = 1) {
   126  .  .  0: *(obj @ 15)
   127  .  }
   128  .  Unresolved: []*ast.Ident (len = 2) {
   129  .  .  0: *(obj @ 50)
   130  .  .  1: *(obj @ 90)
   131  .  }
   132  .  Comments: nil
   133  }
 

*/
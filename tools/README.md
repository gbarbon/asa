DAPA:
=====
Degradation-Aware Privacy Analysis of Android Apps
--------------------------------------------------

This tool performs the analysis of translated Android apps as described in *DAPA: Degradation-Aware Privacy Analysis of Android Apps*.


###Run the analysis

To perform the analysis run `java -jar dapa.jar input-file`

For all possible options, see `java -jar dapa.jar -h`


###License

This tool is released under the MIT license; see [License](LICENSE.txt) for more details.


###Language

id :=  \[A-Z_a-z\]\[A-Z_a-z0-9\]*

Program := Class*

Class := `class` id [`extends` id] `{` FieldDeclaration* MethodDeclaration* `}`

FieldDeclaration := `static` Type id (`,` id)* `;`

Type := `int`
      | `boolean`
      | `string`
      | Type `[]`

MethodDeclaration := `static` (`void` | Type) id `(`[Formals]`)` Block

Formals := Type id (, Type id)*

Block := `{` VarDeclaration* Statement* `}`

VarDeclaration := Type id (`,` id)* `;`

Statement := `skip` `;`
           | Loc `=` Expr `;`
           | `print` `(` Expr `)` `;`
           | `println` `(` Expr `)` `;`
           | `log` `(` Expr `)` `;`
           | id `(` [Actuals] `)` `;`
           | `return` Expr `;`
           | `if` `(` Expr `)` Statement (`elif` `(` Expr `)` Statement) [`else` Statement]
           | `while` `(` Expr `)` Statement
           | Block

Loc := id (`[` Expr `]`)*

Actuals := Expr (`,` Expr)*

Expr := Loc
      | `new` Type `[` integer `]`
      | `len` `(` Expr `)`
      | `toCharArray` `(` Expr `)`
      | id `(` [Actuals] `)`
      | Expr Bop Expr
      | Unop Expr
      | Litteral
      | `(` Expr `)`

Litteral := `"` string `"`
          | `true`
          | `false`
          | integer

Bop := `++`
     | `+`
     | `-`
     | `*`
     | `/`
     | `%`
     | `&&`
     | `||`
     | `==`
     | `!=`
     | `<=`
     | `<`
     | `>=`
     | `>`

Unop := `!`
      | `-`

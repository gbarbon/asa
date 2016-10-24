DAPA: Degradation-Aware Privacy Analysis of Android Apps
========================================================
This tool performs the analysis of Android applications (translated in our JAVA-like language) as described in *DAPA: Degradation-Aware Privacy Analysis of Android Apps* available [here](http://link.springer.com/chapter/10.1007/978-3-319-46598-2_3).

This tool is written in SCALA.

Compilation
-----------

This tool is written in SCALA using [sbt](http://www.scala-sbt.org/).
Before compilation make sure you have [sbt](http://www.scala-sbt.org/) and [sbt assembly](https://github.com/sbt/sbt-assembly) plugin installed.

To compile the tool just run `sbt assembly` from the tool root directory.

The standalone `dapa-assembly-1.0.0.jar` file is produced in folder `target/scala-"YourScalaVersion"/`.

Usage
-----

To perform the analysis run `java -jar dapa.jar input-file`

For all possible options, see `java -jar dapa.jar -h`


License
-------

See [License](LICENSE.txt)

Language
--------
The tool language specification.

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

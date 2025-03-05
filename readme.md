### compiler construction

```
+----------------------++-----------------------------++-------------------------++
|    high-level lang   >>     high-level lang AST     >>         IR AST (--> IR) >>
+----------------------++-----------------------------++-------------------------++
|             \                   /         \                     /               |
|              \                 /           \                   /                |
|                    parsing                       frontend                       |
+------------------------------------- ++--------------------------++--------------

+------------------++-------------------++----------------------++----------------+
|      IR AST      >>      assembly     >>     machine code     >>     output     |
|        \                   /  \                 /    \                 /        |
|         \                 /    \               /      \               /         |
|               backend              assembler              execution             |
+-----------------------------++--------------------++------------------++---------

```
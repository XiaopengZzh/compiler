### compiler construction

```
+------------------++-----------------------++---------------++------------------++
|  high-level lang >>  high-level lang AST  >>      IR       >>      IR AST      >>
+------------------++-----------------------++---------------++------------------++
|         \                   /    \               /  \               /           |
|          \                 /      \             /    \             /            |
|               parsing                 frontend           parsing                |
+-------------------------------++------------------++------------------++---------

+------------------++-------------------++----------------------++----------------+
|      IR AST      >>      assembly     >>     machine code     >>     output     |
|        \                   /  \                 /    \                 /        |
|         \                 /    \               /      \               /         |
|               backend              assembler              execution             |
+-----------------------------++--------------------++------------------++---------

```
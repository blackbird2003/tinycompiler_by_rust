要求:
编译器同样是重要的系统软件，它能够将我们编写的⾼级源代码转换成贴近底层的⽬标代码，本选课即为利⽤ Rust 实现⼀个简单语⾔的编译器。
1. 你可以⾃定义实现的语⾔，但它⾄少应该具有基本的数据类型、变量声明、函数声明、循环、条件判断等基本语法。
2. 实现的编译器⾄少应该具有语法分析和代码⽣成这两部分。
3. 在代码⽣成中，你可以选择基于 LLVM 的技术将源代码编译成⽬标⽂件，或者类似于 Java,Python 等语⾔的字节码并实现⼀个虚拟机来执⾏它。
参考资料：
Compiler Book
Lox implementation in RustStanford Compiler Course

目标:实现一个Wend语言(简化版C语言)的编译器,包含lexer/parser/analyzer/assembly generate四个主要的组件以及相关的数据结构(语法树、符号表)。

参考资料:

- Tiny Compiler ssloy.github.io/tinycompiler/ 一个用Python语言实现Wend编译器的教程
- How to Develop a Compiler (青木峰郎 著) 用Java实现Cb(也是一个简化版C语言)编译器的教程



实现顺序

顺序	文件	行数	依赖	难度

1️⃣	lexer.rs	~100	无	⭐⭐ 中等

2️⃣	syntree.rs	~100	无	⭐ 简单

3️⃣	parser.rs	~200	lexer, syntree	⭐⭐⭐ 困难

4️⃣	symtable.rs	~100	syntree	⭐⭐ 中等

5️⃣	analyzer.rs	~150	syntree, symtable	⭐⭐⭐ 困难

6️⃣	transasm_recipe.rs	~100	无	⭐ 简单

7️⃣	transasm.rs	~150	syntree, transasm_recipe	⭐⭐⭐ 困难

8️⃣	main.rs	~50	所有	⭐ 简单

![overview](images/overview.png)



# 命名约定
蛇形命名法（snake_case）：用于变量、函数、方法、模块、文件

帕斯卡命名法（PascalCase）：用于类型、特征、枚举、泛型参数

全大写蛇形命名法（SCREAMING_SNAKE_CASE）：用于常量、静态变量

# Lexer
Lexer本质上是一个状态机，把输入的字符串转化成一系列Tokens。

Token的定义是(token_type, value)。在编译器中，增加行号用于代码生成。

```rust
pub struct Token {
    pub token_type: String,
    pub value: String,
    pub lineno: usize,
}
```

按照下面这张图去实现即可
![lexer](images/lexer.png)

# Syntree
我们要定义一个语法树。

例如,对于下面这个实现开根号的函数(这不是我们要实现的最终语言)
```c
fun main() {
    // square root of a fixed-point number
    // stored in a 32 bit integer variable, shift is the precision

    fun sqrt(n:int, shift:int) : int {
        var x:int;
        var x_old:int;
        var n_one:int;

        if n > 2147483647/shift { // pay attention to potential overflows
            return 2 * sqrt(n / 4, shift);
        }
        x = shift; // initial guess 1.0, can do better, but oh well
        n_one = n * shift; // need to compensate for fixp division
        while true {
            x_old = x;
            x = (x + n_one / x) / 2;
            if abs(x - x_old) <= 1 {
                return x;
            }
        }
    }

    fun abs(x:int) : int {
        if x < 0 {
            return -x;
        } else {
            return x;
        }
    }

    // 25735 is approximately equal to pi * 8192;
    // expected value of the output is sqrt(pi) * 8192 approx 14519

    println sqrt(25735, 8192);
}
```
它的语法树应该是

![syntree](images/syntree.png)

语法树是对程序的一种抽象，这种抽象与语言无关，因此我们通过解析一种语言得到语法树后，可以通过遍历语法树生成另一种语言的代码，包括汇编代码。


# Parser
Parser的作用是

- 判断一系列Token组成的语句是否符合语法规则
- 如果符合，则构建出相应的语法树

这里，我们使用一种实现简单、功能强大(支持任何上下文无关文法，包括歧义文法)的Earley Parser。


Earley 解析器以输入位置为阶段进行工作。设输入词法单元序列为 $t_0 t_1 \ldots t_{n-1}$。对每个输入位置 $j \in [0 \ldots n]$，算法维护一个集合 $J_j$，其中的元素称为 Earley。 在任意 $J_j$ 中，相同的 Earley 项至多出现一次。

每个 Earley 项的形式为 $(A \to \alpha \bullet \beta,\ k)$。其中 $A \to \alpha\beta$ 是一条语法产生式，点 $\bullet$ 表示该产生式右侧中已经匹配完成的位置；$k$ 是该产生式开始匹配时的输入位置，即该非终结符 $A$ 被预测或引入时已处理的词法单元数量。


算法从集合 $J_0 = \{(S' \to \bullet S,\ 0)\}$ 开始，其中 $S$ 是语法的起始符号，$S'$ 是引入的人工起始符号。随后，算法按输入位置 $j = 0, 1, \ldots, n$ 依次构造各个集合 $J_j$。。


算法构造每个集合的流程如下:

- 遍历$J_j$的所有 Earley 项(遍历过程中,集合大小可能增长)

    - 若形如 $(A \to \alpha \bullet B\beta,\ k)$，其中 $B$ 是非终结符，则对每一条产生式 $B \to \gamma$，将项 $(B \to \bullet \gamma,\ j)$ 加入 $J_j$。该步骤称为预测（predict）。预测步骤可能会在$J_j$中添加新的Earley项，但应保证$J_j$作为集合的互异性(不会陷入无限循环)。

    - 若形如 $(A \to \alpha \bullet t\beta,\ k)$，其中 $t$ 是终结符，且 $t = t_j$，则将项 $(A \to \alpha t \bullet \beta,\ k)$ 加入 $J_{j+1}$。该步骤称为扫描（scan）。

    - 若为完成项 $(B \to \gamma \bullet,\ k)$，则对集合 $J_k$ 中的每一个项 $(A \to \alpha \bullet B\beta,\ l)$，将项 $(A \to \alpha B \bullet \beta,\ l)$ 加入 $J_j$。该步骤称为完成（complete）。



如果 $ (S' \to S \bullet, 0) $ 存在于集合 $ J_n $ 中，则算法成功解析了 $ n $ 个输入词法单元的序列 $ t_0t_1 \ldots t_{n-1} $，否则报告错误。

伪代码
```
DECLARE ARRAY S;

function INIT(words)
    S ← CREATE_ARRAY(LENGTH(words) + 1)
    for k ← from 0 to LENGTH(words) do
        S[k] ← EMPTY_ORDERED_SET

function EARLEY_PARSE(words, grammar)
    INIT(words)
    ADD_TO_SET((γ → •S, 0), S[0])
    for k ← from 0 to LENGTH(words) do
        for each state in S[k] do  // S[k] can expand during this loop
            if not FINISHED(state) then
                if NEXT_ELEMENT_OF(state) is a nonterminal then
                    PREDICTOR(state, k, grammar)         // non_terminal
                else do
                    SCANNER(state, k, words)             // terminal
            else do
                COMPLETER(state, k)
        end
    end
    return chart

procedure PREDICTOR((A → α•Bβ, j), k, grammar)
    for each (B → γ) in GRAMMAR_RULES_FOR(B, grammar) do
        ADD_TO_SET((B → •γ, k), S[k])
    end

procedure SCANNER((A → α•aβ, j), k, words)
    if j < LENGTH(words) and a ⊂ PARTS_OF_SPEECH(words[k]) then
        ADD_TO_SET((A → αa•β, j), S[k+1])
    end

procedure COMPLETER((B → γ•, x), k)
    for each (A → α•Bβ, j) in S[x] do
        ADD_TO_SET((A → αB•β, j), S[k])
    end
```

# 符号表+语义分析器

## 符号表设计
python版本tinycompiler中的符号表设计
```python
class SymbolTable():
    def __init__(self):
        self.variables = [{}]     # stack of variable symbol tables
        self.functions = [{}]     # stack of function symbol tables
        self.ret_stack = [ None ] # stack of enclosing function symbols, useful for return statements
        self.scope_cnt = 0        # global scope counter for the display table allocation

    def add_fun(self, name, argtypes, deco):  # a function can be identified by its name and a list of argument types, e.g.
        signature = (name, *argtypes)         # fun foo(x:bool, y:int) : int {...} has ('foo',Type.BOOL,Type.INT) signature
        if signature in self.functions[-1]:
            raise Exception('Double declaration of the function %s %s' % (signature[0], signature[1:]))
        self.functions[-1][signature] = deco
        deco['scope'] = self.scope_cnt # id for the function block in the scope display table
        self.scope_cnt += 1

    def add_var(self, name, deco):
        if name in self.variables[-1]:
            raise Exception('Double declaration of the variable %s' % name)
        self.variables[-1][name] = deco
        deco['scope']  = self.ret_stack[-1]['scope']   # pointer to the display entry
        deco['offset'] = self.ret_stack[-1]['var_cnt'] # id of the variable in the corresponding stack frame
        self.ret_stack[-1]['var_cnt'] += 1

    def push_scope(self, deco):
        self.variables.append({})
        self.functions.append({})
        self.ret_stack.append(deco)
        deco['var_cnt'] = 0 # reset the per scope variable counter

    def pop_scope(self):
        self.variables.pop()
        self.functions.pop()
        self.ret_stack.pop()

    def find_var(self, name):
        for i in reversed(range(len(self.variables))):
            if name in self.variables[i]:
                return self.variables[i][name]
        raise Exception('No declaration for the variable %s' % name)

    def find_fun(self, name, argtypes):
        signature = (name, *argtypes)
        for i in reversed(range(len(self.functions))):
            if signature in self.functions[i]:
                return self.functions[i][signature]
        raise Exception('No declaration for the function %s' % signature[0], signature[1:])
```
SymbolTable实际上是一个符号表的栈，每个作用域有各自的符号表，包括变量的符号表和函数的符号表。

我们实现的语言约定下面的语法：
fun →
    fun_type ID '(' param_list ')' '{'
        var_list
        fun_list
        statement_list
    '}'
即，在一个函数中，必须先声明所有的局部变量，然后声明所有的嵌套函数，最后才能开始写具体的语句。这会为parser个analyzer带来极大的便利。
我们为每一个scope(作用域)分配了id。这些声明的变量与函数参数共同构成了属于这个作用域的变量，它们都有各自的变量id，如下图所示。

![symtable](images/symtable.png)

这样，我们只需要一个scope_id加一个var_id即可确定一个变量，也可以通过遍历符号表栈获取外层作用域的变量。

在进入、退出作用域时，通过栈操作对符号表进行维护即可。



## 语义分析器

语义分析器借助上述符号表，对AST进行检查，确认AST中节点变量/函数的合法性，确保没有变量和函数的未定义、重复定义等问题。如果没有问题，则能够认为代码已经“通过检查”，语义分析器同时会对AST节点添加修饰信息,便于下一步生成汇编代码。


# 汇编生成

## 中间表示：三地址码





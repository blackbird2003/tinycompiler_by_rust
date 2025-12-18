目标:实现一个Wend语言(简化版C语言)的编译器,包含lexer/parser/analyzer/assembly generate四个主要的组件以及相关的数据结构。

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

Token的定义是(token_type, value)

```rust
struct Token {
    token_type: String,
    value: String,
}
```

按照下面这张图去实现即可
![lexer](images/lexer.png)

# Syntree
我们要定义一个语法树。

例如,对于下面这个实现开根号的函数
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

每个 Task 项的形式为 $(A \to \alpha \bullet \beta,\ k)$。其中 $A \to \alpha\beta$ 是一条语法产生式，点 $\bullet$ 表示该产生式右侧中已经匹配完成的位置；$k$ 是该产生式开始匹配时的输入位置，即该非终结符 $A$ 被预测或引入时已处理的词法单元数量。

在代码中， Earley 项可以表示为一个存储三个整数的 Task 结构体。为了方便，使用Vec\<Vec\<Task\>\>而不是哈希表来表示集合$ \{J_j\}_{j=0}^n $。

```rust
struct Task {
    rule: usize,    // 语法中解析规则的索引
    dot: usize,     // 规则中下一个符号的索引（点的位置）
    start: usize,   // 开始此规则时已看到的词法单元数量
}

worklists: Vec<Vec<Task>> = vec![vec![Task::new(0, 0, 0)]];
```


算法从集合 $J_0 = \{(S' \to \bullet S,\ 0)\}$ 开始，其中 $S$ 是语法的起始符号，$S'$ 是引入的人工起始符号。随后，算法按输入位置 $j = 0, 1, \ldots, n$ 依次构造各个集合 $J_j$。如果 $ (S' \to S \bullet, 0) $ 存在于集合 $ J_n $ 中，则算法成功解析了 $ n $ 个输入词法单元的序列 $ t_0t_1 \ldots t_{n-1} $，否则报告错误。


算法构造每个集合的流程如下:

在固定的输入位置 $j$ 上，反复对 $J_j$ 应用以下规则，直到 $J_j$ 不再发生变化为止。

- 若 $J_j$ 中包含一个项 $(A \to \alpha \bullet B\beta,\ k)$，其中 $B$ 是非终结符，则对每一条产生式 $B \to \gamma$，将项 $(B \to \bullet \gamma,\ j)$ 加入 $J_j$。该步骤称为预测（predict）。预测步骤不消耗输入符号，且可能触发进一步的预测。

- 若 $J_j$ 中包含一个完成项 $(B \to \gamma \bullet,\ k)$，则对集合 $J_k$ 中的每一个项 $(A \to \alpha \bullet B\beta,\ l)$，将项 $(A \to \alpha B \bullet \beta,\ l)$ 加入 $J_j$。该步骤称为完成（complete）。完成步骤不消耗输入符号。

当 $J_j$ 达到闭包后，若 $j < n$，则应用扫描规则。若 $J_j$ 中包含一个项 $(A \to \alpha \bullet t\beta,\ k)$，其中 $t$ 是终结符且 $t = t_j$，则将项 $(A \to \alpha t \bullet \beta,\ k)$ 加入 $J_{j+1}$。扫描（scan）是唯一会推进输入位置的规则。


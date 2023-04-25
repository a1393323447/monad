## 前言

曾经看过这样一个观点：“程序员应该每年都学一门新语言”。不同的编程语言都会有一个不同的计算模型。同一个问题在不同的计算模型中实现的思路也不同。当我们使用不同的语言时，往往能够从一个全新的角度去思考以往遇到的问题。而这也能够促进我们对已经学过的编程语言的认识。

接触了很多命令式编程语言之后，我一直想着学一门函数式编程语言。简单地了解过一点 `Haskell` ，但学得很浅，而且也是按照命令式的思维去写函数式的代码，导致写出来的代码十分冗余、混乱。最近在知乎上刷到了一个关于 `Haskell` 里 `IO Monad` 的问题，我就又生出了学习函数式编程的心思。刚好刷到 `UMB CS450` 课程，该课程以 `Racket` 为教学语言，主要讲述高级语言的程序结构。我一看视频就有种听不下来的感觉。一连看了快两周，看到了第 29 讲。课程中提到的 `Monad` 和 `CPS` 就给了我一种从来没有过的思考方式，让我颇有一种顿悟的感觉（虽然可能是错觉）。于是打算写一篇文章来记录一下，也当作学习笔记。

文章的标题模仿了 `CSAPP` 的译名，但显然我也到不了“深入”的程度，所以也就将这个词从标题中剔除。: ) 话不多说，我们进入正题。

## 热身

热身部分是为了不熟悉 `racket` 的读者写的一些语法知识，帮助他们读完这篇文章。对 `racket` 或者是 `lisp` 类语言有所了解的读者可以跳过这一部分。

```
; 注释

; 定义变量
(define a 10)

; lambda 函数
(lambda (param1 param2) (+ param1 param2))

; 定义函数
(define (foo x) (+ x 1))

; 定义函数，且对输入输出进行检查
(define/contract (foo x y)
; 要求 x y 为 number 返回值也为 number
; 即 (number? x) 返回 true
  (-> number? number? number?)
  (+ x y))

; 'hello 是一个 symbol 可以理解为一个特殊的字符串
```

## 副作用与纯函数

 在直接喷出一大堆专业名词之前，我们先再来提一下很多函数式编程语言（如: `Haskell`）都会避免 `副作用` 和 追求达成的 `纯函数` 。

**副作用**一般来说是指在调用一个函数后，函数隐式地修改了函数作用域之外的一些变量的状态、使用I/O、引发异常、中止错误。而没有副作用的函数被称为**纯函数**。

没有副作用，意味着：

-   同一个函数，相同的输入总能得到相同的结果。当我们遇到 bug 时，也更容易复现 bug 。
-   没有隐式地修改作用域外的变量，我们就不容易陷入复杂的、难以察觉、难以追溯的错误。
-   没有副作用的函数，我们也更容易验证其正确性

纯函数听起来是个好东西，那就让我们用起来吧。

## 纯函数中的“副作用”

假设我们要实现一门解释型语言。这门语言提供定义变量、变量赋值的功能。假设变量的类型只能是数字的情况下，我们要提供一个类似于堆分配的功能，该如何实现呢？

在命令式编程语言中，我们可以简单地将堆定义为一个 `HashTable<handle, val>` 。然后定义变量，读取变量，变量赋值的功能就相当于对这个哈希表的插入和修改。于是我们得到如下的 `api`：

```rust
struct Handle(usize);

struct Heap {
    data: HashTable<Handle, i32>,
}

trait Heap {
    fn alloc(&mut self, init: i32) -> Handle;
    fn read(&mut self, handle: Handle) -> i32;
    fn write(&mut self, handle: Handle, val: i32);
}
```

而为了避免副作用，我们应该怎么办呢？答案很简单，甚至可以说是有点无脑。我们只要把修改之前的哈希表和修改后的哈希表看成不同的哈希表就行。

在这个思路下，上面的 `api` 就可以改成：

```rust
trait Heap: Sized {
    fn alloc(self, init: i32) -> Effect<Self, Handle>;
    fn read(self, handle: Handle) -> i32;
    fn write(self, handle: Handle, val: i32) -> Effect<Self, ()>;
}

struct Effect<H: Heap, T> {
    state: H,
    result: T
}
```

换成 `Racket` 来表达，就是：

```scheme
(struct heap (data) #:transparent)
(struct handle (id) #:transparent)
(struct effect (state result) #:transparent)

(define (empty-heap) (heap (hash)))

(define/contract (heap-alloc hp init)
  (-> heap? number? effect?)
  (define data (heap-data hp))
  (define next-id (hash-count hp))
  (define new-handle (handle next-id))
  (define new-data (hash-set data new-handle init))
  (define new-heap (heap new-data))
  (effect new-heap new-handle))

(define/contract (heap-read hp hd)
  (-> heap? handle? number?)
  (hash-ref (heap-data hp) hd))

(define/contract (heap-write hp hd val)
  (-> heap? handle? number? effect?)
  (define data (heap-data hp))
  (define new-data (hash-set data hd val))
  (define new-heap (heap new-data))
  (effect new-heap (void)))
```

## 程序迭代

### program 0

有了上面的 `api` 后，我们再来实现一个小程序：在堆上分配两个变量 `x` 和 `y` 初始值分别为 `1` 和 `2` ，最后再在堆上分配一个变量 `z` ，它的值为 `x + y` ：

 ```scheme
 (define/contract (prog0 hp0)
   (-> heap? effect?)
   ; allocate x
   (define eff-x (heap-alloc hp0 1))
   (define x (effect-result eff-x))
   (define hp1 (effect-state eff-x))
   ; allocate y
   (define eff-y (heap-alloc hp1 2))
   (define y (effect-result eff-y))
   (define hp2 (effect-state eff-y))
   ; read x y
   (define x-val (heap-read hp2 x))
   (define y-val (heap-read hp2 y))
   ; alloc z
   (heap-alloc hp2 (+ x-val y-val)))
 ```

我们再定义一个函数来运行这个程序：

```scheme
(define (run-state op)
  ; 我们只关心最终的 heap
  (effect-state (op (empty-heap))))

; 运行 prog0
(run-state prog0)
```

运行结果如下：

```
(heap
 (hash
  (handle 0) ; x = 1
  1
  (handle 1) ; y = 2
  2
  (handle 2) ; z = 3
  3))
```

运行的结果也很简单易懂，我们就把这个结果定义为 `expected-heap` ，进行一定的单元测试吧：

```scheme
; 引入单元测试库
(require rackunit)

(define expected-heap
  (heap (hash (handle 0) 1
            (handle 1) 2
            (handle 2) 3)))

(check-equal? (run-state prog0) expected-heap)
```

再次运行，没有任何输出，说明我们通过了测试。好耶 `\(^o^)/~`

### program 1

我们来审视一下这个程序：
$$
(def \ x) \stackrel{H}{\longrightarrow} (H', result) \\
(add \ x \ y) \stackrel{H}{\longrightarrow} (H', result) \\
$$
这一小段实现了两个功能 `def` 和 `add`：

-   `(def x)` 接收一个 `heap H` 输出一个包含新的 `heap H'` 的 `effect` 结构 `(H', result)`  
-   `(add x y)` 接收一个 `heap H` ，计算 `x` 和 `y` 的和，在 `H` 中再申请一个新的变量，最后输出一个包含新的 `heap H'` 的 `effect` 结构 `(H', result)`  

所以我可以将这两个功能抽象出来：

```scheme
(define/contract (def val)
  (-> number? (-> heap? effect?))
  (lambda (in-hp)
    (heap-alloc in-hp val)))

(define/contract (add x y)
  (-> handle? handle? (-> heap? effect?))
  (lambda (in-hp)
    (define x-val (heap-read in-hp x))
    (define y-val (heap-read in-hp y))
    (define z-val (+ x-val y-val))
    ((def z-val) in-hp)))
```

我们再通过 `def` 和 `add` 重写 `prog0`：

```scheme
(define/contract (prog1 hp0)
  (-> heap? effect?)
  ; allocate x
  (define eff-x ((def 1) hp0))
  (define x (effect-result eff-x))
  (define hp1 (effect-state eff-x))
  ; allocate y
  (define eff-y ((def 2) hp1))
  (define y (effect-result eff-y))
  (define hp2 (effect-state eff-y))
  ; define y with val x + y 
  ((add x y) hp2))

(check-equal? (run-state prog1) expected-heap)
```

再次运行程序，不出所料，我们通过了测试。

### program 3

通过抽象出 `def` 和 `add` ，我们很容易发现 `program 2` 主要做了：
$$
\stackrel{H0}{\longrightarrow} (def \ 1)
\stackrel{H1}{\longrightarrow} (def \ 2)
\stackrel{H2}{\longrightarrow} (add\ x\ y)
$$
我们很容易总结出一个模式：
$$
\stackrel{H0}{\longrightarrow} operation_0
\stackrel{H1}{\longrightarrow} operation_1
\stackrel{H2}{\longrightarrow}\ ...
$$
那么与其像 `program 2` 那样一直手动连接两个操作，我们不如定义一个 `bind` 函数替我们做这件事吧：

```scheme
; 我们先定义一个 operation 应满足的接口
; 从上面的模式我们可以总结出:
; operation 应该接受一个 heap 返回一个 effect (H', result)
(define/contract (operation? op)
  (-> (-> heap? effect?) boolean?)
  #t)

; 那么 bind 函数就是接受两个 operation 并将它们连接成一个新的 operaion
(define/contract (bind op1 op2)
  (-> operation? operation? operation?)
  ; new operation
  (lambda (in-hp)
    (define op1-eff (op1 in-hp))
    (define op1-hp (effect-state op1-eff))
    (define op1-res (effect-result op1-eff)) ; 注意这行
    (op2 op1-hp)))
```

但我们很快就发现一个问题：`op2` 无法获取到 `op1` 的结果 `op1-res` 。这个 `bind` 的实现无疑是有问题的。那么我们来思考一下，我们该如何让 `op2` 获取到 `op1-res` 呢？

对函数式编程有一定了解的读者可能很快就想到了 `lambda` 函数。我们可以将 `(bind op1 op2)` 改成 `(bind op cont)` 。让 `bind` 接受一个 `lambda` 函数 `continuation` 。`cont` 接受 `op-res` 并返回接下来要进行的操作，如果接下来有多个要进行的操作，那么就用 `bind` 进行连接。于是我们可以将 `bind` 实现为：

```scheme
; 根据上面的总结，我们得知一个 cont-op 接受上一次操作的返回值
; 然后返回接下来要进行的操作
(define/contract (cont-op? cont)
  (-> (-> any/c operation?) boolean?)
  #t)

; 于是 bind 可以实现为:
(define/contract (bind op cont)
  (-> operation? cont-op? operation?)
  ; new operation
  (lambda (in-hp)
    (define op-eff (op in-hp))
    (define op-hp (effect-state op-eff))
    (define op-res (effect-result op-eff))
    ((cont op-res) op-hp)))
```

我们来用 `bind` 实现 `prog3` 吧：

```scheme
; 注意: prog3 依旧是一个接受 heap 返回 effect 的函数
; 但和以往直接定义函数不同的是, prog3 是 bind 的返回值
(define prog3
  ; 定义一个初始值为 1 的变量
  (bind (def 1)
    ; 将该变量命名为 x
    (lambda (x)
      ; 定义一个初始值为 2 的变量
      (bind (def 2)
        ; 将该变量命名为 y
        (lambda (y)
          ; 定义一个变量值为 x + y
          (add x y))))))

(check-equal? (run-state prog3) expected-heap)
```

同样地，运行程序进行测试。没有错误，完美通过。

### program 4

相信大多数人看到 `prog3` 结尾那一串右括号，都会想起一个笑话：

>   一个黑客偷到了美国用于导弹控制的LISP代码的最后一页，却发现这最后一页全是右括号 )

有什么改进的方法呢？我知道的有两个：

-   利用 Racket 的可变长参数特性，定义一个函数 `(seq . ops)` 。然后递归地将各个操作用 `bind` 串联起来
-   利用 Racket 的宏系统，定义一个宏将各个操作串联起来

在这篇文章中，我会使用第二种方式。第一种就留给读者作为作业了 : )

我们定义一个宏 `do` ：

```scheme
(define-syntax do
  (syntax-rules (<-)
    ; Only one monadic-op, return it
    [(_ mexp) mexp]
    ; A binding operation
    [(_ var <- mexp rest ...) (bind mexp (lambda (var) (do rest ...)))]
    ; No binding operator, just ignore the return value
    {(_ mexp rest ...)        (bind mexp (lambda (_) (do rest ...)))}))
```

有了上面的 `do` 宏之后，我们就可以将 `prog3` 改写为：

```scheme
(define prog4
  (do
    x <- (def 1)
    y <- (def 2)
    (add x y)))
    
(check-equal? (run-state prog4) expected-heap)
```

是不是感觉程序一下子清爽了很多、高级了很多呢？想必接触过 `Haskell` 的读者会联想到 `Haskell` 中的 `do` 。而这种模式的名字也呼之欲出了：`Monad` 。

## Monad

### 定义

对于 `monad` 的定义，我放 `CS450` 中原文给大家品鉴一波：

>   A monad is a functional pattern which can be categorized of two base combinators:
>
>   \- Bind: combines two effectful operations O1 and O2. Operation O1 produces a value
>
>    that consumed by operation O2
>
>   \- Pure: converts a pure value to a monadic operation, which can then be chained
>
>    with bind.

### pure 组合子

看完这段定义后，我们会发现，我们已经定义好了 `bind` 还缺少了一个组合子 `pure` 。那么这个组合子究竟要怎么定义，它又有什么用呢？

我们来考虑这样一个情况：我们现在要定义的变量不再是简单的 `1 + 2` 而是一个很复杂的式子，我们想要在 `do` 的上下文中定义一些中间变量，进行一些计算。那么我们现在是没办法做到的。想一下我们在 `bind` 中是如何让被连接起来的操作共享计算结果的？没错，是通过 `lambda` 函数做到的。

当我们想要在 `do` 上下文中定义一些变量、进行一些计算，并将这些值共享给其它操作的时候，也必须要通过 `lambda` 传递，所以我们需要一个 `pure` 组合子将这些 `pure value` 转换为 `monadic operation` 通过 `bind` 和其他操作连接起来，从而达到共享这些计算结果的目的。

搞清楚 `pure` 的作用后，`pure` 的实现方式也自然而然地得到了：

```scheme
(define (pure v)
  ; 保持 hp 不变 (这也是 pure 的意义)
  ; 将这个操作的 result 设为 v
  (lambda (hp) (effect hp v)))
```

有了 `pure` 之后我们就可以做下面的操作了：

```scheme
(define prog5
  (do
    x-val <- (pure (- 2 1)) ; 我的大脑不允许我想出比这更复杂的式子了 :)
    x <- (def x-val)
    y <- (def 2)
    (add x y)))

(check-equal? (run-state prog5) expected-heap)
```

老规矩，运行测试。不出所料，通过测试。

### 仅此而已？

看到这里，有些人会决定很可笑、或者很愤怒：绕了一大圈，就是为了模拟赋值这种副作用？这在其它语言里不是随手写吗？这和脱裤子放屁有什么区别？你莫不是在消遣洒家！

当然不是啦~ `Monad` 当然不是仅此而已。再看一下 `Monad` 的定义，总结一下来说：`Monad` 包含两个组合子 `bind` 和 `pure` 。通过这两个组合子，我们将一些随着程序进行不断被改变、不断被传递的状态隐藏起来（如：前面程序中的 `heap` ）。那么我们思考一下，程序中的状态只有普通的变量吗？当然不是！程序处于正常状态和异常状态、从正常状态装换为异常状态，这一系列行为也完美符合了 `Monad` 的定义。所以接下来，我来介绍一下如何用 `Monad` 的方式进行异常处理。

### 异常处理

在有了 `Monad` 的理论之后，我们来思考一下，需要异常处理的操作应该返回什么样的状态？答案很显然，正常状态和异常状态。所以我们可以定义为：

```scheme
(struct return-ok (val) #:transparent)
(struct return-err (val) #:transparent)

; 因为没有太过深入学习 racket 
; 我没能找到比较优雅地写 union type 的方法
; 所以就这样凑合着用
(define (result? res) 
  (or (return-ok? res) 
      (return-err? res)))
```

那么异常处理的操作理论上就是要接受上一次操作的 `result`，消耗它，并产生一个新的 `result` 。但仔细想想，`result` 中其实包含两类：`return-ok` 和 `return-err` 。我们的操作希望接收的其实只有正确的结果 `return-ok` 。而只要其中某个操作产生`return-err` ，我们不希望继续将错误向下传，而是立刻向上抛出，最抵达我们事先定义的 `error handler` 中进行处理。所以我们的操作应该接受上一次操作的正确结果，并返回一个 `result` 。而对 `result` 的处理我们选择将其放到 `bind` 中实现。

```scheme
(define (bind res cont)
  (cond
    ; 如果上一次的结果是 err 就向上抛出
    [(return-err? res) res]
    ; 如果是 ok 则取出 val 向下传递
    [else (cont (return-ok-val res))]))
```

`pure` 的实现就比较简单：

```scheme
(define (pure v)
  (return-ok v))
```

我们再来实现一个 `save-/`：

```scheme
(define (safe-/ x y)
  (cond [(= 0 y) (return-err 'division-by-zero)]
          [else (return-ok (/ x y))]))
```

我们也可以仿照 `do` 来实现 `try`：

```scheme
(define-syntax try
  (syntax-rules (<-)
    ; Only one monadic-op, return it
    [(_ mexp) mexp]
    ; A binding operation
    [(_ var <- mexp rest ...) (bind mexp (λ (var) (try rest ...)))]
    ; No binding operator, just ignore the return value
    {(_ mexp rest ...)        (bind mexp (λ (_) (try rest ...)))}))
```

但这样的 `try` 需要我们手动去对最后的结果进行判断，所以我们可以做得更好一点：

```scheme
(define-syntax try-impl
  (syntax-rules (<-)
    ; Only one monadic-op, return it
    [(_ mexp) mexp]
    ; A binding operation
    [(_ var <- mexp rest ...) (bind mexp (λ (var) (try-impl rest ...)))]
    ; No binding operator, just ignore the return value
    [(_ mexp rest ...)        (bind mexp (λ (_) (try-impl rest ...)))]))

(define-syntax try
  (syntax-rules (catch)
    [(_ ops ... catch (<err>) (body ...))
     (final-handler
      (try-impl ops ...)
      (λ (<err>) body ...))]))
```

 于是我们就可以写出这样的程序：

```scheme
(define a
  (try
   x <- (safe-/ 2 1)
   y <- (safe-/ 4 2)
   (h-pure (+ x y))

   catch (err) {
     (printf "~a\n" err)
   }))
(check-equal? a 4)
```

以及

```scheme
(try
   x <- (safe-/ 2 1)
   y <- (safe-/ 4 2)
   z <- (h-pure (- x y))
   (safe-/ (+ x y) z)

   catch (err) {
     (printf "err: ~a\n" err)
   })
; 输出 err: division-by-zero
```

## 总结

这篇文章主要从程序员的视角出发，一步步地引出 `Monad` 的概念。看到这里的你，是否有所感悟，想要大试身手了呢？那么试着用 `monad` 实现一下功能吧：

```python
# A stack-based evaluator
# 很多虚拟机都是基于栈实现的
# 你能通过 monad 在你喜欢的函数式语言里面模拟下面的程序吗?

def mult():
    x = pop()
    y = pop()
    push (x * y)

def prog():
    push(2)
    push(5)
    mult()
    push(2)
    mult()
```

最终的 `racket` 代码类似于：

```scheme
(define (prog3)
  (do
    (push 2)
    (push 5)
    (mul)
    (push 2)
    (mul)))
(check-equal? (run-state empty (prog3)) '(20))
```

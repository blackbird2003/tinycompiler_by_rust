def foo():
    global bar
    print(bar)
    bar = 1

bar = 0
foo()
print(bar)


def foo2():
    x = None
    def f(y): # y = 1
        nonlocal x
        def g(y):
            nonlocal x
            x = y + 1
            print("In g, x =", x)
        x = y
        g(x + 1)
        print("In f, x =", x)

    x = 0
    f(x + 1)
    print("In foo2, x =", x)

foo2()



def counter():
    count = -1
    def increment():
        nonlocal count
        count += 1
        return count
    return increment

counter1 = counter()
counter2 = counter()

for _ in range(3):
    print("outer counter:", counter1())
    for _ in range(2):
        print("  inner counter:", counter2())



import inspect

# def factorial(n):
#     result = 1
#     print("instances of n:", [frame[0].f_locals["n"] for frame in reversed(inspect.stack()[:-1])])
#     if n > 1:
#         result = n * factorial(n-1)
#     return result

# print(factorial(7))


def factorial():                             # n is stack[-1]
    stack.append(1)
    print(stack)
    if stack[-2] > 1:                        # n is stack[-2], result is stack[-1]
        stack.append(stack[-2]-1)
        print('~',stack)
        stack[-2] = stack[-3] * factorial()  # n is stack[-3], result is stack[-2]
        print("*", stack)
        stack.pop()
        print(stack)
    print(stack[0:-1])
    return stack.pop()

stack = []
stack.append(5)
print(factorial())
stack.pop()


def sopfr(n):
    def sopfr_aux(n):
        nonlocal div
        rec = 0
        if n % div == 0:
            # breakpoint, print stack
            var = {"main":[], "sopfr":["n","div"], "sopfr_aux":["n","rec"]}
            print([frame[0].f_locals[name] for frame in reversed(inspect.stack()[:-1]) for name in var[frame[3]]])
            rec = div
            if n != div:
                rec = rec + sopfr_aux(n // div)
        else:
            div = div + 1
            rec = sopfr_aux(n)
        return rec

    div = 2
    return sopfr_aux(n)

print(sopfr(42))
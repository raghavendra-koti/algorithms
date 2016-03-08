__author__ = 'Raghavendrai'

print("Trying out decorators")


def decorator(func):
    def decorator_func():
        print("Decorating...")
        func()
        print("Decorated")

    return decorator_func


@decorator
def function1():
    print("Calling my method")


function1()
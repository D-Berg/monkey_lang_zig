
def counter(x):
    if x > 997:
        print(x)
    else:
        foobar = 9999
        counter(x + 1)

counter(0)

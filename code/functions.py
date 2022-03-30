def factorial(n):
   i = 1
   x = 1
   while i <= n:
      x = x * i
      i = i + 1
   return x

iteration = 1
result = 0
while iteration <= 100000:
   result = factorial(15)
   iteration = iteration + 1

print(result)

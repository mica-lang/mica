local function factorial(n)
   local i = 1
   local x = 1
   while i <= n do
      x = x * i
      i = i + 1
   end
   return x
end

local result = 0
for _ = 1, 100000 do
   result = factorial(15)
end
return result

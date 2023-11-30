-- /setvar key=input 5 |
-- /setvar key=i 1 |
-- /setvar key=product 1 |
-- /while left=i right=input rule=lte "/mul product i \| /setvar key=product \| /addvar key=i 1" |
-- /getvar product |
-- /echo Factorial of {{getvar::input}}: {{pipe}} |
-- /flushvar input |
-- /flushvar i |
-- /flushvar product

input = 5
i = 1
product = 1
while i <= input do
    product = product * i
    i = i + 1
end
echo("Factorial of " .. input .. ": " .. product)
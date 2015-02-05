output_obj = DSpaceObj.get_output_obj(0)

len = #data
min = data[1]
max = min
sum = 0
avg = 0

for i = 2, len do
        val = data[i]
        sum = sum + val
        if val < min then
                min = val
        end

        if val > max then
                max = val
        end
end

avg = sum/len
--print(string.format("Number of elements in input data obj: %d", len))
--print(string.format("The data obj has min, max, avg: %.4f, %.4f, %.4f", min, max, avg))
output_obj:setval(1,0,0,0,min)
output_obj:setval(1,1,0,0,max)
output_obj:setval(1,2,0,0,avg)
return 3
----

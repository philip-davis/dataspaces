input_obj = DSpaceObj.get_input_obj(0)
output_obj = DSpaceObj.get_output_obj(0)

len = input_obj:getlen()
min = input_obj:getval(1,0,0,0)
max = min
sum = 0
avg = 0

for i = 0, len-1 do
        val = input_obj:getval(1,i,0,0)
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
------

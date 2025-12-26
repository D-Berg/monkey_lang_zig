---@param x number
local function counter(x)
    if x > 997 then
        print(x)
    else
        local foobar = 9999
        counter(x + 1)
    end
end

counter(0)

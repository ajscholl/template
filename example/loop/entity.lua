function capitalize(field)
    if #field < 2 then
        return string.upper(field)
    end

    return string.upper(string.sub(field, 1, 1)) .. string.sub(field, 2, -1)
end

function fieldName(field, fields)
    local longest = 0
    for _, f in pairs(fields) do
        if #f > longest then
            longest = #f
        end
    end

    local result = capitalize(field)
    for i = #field + 1, longest do
        result = result .. " "
    end

    return result
end

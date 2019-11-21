function capitalize(field)
    if #field < 2 then
        return string.upper(field)
    end

    return string.upper(string.sub(field, 1, 1)) .. string.sub(field, 2, -1)
end

function capitalize(field)
    if #field < 2 then
        return string.upper(field)
    end

    return string.upper(string.sub(field, 1, 1)) .. string.sub(field, 2, -1)
end

function snakeCase(name)
    local result = ""

    for i = 1, #name do
        local char = string.sub(name, i, i)
        if string.upper(char) == char and result ~= "" then
            result = result .. "_"
        end
        result = result .. char
    end

    return string.lower(result)
end

function pluralize(name)
    if #name < 2 then
        return name .. "s"
    end

    local lastTwo = string.sub(name, #name - 1)
    local last = string.sub(lastTwo, 2, 2)

    -- If the singular noun ends in ‑s, -ss, -sh, -ch, -x, or -z, add ‑es to the end to make it plural.
    if last == "s" or lastTwo == "sh" or lastTwo == "ch" or last == "x" or last == "z" then
        return name .. "es"
    end

    -- If the singular noun ends in -y and the letter before the -y is a vowel, simply add an -s to make it plural.
    if lastTwo == "ay" or lastTwo == "ey" or lastTwo == "iy" or lastTwo == "uy" or lastTwo == "oy" then
        return name .. "s"
    end

    -- If a singular noun ends in ‑y and the letter before the -y is a consonant, change the ending to ‑ies to make the noun plural.
    if last == "y" then
        return string.sub(name, 1, #name - 1) .. "ies"
    end

    -- If the singular noun ends in ‑o, add ‑es to make it plural.
    if last == "o" then
        return name .. "es"
    end

    -- If the singular noun ends in ‑is, the plural ending is ‑es.
    if lastTwo == "is" then
        return string.sub(name, 1, #name - 2) .. "es"
    end

    -- If the singular noun ends in ‑on, the plural ending is ‑a.
    if lastTwo == "on" then
        return string.sub(name, 1, #name - 2) .. "a"
    end

    -- To make regular nouns plural, add ‑s to the end.
    return name .. "s"
end

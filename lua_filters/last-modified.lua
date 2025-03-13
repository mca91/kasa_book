function format_date(date_str)
    -- Extract year, month, and day from Git date format (YYYY-MM-DD)
    local year, month, day = date_str:match("(%d+)-(%d+)-(%d+)")
    
    -- Convert to integers to remove leading zeros
    day = tonumber(day)
    month = tonumber(month)

    -- Convert month number to German month name
    local months = {
        "Januar", "Februar", "März", "April", "Mai", "Juni",
        "Juli", "August", "September", "Oktober", "November", "Dezember"
    }

    -- Return formatted date
    return day .. ". " .. months[month] .. " " .. year
end

function Meta(meta)
    local handle = io.popen("git log -1 --format=%cd --date=short")
    local last_modified = handle:read("*a")
    handle:close()

    -- Trim whitespace and format the date
    last_modified = last_modified:gsub("%s+", "")
    meta["last-modified"] = format_date(last_modified)

    return meta
end
function table_show(t, name, indent)
  local cart    -- a container
  local autoref  -- for self references

  local function isemptytable(t) return next(t) == nil end

  local function basicSerialize (o)
    local so = tostring(o)
    if type(o) == "function" then
      local info = debug.getinfo(o, "S")
      -- info.name is nil because o is not a calling level
      if info.what == "C" then
        return string.format("%q", so .. ", C function")
      else 
        -- the information is defined through lines
        return string.format("%q", so .. ", defined in (" ..
           info.linedefined .. "-" .. info.lastlinedefined ..
           ")" .. info.source)
      end
    elseif type(o) == "number" then
      return so
    else
      return string.format("%q", so)
    end
  end

  local function addtocart (value, name, indent, saved, field)
    indent = indent or ""
    saved = saved or {}
    field = field or name

    cart = cart .. indent .. field

    if type(value) ~= "table" then
      cart = cart .. " = " .. basicSerialize(value) .. ";\n"
    else
      if saved[value] then
        cart = cart .. " = {}; -- " .. saved[value] 
                .. " (self reference)\n"
        autoref = autoref ..  name .. " = " .. saved[value] .. ";\n"
      else
        saved[value] = name
        if isemptytable(value) then
          cart = cart .. " = {};\n"
        elseif value.tag then
          if value.tag == "token" then
            cart = cart .. string.format(" = [1;32m%s<[1;31m%s[1;32m>[0;m%s\n", value.tag, value.value,
              --table.concat({" (id:", value.id, ", line:", value._line,")"})
              ""
              )
          elseif value.repr and type(value.repr) == "function" then
            cart = cart .. string.format(" = [1;34m%s[0;m\n", value:repr(indent))
          else
            if value.kind then
              cart = cart .. string.format(" = [1;33m%s[0;m:[1;35m%s[0;m {\n", value.tag, value.kind)
            else
              cart = cart .. string.format(" = [1;33m%s[0;m {\n", value.tag)
            end
            for k, v in pairs(value) do
              if k ~= "tag" and k ~= "kind" then
                if type(k) == "string" and string.byte(k, 1) == 95 then
                else
                  k = basicSerialize(k)
                  local fname = string.format("%s[%s]", name, k)
                  field = string.format("[%s]", k)
                  -- three spaces between levels
                  addtocart(v, fname, indent .. "..", saved, field)
                end
              end
            end
            cart = cart .. indent .. "};\n"
          end
        else
          cart = cart .. " = {\n"
          for k, v in pairs(value) do
            k = basicSerialize(k)
            local fname = string.format("%s[%s]", name, k)
            field = string.format("[%s]", k)
            -- three spaces between levels
            addtocart(v, fname, indent .. "..", saved, field)
          end
          cart = cart .. indent .. "};\n"
        end
      end
    end
  end

  name = name or "__unnamed__"
  if type(t) ~= "table" then
    return name .. " = " .. basicSerialize(t)
  end
  cart, autoref = "", ""
  addtocart(t, name, indent)
  return cart --.. autoref
end

function table_show_orig(t, name, indent)
  local cart    -- a container
  local autoref  -- for self references

  local function isemptytable(t) return next(t) == nil end

  local function basicSerialize (o)
    local so = tostring(o)
    if type(o) == "function" then
      local info = debug.getinfo(o, "S")
      -- info.name is nil because o is not a calling level
      if info.what == "C" then
        return string.format("%q", so .. ", C function")
      else 
        -- the information is defined through lines
        return string.format("%q", so .. ", defined in (" ..
           info.linedefined .. "-" .. info.lastlinedefined ..
           ")" .. info.source)
      end
    elseif type(o) == "number" then
      return so
    else
      return string.format("%q", so)
    end
  end

  local function addtocart (value, name, indent, saved, field)
    indent = indent or ""
    saved = saved or {}
    field = field or name

    cart = cart .. indent .. field

    if type(value) ~= "table" then
      cart = cart .. " = " .. basicSerialize(value) .. ";\n"
    else
      if saved[value] then
        cart = cart .. " = {}; -- " .. saved[value] 
                .. " (self reference)\n"
        autoref = autoref ..  name .. " = " .. saved[value] .. ";\n"
      else
        saved[value] = name
        if isemptytable(value) then
          cart = cart .. " = {};\n"
        else
          cart = cart .. " = {\n"
          for k, v in pairs(value) do
            k = basicSerialize(k)
            local fname = string.format("%s[%s]", name, k)
            field = string.format("[%s]", k)
            -- three spaces between levels
            addtocart(v, fname, indent .. "  ", saved, field)
          end
          cart = cart .. indent .. "};\n"
        end
      end
    end
  end

  name = name or "__unnamed__"
  if type(t) ~= "table" then
    return name .. " = " .. basicSerialize(t)
  end
  cart, autoref = "", ""
  addtocart(t, name, indent)
  return cart .. autoref
end


function dump(obj, full, fp, indent, name)
  if not name then name = "root" end
  if not fp then fp = io.stdout end 
  if fp == true then
    if full then
      return table_show_orig(obj, name, indent)
    else
      return table_show(obj, name, indent)
    end

  else
    if full then
      fp:write(table_show_orig(obj, name, indent))
    else
      fp:write(table_show(obj, name, indent))
    end
    fp:flush()
  end
end


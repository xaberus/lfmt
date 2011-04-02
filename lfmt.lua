#!/usr/bin/env lua

local lpeg = require "dump";
local lpeg = require "lpeg";

local locale = lpeg.locale();

local P, S, V = lpeg.P, lpeg.S, lpeg.V;

local C, Cb, Cc, Cg, Cs, Cmt, Ct =
    lpeg.C, lpeg.Cb, lpeg.Cc, lpeg.Cg, lpeg.Cs, lpeg.Cmt, lpeg.Ct;

local shebang = P "#" * (P(1) - P "\n")^0 * P "\n";

local function T(k, tag, ...)
  local nms = {...}
  return Ct(k) / function(res)
    --dump(res, nil, nil, nil, "arr")
    local tab = {tag = tag}
    for k, v in ipairs(res) do
      local n = nms[k]
      if n then
        tab[n] = res[k]
      end
    end
    return tab
  end
end

local function K(k) -- keyword
  return (P(k) * -(locale.alnum + P "_"))
end

local lua = P {
  (shebang)^-1 * V "space" * V "chunk" * V "space" * -P(1);

  -- keywords

  keywords = K "and" + K "break" + K "do" + K "else" + K "elseif" +
             K "end" + K "false" + K "for" + K "function" + K "if" +
             K "in" + K "local" + K "nil" + K "not" + K "or" + K "repeat" +
             K "return" + K "then" + K "true" + K "until" + K "while";

  -- longstrings

  longstring = P { -- from Roberto Ierusalimschy's lpeg examples
    V "open" * C((P(1) - V "closeeq")^0) *
        V "close" / function (o, s) return s end;

    open = "[" * Cg((P "=")^0, "init") * P "[" * (P "\n")^-1;
    close = "]" * C((P "=")^0) * "]";
    closeeq = Cmt(V "close" * Cb "init", function (s, i, a, b) return a == b end)
  };

  -- comments & whitespace

  comment = P "--" * V "longstring" +
            P "--" * (P(1) - P "\n")^0 * (P "\n" + -P(1));

  space = (locale.space + V "comment")^0;

  -- Types and Comments

  Name = C((locale.alpha + P "_") * (locale.alnum + P "_")^0 - V "keywords");
  Number = C((P "-")^-1 * V "space" * P "0x" * locale.xdigit^1 *
               -(locale.alnum + P "_")) +
           C((P "-")^-1 * V "space" * locale.digit^1 *
               (P "." * locale.digit^1)^-1 * (S "eE" * (P "-")^-1 *
                   locale.digit^1)^-1 * -(locale.alnum + P "_")) +
           C((P "-")^-1 * V "space" * P "." * locale.digit^1 *
               (S "eE" * (P "-")^-1 * locale.digit^1)^-1 *
               -(locale.alnum + P "_"));
  String = C(P "\"" * (P "\\" * P(1) + (1 - P "\""))^0 * P "\"") +
           C(P "'" * (P "\\" * P(1) + (1 - P "'"))^0 * P "'") +
           C(V "longstring");

  -- Lua Complete Syntax

  chunk = Ct((V "space" * V "stat" * (V "space" * P ";")^-1)^0 *
              (V "space" * V "laststat" * (V "space" * P ";")^-1)^-1);

  block = V "chunk";

  stat =
          T((K "do" * V "space" * V "block" * V "space" * K "end"), "comp", "block") +

          T((K "while" * V "space" * V "exp" * V "space" * K "do" * V "space" *
             V "block" * V "space" * K "end"), "while", "exp", "dblock") +

          T((K "repeat" * V "space" * V "block" * V "space" * K "until" *
             V "space" * V "exp"), "repeat", "dblock", "exp") +

          T((K "if" * V "space" * V "exp" * V "space" * K "then" *
             V "space" * V "block" * V "space" *
             Ct((T((K "elseif" * V "space" * V "exp" * V "space" * K "then" *
              V "space" * V "block" * V "space"), "elif", "exp", "dblock")
             )^0) *
             (K "else" * V "space" * V "block" * V "space")^-1 * K "end"), "ifc", "exp", "dblock", "elifs", "eblock") +

          (T((K "for" * V "space" * V "Name" * V "space" * P "=" * V "space" *
             V "exp" * V "space" * P "," * V "space" * V "exp" *
             (V "space" * P "," * V "space" * V "exp")^-1), "ifor", "var", "init", "cond", "incr") * V "space" *
             K "do" * V "space" * V "block" * V "space" * K "end")
              / function(cl, block) cl.dblock = block return cl end +

          T((K "for" * V "space" * V "namelist" * V "space" * K "in" * V "space" *
             V "explist" * V "space" * K "do" * V "space" * V "block" *
             V "space" * K "end"), "tfor", "names", "exps", "dblock") +

          T((K "function" * V "space" * V "funcname" * V "space" *  V "funcbody"), "func", "name", "body") +
          T((K "local" * V "space" * K "function" * V "space" * V "Name" *
             V "space" * V "funcbody"), "lfunc", "name", "body") +

          T((K "local" * V "space" * V "namelist" *
             (V "space" * P "=" * V "space" * V "explist")^-1), "ldef", "names", "exps") +

          T((V "varlist" * V "space" * P "=" * V "space" * V "explist"), "def", "vars", "exps") +

          V "functioncall";

  laststat =
          T((K "return" * (V "space" * V "explist")^-1), "ret", "exps") +

          T((K "break"), "brk");

  funcname = T((Ct(V "Name" * (V "space" * P "." * V "space" * V "Name")^0) *
      (V "space" * P ":" * V "space" * V "Name")^-1), "fname", "names", "mtd");

  namelist = Ct(V "Name" * (V "space" * P "," * V "space" * V "Name")^0);

  varlist = Ct(V "var" * (V "space" * P "," * V "space" * V "var")^0);

  -- Let's come up with a syntax that does not use left recursion
  -- (only listing changes to Lua 5.1 extended BNF syntax)
  -- value ::= nil | false | true | Number | String | '...' | function |
  --           tableconstructor | functioncall | var | '(' exp ')'
  -- exp ::= unop exp | value [binop exp]
  -- prefix ::= '(' exp ')' | Name
  -- index ::= '[' exp ']' | '.' Name
  -- call ::= args | ':' Name args
  -- suffix ::= call | index
  -- var ::= prefix {suffix} index | Name
  -- functioncall ::= prefix {suffix} call

  -- Something that represents a value (or many values)
  value = T((K "nil"), "nil") +
          T((K "false"), "false") +
          T((K "true"), "true") +
          T((V "Number"), "nval", "value") +
          T((V "String"), "sval", "value") +
          T((P "..."), "elps") +
          V "function" +
          V "tableconstructor" +
          V "functioncall" +
          V "var" +
          T((P "(" * V "space" * V "exp" * V "space" * P ")"), "par", "exp");

  -- An expression operates on values to produce a new value or is a value
  exp = T((V "unop" * V "space" * V "exp"), "unop", "op", "exp") +
        ((V "value" * (V "space" * V "binop" * V "space" * V "exp")^-1) /
          function(...)
            local a = {...}
            if #a == 1 then
              return {tag = "val", val = a[1]}
            else
              return {tag = "binop", a = {tag = "val", val = a[1]}, b = a[3], op = a[2]}
            end
          end);

  -- Index and Call
  index = T((P "[" * V "space" * V "exp" * V "space" * P "]"), "iidx", "exp") +
          T((P "." * V "space" * V "Name"), "nidx", "name");

  call = T((V "args"), "dcall", "args") +
         T((P ":" * V "space" * V "Name" * V "space" * V "args"), "mcall", "name", "args");

  -- A Prefix is a the leftmost side of a var(iable) or functioncall
  prefix = T((P "(" * V "space" * V "exp" * V "space" * P ")"), "ppfx", "exp") +
           T((V "Name"), "npfx", "name");
  -- A Suffix is a Call or Index
  suffix = V "call" +
           V "index";

  var = T((V "prefix" * Ct((V "space" * V "suffix" * #(V "space" * V "suffix"))^0) *
            V "space" * V "index"), "rvar", "pfx", "sfxs", "idx") +
        T(V "Name", "svar", "name");

  functioncall = T((V "prefix" *
                     Ct((V "space" * V "suffix" * #(V "space" * V "suffix"))^0) *
                 V "space" * V "call"), "fcall", "pfx", "sfxs", "call");

  explist = Ct(V "exp" * (V "space" * P "," * V "space" * V "exp")^0);

  args = T((P "(" * V "space" * (V "explist" * V "space")^-1 * P ")"), "args", "exps") +
         T((V "tableconstructor"), "tab", "value") +
         T((V "String"), "str", "value");

  ["function"] = T((K "function" * V "space" * V "funcbody"), "afunc", "body");

  funcbody = T((Ct(P "(" * V "space" * (V "parlist" * V "space")^-1 * P ")") *
                 V "space" *  V "block" * V "space" * K "end"), "fbody", "prms", "block");

  parlist = T((V "namelist" * (V "space" * P "," * V "space" * P "...")^-1), "parlst", "names", "va") +
            T(P "...", "parlst", "va");

  tableconstructor = T((P "{" * V "space" * (V "fieldlist" * V "space")^-1 * P "}"), "tcon", "flst");

  fieldlist = Ct(V "field" * (V "space" * V "fieldsep" * V "space" * V "field")^0
                  * (V "space" * V "fieldsep")^-1);

  field = T((P "[" * V "space" * V "exp" * V "space" * P "]" * V "space" * P "=" *
              V "space" * V "exp"), "ifld", "aexp", "exp") +
          T((V "Name" * V "space" * P "=" * V "space" * V "exp"), "nfld", "name", "exp") +
          T((V "exp"), "lfld", "exp");

  fieldsep = P "," +
             P ";";

  binop = T((K "and"), "and") + -- match longest token sequences first
          T((K "or"), "or") +
          T((P ".."), "con") +
          T((P "<="), "le") +
          T((P ">="), "ge") +
          T((P "=="), "eq") +
          T((P "~="), "ne") +
          T((P "+"), "add") +
          T((P "-"), "sub") +
          T((P "*"), "mul") +
          T((P "/"), "div") +
          T((P "^"), "pow") +
          T((P "%"), "mod") +
          T((P "<"), "ls") +
          T((P ">"), "gt");

  unop = T((P "-"), "unm") +
         T((P "#"), "len") +
         T((K "not"), "not");
};

function ast_prefix(tree, indent)
  --dump(tree, nil, nil, nil, "pfx")
  local h = {
    ["ppfx"] = function(tree, indent)
      return "<ppfx>"
    end,
    ["npfx"] = function(tree, indent)
      return tree.name
    end
  }

  return h[tree.tag](tree, indent)
end

function ast_index(tree, indent)
  --dump(tree, nil, nil, nil, "idx")
  local h = {
    ["nidx"] = function(tree, indent)
      return "." .. tree.name
    end,
    ["iidx"] = function(tree, indent)
      return "[" .. ast_exp(tree.exp, indent)  .. "]"
    end,
  }

  return h[tree.tag](tree, indent)
end


function ast_suffixes(tree, indent)
  local tab = {}
  for k, v in ipairs(tree) do
    tab[#tab+1] = ast_suffix(v, indent)
  end
  return table.concat(tab, "")
end

function ast_suffix(tree, indent)
  --dump(tree, nil, nil, nil, "sfx")
  local h = {
    ["nidx"] = function(tree, indent)
      return ast_index(tree, indent)
    end,
    ["iidx"] = function(tree, indent)
      return ast_index(tree, indent)
    end,
    ["mcall"] = function(tree, indent)
      return ast_call(tree, indent)
    end,
    ["dcall"] = function(tree, indent)
      return ast_call(tree, indent)
    end,
  }

  return assert(h[tree.tag], tree.tag)(tree, indent)
end

function ast_var(tree, indent)
  --dump(tree, nil, nil, nil, "var")
  local h = {
    ["svar"] = function(tree, indent)
      return tree.name
    end,
    ["rvar"] = function(tree, indent)
      return ast_prefix(tree.pfx, indent) .. ast_suffixes(tree.sfxs, indent) .. ast_index(tree.idx, indent)
    end,
  }

  return h[tree.tag](tree, indent)
end

function ast_field(tree, indent)
  --dump(tree, nil, nil, nil, "fld")
  local h = {
    ["nfld"] = function(tree, indent)
      return tree.name .. " = " .. ast_exp(tree.exp, indent)
    end,
    ["ifld"] = function(tree, indent)
      return "[" .. ast_exp(tree.aexp, indent) .. "] = " .. ast_exp(tree.exp, indent)
    end,
    ["lfld"] = function(tree, indent)
      return ast_exp(tree.exp, indent)
    end,
  }

  return h[tree.tag](tree, indent)
end

function ast_parlist(tree, indent)
  --dump(tree, nil, nil, nil, "parlst")
  local h = {
    ["parlst"] = function(tree, indent)
      local ret = "("
      if tree.names then
        ret = ret .. table.concat(tree.names, ", ")
      end
      if tree.va then
        ret = ret .. ", ..."
      end
      ret = ret .. ")"
      return ret
    end
  }

  return h[tree.tag](tree, indent)
end

function ast_funcbody(tree, indent)
  --dump(tree, nil, nil, nil, "fbody")
  local h = {
    ["fbody"] = function(tree, indent)
      local tab = {}
      for k, v in ipairs(tree.prms) do
        tab[#tab+1] = "(" .. ast_parlist(v, indent) .. ")"
      end
      return table.concat(tab, "") .. "\n" .. ast_chunk(tree.block, indent .. "  ") .. indent .. "end"
    end
  }

  return h[tree.tag](tree, indent)
end


function ast_value(tree, indent)
  --dump(tree, nil, nil, nil, "val")
  local h = {
    ["fcall"] = function(tree, indent)
      return ast_prefix(tree.pfx, indent) .. ast_suffixes(tree.sfxs, indent) .. ast_call(tree.call, indent)
    end,
    ["sval"] = function(tree, indent)
      return tree.value
    end,
    ["nval"] = function(tree, indent)
      return tree.value
    end,
    ["tcon"] = function(tree, indent)
      if tree.flst then
        local indent2 = indent .. "  "
        if #tree.flst > 1 then
          local fields = {}
          for k, v in ipairs(tree.flst) do
            fields[#fields+1] = indent2 .. ast_field(v, indent2) .. ";\n"
          end
          return "{\n" .. table.concat(fields, "") .. indent  .. "}"
        else
          return "{" .. ast_field(tree.flst[1], indent2) .. "}"
        end
      else
        return "{}"
      end
    end,
    ["afunc"] = function(tree, indent)
      return "function" .. ast_funcbody(tree.body, indent)
    end,
    ["svar"] = function(tree, indent)
      return ast_var(tree, indent)
    end,
    ["rvar"] = function(tree, indent)
      return ast_var(tree, indent)
    end,
    ["nil"] = function(tree, indent)
      return "nil"
    end,
    ["false"] = function(tree, indent)
      return "false"
    end,
    ["true"] = function(tree, indent)
      return "true"
    end,
    ["par"] = function(tree, indent)
      return "(" .. ast_exp(tree.exp, indent) .. ")"
    end,
    ["elps"] = function(tree, indent)
      return "..."
    end,
  }

  return assert(h[tree.tag], tree.tag)(tree, indent)
end

function ast_args(tree, indent)
  --dump(tree, nil, nil, nil, "args")
  local h = {
    ["args"] = function(tree, indent)
      if tree.exps then
        return "(" .. ast_exps(tree.exps, indent) .. ")"
      else
        return "()"
      end
    end,
    ["tab"] = function(tree, indent)
      return ast_value(tree.value, indent)
    end,
    ["str"] = function(tree, indent)
      return tree.value
    end
  }

  return h[tree.tag](tree, indent)
end


function ast_call(tree, indent)
  --dump(tree, nil, nil, nil, "call")
  local h = {
    ["dcall"] = function(tree, indent)
      return ast_args(tree.args, indent)
    end,
    ["mcall"] = function(tree, indent)
      return ":" .. tree.name .. ast_args(tree.args, indent)
    end,
  }

  return h[tree.tag](tree, indent)
end

function ast_exps(tree, indent)
  local tab = {}
  for k, v in ipairs(tree) do
    tab[#tab+1] = ast_exp(v, indent)
  end
  return table.concat(tab, ", ")
end

function ast_unop(tree, a, indent)
  local h = {
    ["unm"] = function(tree, indent)
      return "-" .. ast_exp(a, indent)
    end,
    ["not"] = function(tree, indent)
      return "not " .. ast_exp(a, indent)
    end,
    ["len"] = function(tree, indent)
      return "#" .. ast_exp(a, indent)
    end,
  }

  return assert(h[tree.tag], tree.tag)(tree, indent)
end

function ast_binop(tree, a, b, indent)
  --dump(tree, nil, nil, nil, "binop")
  local h = {
    ["add"] = function(tree, indent)
      return ast_exp(a, indent) .. " + " .. ast_exp(b, indent)
    end,
    ["sub"] = function(tree, indent)
      return ast_exp(a, indent) .. " - " .. ast_exp(b, indent)
    end,
    ["mul"] = function(tree, indent)
      return ast_exp(a, indent) .. " * " .. ast_exp(b, indent)
    end,
    ["div"] = function(tree, indent)
      return ast_exp(a, indent) .. " / " .. ast_exp(b, indent)
    end,
    ["mod"] = function(tree, indent)
      return ast_exp(a, indent) .. " % " .. ast_exp(b, indent)
    end,
    ["pow"] = function(tree, indent)
      return ast_exp(a, indent) .. " ^ " .. ast_exp(b, indent)
    end,
    ["ne"] = function(tree, indent)
      return ast_exp(a, indent) .. " ~= " .. ast_exp(b, indent)
    end,
    ["eq"] = function(tree, indent)
      return ast_exp(a, indent) .. " == " .. ast_exp(b, indent)
    end,
    ["le"] = function(tree, indent)
      return ast_exp(a, indent) .. " <= " .. ast_exp(b, indent)
    end,
    ["ge"] = function(tree, indent)
      return ast_exp(a, indent) .. " >= " .. ast_exp(b, indent)
    end,
    ["ls"] = function(tree, indent)
      return ast_exp(a, indent) .. " < " .. ast_exp(b, indent)
    end,
    ["gt"] = function(tree, indent)
      return ast_exp(a, indent) .. " > " .. ast_exp(b, indent)
    end,
    ["con"] = function(tree, indent)
      return ast_exp(a, indent) .. " .. " .. ast_exp(b, indent)
    end,
    ["and"] = function(tree, indent)
      return ast_exp(a, indent) .. " and " .. ast_exp(b, indent)
    end,
    ["or"] = function(tree, indent)
      return ast_exp(a, indent) .. " or " .. ast_exp(b, indent)
    end,
  }

  return assert(h[tree.tag], tree.tag)(tree, indent)
end


function ast_exp(tree, indent)
  --dump(tree, nil, nil, nil, "exp")
  local h = {
    ["val"] = function(tree, indent)
      return ast_value(tree.val, indent)
    end,
    ["unop"] = function(tree, indent)
      return ast_unop(tree.op, tree.exp, indent)
    end,
    ["binop"] = function(tree, indent)
      return ast_binop(tree.op, tree.a, tree.b, indent)
    end,
  }

  return h[tree.tag](tree, indent)
end

function ast_funcname(tree, indent)
  local h = {
    ["fname"] = function(tree, indent)
      --dump(tree, nil, nil, nil, "fname")
      if tree.mtd then
        return table.concat(tree.names, ".") .. ":" .. tree.mtd
      else
        return table.concat(tree.names, ".")
      end
    end,
  }
  return assert(h[tree.tag], tree.tag)(tree, indent)
end

function ast_chunk(tree, indent)
  local h = {
    ["def"] = function(tree, indent)
      local vars = {}
      for k, v in ipairs(tree.vars) do
        vars[#vars+1] = ast_var(v, indent)
      end
      return indent .. table.concat(vars, ", ") .. " = " .. ast_exps(tree.exps, indent) .. "\n"
    end,
    ["ldef"] = function(tree, indent)
      --dump(tree, nil, nil, nil, "ldef")
      local vars = {}
      if tree.exps then
        local exps = {}
        for k, v in ipairs(tree.exps) do
          exps[#exps+1] = ast_exp(v, indent)
        end
        return indent .. "local " .. table.concat(tree.names, ", ") .. " = " .. table.concat(exps, ", ") .. "\n"
      else
        return indent .. "local " .. table.concat(tree.names, ", ") .. "\n"
      end
    end,
    ["lfunc"] = function(tree, indent)
      --dump(tree, nil, nil, nil, "lfunc")
      return "local function " .. tree.name .. ast_funcbody(tree.body, indent) .. "\n"
    end,
    ["func"] = function(tree, indent)
      --dump(tree, nil, nil, nil, "func")
      return "function " .. ast_funcname(tree.name, indent) .. ast_funcbody(tree.body, indent) .. "\n"
    end,
    ["fcall"] = function(tree, indent)
      return indent .. ast_prefix(tree.pfx, indent) .. ast_suffixes(tree.sfxs, indent) .. ast_call(tree.call, indent)  .. "\n"
    end,
    ["ifc"] = function(tree, indent)
      --dump(tree, nil, nil, nil, "def")
      local ret = ""
      local indent2 = indent .. "  "
      ret = ret .. indent .. "if " .. ast_exp(tree.exp) .. " then" .. "\n"
      ret = ret .. ast_chunk(tree.dblock, indent2)
      local tab = {}
      for k, v in ipairs(tree.elifs) do
        --dump(v, nil, nil, nil, "def")
        tab[#tab+1] = indent .. "elseif " .. ast_exp(v.exp) .. "\n" .. ast_chunk(v.dblock, indent2)
      end
      ret = ret .. table.concat(tab, "")
      if tree.eblock then
        ret = ret .. indent .. "else\n"
        ret = ret .. ast_chunk(tree.eblock, indent2)
      end
      ret = ret .. indent .. "end\n"
      return ret
    end,
    ["ifor"] = function(tree, indent)
      --dump(tree, nil, nil, nil, "ifor")
      local ret = ""
      ret = ret .. indent .. "for " .. tree.var
      ret = ret .. " = " .. ast_exp(tree.init, indent)
      ret = ret .. ", " .. ast_exp(tree.cond, indent)
      if tree.incr then
        ret = ret ..  ", " .. ast_exp(tree.incr, indent)
      end
      ret = ret ..  " do\n"
      ret = ret .. ast_chunk(tree.dblock, indent .. "  ")
      ret = ret .. indent .. "end\n"
      return ret
    end,
    ["tfor"] = function(tree, indent)
      --dump(tree, nil, nil, nil, "tfor")
      local ret = ""
      ret = ret .. indent .. "for " ..table.concat(tree.names, ", ")
      ret = ret .. " = " .. ast_exps(tree.exps) .. " do\n"
      ret = ret .. ast_chunk(tree.dblock, indent .. "  ")
      ret = ret .. indent .. "end\n"
      return ret
    end,
    ["comp"] = function(tree, indent)
      return indent .. "do" .. "\n" .. ast_chunk(tree, indent) .. indent .. "end" .. "\n"
    end,
    ["ret"] = function(tree, indent)
      if tree.exps then
        return indent .. "return " .. ast_exps(tree.exps, indent) .. "\n"
      else
        return indent .. "return" .. "\n"
      end
    end,
  }
  local tab = {}

  for k, v in ipairs(tree) do
    --dump(v, nil, nil, nil, "def")
    tab[#tab+1] = assert(h[v.tag], v.tag)(v, indent)
  end
  if indent == "" then
    return table.concat(tab, "\n")
  else
    return table.concat(tab, "")
  end
end

print(ast_chunk(lpeg.match(lua, io.input(assert(({...})[1], "no filename given")):read("*a")), ""))

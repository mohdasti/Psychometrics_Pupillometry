-- Word numbers every table in one sequence. After Quarto has written the
-- captions, renumber the tables that follow the Supplement heading as
-- Table S1, Table S2, and so on, and point the earlier references at
-- those numbers. Main-text tables are left as they are.

local nbsp = utf8.char(160)

local function inline_text(inlines)
  local parts = {}
  for _, el in ipairs(inlines) do
    if el.t == "Str" then
      table.insert(parts, el.text)
    elseif el.t == "Space" or el.t == "SoftBreak" or el.t == "LineBreak" then
      table.insert(parts, " ")
    elseif el.content then
      table.insert(parts, inline_text(el.content))
    end
  end
  return table.concat(parts)
end

local function normalize(text)
  return text:gsub(nbsp, " ")
end

local function table_number(text)
  return normalize(text):match("^Table%s+(%d+)")
end

local function is_gap(el)
  return el.t == "Space" or el.t == "SoftBreak" or (el.t == "Str" and (el.text == nbsp or el.text == ""))
end

local function rewrite_inlines(inlines, map)
  local out = {}
  local i = 1
  while i <= #inlines do
    local el = inlines[i]
    if el.t == "Str" then
      local num, rest = normalize(el.text):match("^Table%s+(%d+)(.*)$")
      if num and map[num] and el.text:match("^Table") then
        table.insert(out, pandoc.Str("Table " .. map[num] .. rest))
        i = i + 1
      else
        local nxt = inlines[i + 1]
        local nxt2 = inlines[i + 2]
        if el.text == "Table" and nxt and is_gap(nxt) and nxt2 and nxt2.t == "Str" then
          local n2, rest2 = nxt2.text:match("^(%d+)(.*)$")
          if n2 and map[n2] then
            table.insert(out, pandoc.Str("Table"))
            table.insert(out, pandoc.Space())
            table.insert(out, pandoc.Str(map[n2] .. rest2))
            i = i + 3
          else
            table.insert(out, el)
            i = i + 1
          end
        else
          table.insert(out, el)
          i = i + 1
        end
      end
    elseif el.content then
      el.content = rewrite_inlines(el.content, map)
      table.insert(out, el)
      i = i + 1
    else
      table.insert(out, el)
      i = i + 1
    end
  end
  return out
end

local function each_row(rows, fn)
  for _, row in ipairs(rows or {}) do
    for _, cell in ipairs(row.cells or {}) do
      fn(cell.contents)
    end
  end
end

local function visit_table(block, fn)
  if block.caption and block.caption.long then
    fn(block.caption.long)
  end
  if block.head then
    each_row(block.head.rows, fn)
  end
  if block.bodies then
    for _, body in ipairs(block.bodies) do
      each_row(body.body, fn)
    end
  end
end

local function rewrite_blocks(blocks, map)
  for _, block in ipairs(blocks) do
    if block.t == "Para" or block.t == "Plain" or block.t == "Header" then
      block.content = rewrite_inlines(block.content, map)
    elseif block.t == "Div" or block.t == "BlockQuote" then
      rewrite_blocks(block.content, map)
    elseif block.t == "Table" then
      visit_table(block, function(inner)
        rewrite_blocks(inner, map)
      end)
    end
  end
end

function Pandoc(doc)
  if not FORMAT:match("docx") then
    return doc
  end
  local after = false
  local n = 0
  local map = {}

  local function consider(text)
    local num = table_number(text)
    if after and num and text:find(":", 1, true) and not map[num] then
      n = n + 1
      map[num] = "S" .. tostring(n)
    end
  end

  local function scan(blocks)
    for _, block in ipairs(blocks) do
      if block.t == "Header" and block.level == 1 and inline_text(block.content):find("Supplement", 1, true) then
        after = true
      elseif block.t == "Div" or block.t == "BlockQuote" then
        scan(block.content)
      elseif block.t == "Table" then
        visit_table(block, function(inner)
          scan(inner)
        end)
      elseif block.t == "Para" or block.t == "Plain" then
        consider(inline_text(block.content))
      end
    end
  end

  scan(doc.blocks)
  rewrite_blocks(doc.blocks, map)
  return doc
end

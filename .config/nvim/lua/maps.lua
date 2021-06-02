local maps = {}

local cmd = vim.cmd  -- to execute Vim commands e.g. cmd('pwd')
local fn = vim.fn    -- to call Vim functions e.g. fn.bufnr()
local g = vim.g      -- a table to access global variables

local scopes = {o = vim.o, b = vim.bo, w = vim.wo}

-- function to ease creation of maps
local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

vim.g.mapleader = ','

---- Comp ----
local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col('.') - 1
    if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
        return true
    else
        return false
    end
end

---- Comp integration with nvim-autopairs ----
require'nvim-autopairs'.setup()
local npairs = require('nvim-autopairs')

-- skip it, if you use another global object
_G.MUtils= {}

vim.g.completion_confirm_key = ""
MUtils.completion_confirm=function()
  if vim.fn.pumvisible() ~= 0  then
    if vim.fn.complete_info()["selected"] ~= -1 then
      return vim.fn["compe#confirm"](npairs.esc("<cr>"))
    else
      return npairs.esc("<cr>")
    end
  else
    return npairs.autopairs_cr()
  end
end

---- Tab/S-Tab Popup Navigation Functions
--- move to prev/next item in completion menuone
--- jump to prev/next snippet's placeholder
_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-n>"
  elseif vim.fn.call("vsnip#available", {1}) == 1 then
    return t "<Plug>(vsnip-expand-or-jump)"
  elseif check_back_space() then
    return t "<Tab>"
  else
    return vim.fn['compe#complete']()
  end
end
_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  elseif vim.fn.call("vsnip#jumpable", {-1}) == 1 then
    return t "<Plug>(vsnip-jump-prev)"
  else
    -- If <S-Tab> is not working in your terminal, change it to <C-h>
    return t "<S-Tab>"
  end
end

function maps.reload()
-- init.lua
map('n', '<leader>sv', ':call v:lua.reload_config()<CR>')
map('n', '<leader>ev', ':tabnew<cr>:lcd ~/.config/nvim/<cr>:NERDTreeFocus<cr>')

-- terminal mode
map('t', '<Esc>', '<C-\\><C-n>')
-- lsp specific mappings
map('n', '<C-Enter>', '<cmd>lua vim.lsp.buf.hover()<CR>', {noremap = true})
map('n', '<A-Enter>', '<cmd>lua vim.lsp.buf.definition()<CR>', {noremap = true})
map('n', '<S-Enter>', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>')

map('n', '<S-F2>', '<cmd>lua vim.lsp.buf.references()<CR>')
map('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<CR>')

-- NerdTree
map('n', '<leader>T', ':NERDTreeToggle<CR>')
map('n', '<leader>t', ':NERDTreeFocus<CR>')

-- Compe Maps
map("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
map("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
map("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
map("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})

map('i', '<C-Space>', 'compe#complete()', {expr = true, silent = true})
map('i' , '<CR>','v:lua.MUtils.completion_confirm()', {expr = true , noremap = true})
map('i', '<C-e>', 'compe#close(\'<C-e>\')', {expr = true, silent = true})
map('i', '<C-f>', 'compe#scroll({ \'delta\': +4 })', {expr = true, silent = true})
map('i', '<C-d>', 'compe#scroll({ \'delta\': -4 })', {expr = true, silent = true})
end

return maps

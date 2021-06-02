local option = {}

function option.reload()
-- completion settings
vim.cmd('syntax enable')
vim.wo.foldmethod = 'syntax'
vim.wo.foldenable = false
vim.wo.foldlevel  = 6

vim.o.completeopt = 'menuone,noinsert,noselect'

vim.wo.number = true
vim.wo.relativenumber = true
vim.o.bg = 'dark'
vim.o.guifont = 'Fira Code Regular:h11'
vim.o.clipboard = 'unnamedplus'
vim.cmd('colorscheme tender')

-- tabs
vim.bo.tabstop = 4
vim.bo.shiftwidth = 4
vim.bo.expandtab = true
vim.g['indentLine_char'] = '┊'
end

return option

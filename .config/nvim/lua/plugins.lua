vim.cmd 'filetype plugin indent on'
vim.cmd 'syntax enable'

vim.cmd 'packadd paq-nvim'
local paq = require('paq-nvim').paq

paq {'savq/paq-nvim', opt = true} -- paq-nvim nanages itself

-- LSP
paq 'nvim-lua/lsp_extensions.nvim'
paq 'neovim/nvim-lspconfig'
paq 'ojroques/nvim-lspfuzzy'
paq 'hrsh7th/nvim-compe'

-- rust-tools
paq 'rust-lang/rust.vim'
paq 'simrat39/rust-tools.nvim'
paq 'nvim-lua/popup.nvim'           -- rust-tools dep
paq 'nvim-lua/plenary.nvim'         -- rust-tools dep
paq 'nvim-telescope/telescope.nvim' -- rust-tools dep

-- Interaction
paq 'scrooloose/nerdtree'
paq 'tpope/vim-obsession'
paq 'hrsh7th/vim-vsnip'
paq 'windwp/nvim-autopairs'
paq 'tpope/vim-surround'

-- Visual Stuff
paq 'itchyny/lightline.vim'
paq 'yggdroot/indentline'

-- Themes
paq 'vim-scripts/AtelierDune'
paq 'morhetz/gruvbox'
paq 'tomasr/molokai'
paq 'NLKNguyen/papercolor-theme'
paq 'jacoborus/tender.vim'

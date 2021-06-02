vim.cmd('filetype plugin indent on')

option = require 'option'  -- visual configs
maps   = require 'maps'    -- keymaps

function _G.reload_config()
    option.reload()
    maps.reload()
end

_G.reload_config()
require 'plugins' -- plugins
require 'lsp'     -- set up language server

local nvim_lsp = require 'lspconfig'

--local lspfuzzy = require 'lspfuzzy'
--lspfuzzy.setup {}


---- Enable Rust Analyzer ----
local capabilities = vim.lsp.protocol.make_client_capabilities()

nvim_lsp.rust_analyzer.setup{
	capabilities = capabilities,
}

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = true,
    signs = true,
    update_in_insert = true,
  }
)

-- Enable type inlay hints
vim.cmd([[
autocmd CursorMoved,InsertLeave,BufEnter,BufWinEnter,TabEnter,BufWritePost * lua require'lsp_extensions'.inlay_hints{ prefix = '', highlight = "Comment", enabled = {"TypeHint", "ChainingHint", "ParameterHint"} }
]])

--  enabled = true;
--  debug = false;
--  min_length = 1;
--  preselect = 'enable';
--  allow_prefix_unmatch = false;

-- Compe
require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = true;
  preselect = 'never';

  source = {
    path = true;
    buffer = true;
    vsnip = true;
    nvim_lsp = true;
  };
}

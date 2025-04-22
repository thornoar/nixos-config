local ht = require('haskell-tools')
local bufnr = vim.api.nvim_get_current_buf()
local opts = { noremap = true, silent = true, buffer = bufnr, }
-- haskell-language-server relies heavily on codeLenses,
vim.keymap.set('n', '<leader>cs', ht.hoogle.hoogle_signature, opts)
vim.keymap.set('n', '<C-CR>', ht.lsp.buf_eval_all, opts)
vim.keymap.set('i', '<C-e>', "-- >>> ")

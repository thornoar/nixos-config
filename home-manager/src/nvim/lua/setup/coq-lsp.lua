-- local handlers =  {
--     ["textDocument/hover"] =  vim.lsp.with(vim.lsp.handlers.hover, { border = border }),
--     ["textDocument/signatureHelp"] =  vim.lsp.with(vim.lsp.handlers.signature_help, { border = border }),
-- }

-- package.path = package.path .. ';'..os.getenv('PROJECTS')..'/coq-lsp.nvim/lua/'
require("coq-lsp").setup({
    lsp = {
        on_attach = function(_, _) end,
        capabilities = require('cmp_nvim_lsp').default_capabilities(),
        -- handlers = handlers,
        root_dir = function (_)
            return vim.loop.cwd()
        end,
        autostart = true,
    }
})

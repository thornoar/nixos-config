-- $lsp
vim.diagnostic.config({
    virtual_text = false
})
local symbols = { Error = "E", Info = "I", Hint = "H", Warn = "W" }
for name, icon in pairs(symbols) do
    local hl = "DiagnosticSign" .. name
    vim.fn.sign_define(hl, { text = icon, numhl = hl, texthl = hl })
end
-- vim.keymap.set('n', '<C-S-d>', function () vim.diagnostic.jump({ count = -1, float = true }) end)
vim.keymap.set('n', '<C-d>', function () vim.diagnostic.jump({ count = 1, float = true }) end)
-- vim.keymap.set('n', '<M-S-d>', function () vim.diagnostic.jump({ count = -1, float = true, severity = vim.diagnostic.severity.ERROR }) end)
vim.keymap.set('n', '<M-d>', function () vim.diagnostic.jump({ count = 1, float = true, severity = vim.diagnostic.severity.ERROR }) end)
vim.api.nvim_create_autocmd('LspAttach', {
    desc = 'LSP actions',
    callback = function(event)
        local opts = { buffer = event.buf }
        vim.keymap.set('n', '<C-Space>', function () vim.lsp.buf.hover({ border = "rounded" }) end, opts)
        vim.keymap.set('n', '<M-C-CR>', function () vim.lsp.buf.references() end, opts)
        vim.keymap.set({ 'n', 'x' }, '<leader>cf', function () vim.lsp.buf.format({ async = true }) end, opts)
        vim.keymap.set('n', '<leader>ca', function () vim.lsp.buf.code_action() end, opts)
    end
})

local lsp_capabilities = require('cmp_nvim_lsp').default_capabilities()
-- local hl_name = "FloatBorder"
-- local border = { '+', '-', '+', '|', '+', '-', '+', '|' }
-- local border = {
--     { "╭", hl_name },
--     { "─", hl_name },
--     { "╮", hl_name },
--     { "│", hl_name },
--     { "╯", hl_name },
--     { "─", hl_name },
--     { "╰", hl_name },
--     { "│", hl_name },
-- }
-- local handlers =  {
--     ["textDocument/hover"] =  vim.lsp.with(vim.lsp.handlers.hover, { border = border }),
--     ["textDocument/signatureHelp"] =  vim.lsp.with(vim.lsp.handlers.signature_help, { border = border }),
--     -- ["textDocument/hover"] =  vim.lsp.handlers.hover,
--     -- ["textDocument/signatureHelp"] =  vim.lsp.handlers.signature_help,
-- }

local lspconfig = require('lspconfig')
local lspbasicconfig = {
    autostart = true,
    capabilities = lsp_capabilities,
    -- handlers = handlers,
    root_dir = function (_)
        return vim.loop.cwd()
    end,
}

-- local configs = require('lspconfig.configs')
-- local util = require 'lspconfig.util'
-- if not configs.asy_ls then
--     configs.asy_ls = {
--         default_config = {
--             cmd = {'asy', '-lsp'},
--             filetypes = {'asy'},
--             root_dir = function(fname)
--                 return util.find_git_ancestor(fname)
--             end,
--             single_file_support = true,
--             settings = {},
--         },
--     }
-- end
-- lspconfig.asy_ls.setup(lspbasicconfig)

vim.diagnostic.config({
    float = { border = "rounded", },
})

lspconfig.texlab.setup(lspbasicconfig)
lspconfig.ts_ls.setup(lspbasicconfig)
lspconfig.pyright.setup(lspbasicconfig)
lspconfig.clojure_lsp.setup(lspbasicconfig)
-- lspconfig.nil_ls.setup(lspbasicconfig)
lspconfig.nixd.setup({
    -- handlers = handlers,
    capabilities = lsp_capabilities,
    cmd = { "nixd" },
    settings = {
        nixd = {
            nixpkgs = {
                expr = "import <nixpkgs { }>",
            },
            formatting = {
                command = { "nixfmt" },
            },
            options = {
                nixos = {
                    expr = "(builtins.getFlake \"/home/ramak/projects/nixos-config\").nixosConfigurations.laptop.options",
                },
            },
        },
    },
})
lspconfig.r_language_server.setup(lspbasicconfig)
-- lspconfig.hls.setup(lspbasicconfig)
lspconfig.clangd.setup({
    autostart = true,
    capabilities = lsp_capabilities,
    -- handlers = handlers,
    root_dir = function (_)
        return vim.loop.cwd()
    end,
})
lspconfig.bashls.setup(lspbasicconfig)
-- lspconfig.coq_lsp.setup(lspbasicconfig)
-- lspconfig.kotlin_language_server.setup({
--     autostart = true,
--     capabilities = lsp_capabilities,
--     handlers = handlers,
--     root_dir = function (_)
--         return vim.loop.cwd()
--     end,
-- })

lspconfig.tinymist.setup({
    -- settings = {
    --     exportPdf = "never",
    -- },
    offset_encoding = "utf-8",
    autostart = true,
    capabilities = lsp_capabilities,
    -- handlers = handlers,
    root_dir = function (_)
        return vim.loop.cwd()
    end,
})

lspconfig.rust_analyzer.setup({
    autostart = true,
    capabilities = lsp_capabilities,
    -- handlers = handlers,
    settings = {
        ['rust-analyzer'] = {
            check = {
                command = "clippy"
            },
            diagnostics = {
                enable = true
            },
        },
    },
})
local rt = require("rust-tools")
rt.setup({
    server = {
        on_attach = function(_, bufnr)
            -- Hover actions
            vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
            -- Code action groups
            vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
        end,
    },
})
rt.inlay_hints.enable()

lspconfig.lua_ls.setup({
    autostart = true,
    capabilities = lsp_capabilities,
    -- handlers = handlers,
    root_dir = function (_)
        return vim.loop.cwd()
    end,
    settings = {
        Lua = {
            diagnostics = {
                globals = { 'vim' }
            }
        }
    }
})

vim.api.nvim_create_user_command('LA', 'LspStart', {})
vim.api.nvim_create_user_command('LD', 'LspStop', {})
local luasnip = require('luasnip')
local cmp_ultisnips_mappings = require("cmp_nvim_ultisnips.mappings")
local cmp = require('cmp')
cmp.setup({
    sources = {
        { name = 'nvim_lsp' },
        { name = 'path' },
        -- { name = 'buffer' },
        { name = 'ultisnips' },
        -- {
        --     name = 'look',
        --     keyword_length = 2,
        --     option = {
        --         convert_case = true,
        --         loud = true,
        --         dict = os.getenv('WORDLIST')
        --     }
        -- },
    },
    window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
        ['<Down>'] = cmp.mapping(function (fallback)
            cmp.close()
            fallback()
        end, { 'i' }),
        ['<Up>'] = cmp.mapping(function (fallback)
            cmp.close()
            fallback()
        end, { 'i' }),
        ['<C-x>'] = cmp.mapping.select_next_item({}),
        ['<C-Down>'] = cmp.mapping.scroll_docs(1),
        ['<C-a>'] = cmp.mapping.select_prev_item({}),
        ['<C-Up>'] = cmp.mapping.scroll_docs(-1),
        ['<CR>'] = cmp.mapping.confirm({ select = false }),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-Right>'] = cmp.mapping(function(fallback)
            if luasnip.locally_jumpable(1) then
                luasnip.jump(1)
            else
                cmp_ultisnips_mappings.expand_or_jump_forwards(fallback)
            end
        end, { 'i', 's' }),
        ['<C-Left>'] = cmp.mapping(function(fallback)
            if luasnip.locally_jumpable(-1) then
                luasnip.jump(-1)
            else
                cmp_ultisnips_mappings.jump_backwards(fallback)
            end
        end, { 'i', 's' }),
    }),
    snippet = {
        expand = function(args)
            -- luasnip.lsp_expand(args.body)
            vim.fn["UltiSnips#Anon"](args.body)
        end,
    },
})

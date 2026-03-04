return {
    "neovim/nvim-lspconfig",
    dependencies = {
        "hrsh7th/cmp-nvim-lsp",
    },
    config = function ()
            -- local myborder = { "|", " ", "|", "|" }
        local myborder = "single"

        vim.diagnostic.config({
            virtual_text = false
        })
        local symbols = { Error = "E", Info = "I", Hint = "H", Warn = "W" }
        for name, icon in pairs(symbols) do
            local hl = "DiagnosticSign" .. name
            vim.diagnostic.config({signs = {
                text = icon,
                texthl = hl,
                numhl = hl,
            }})
        end
        -- vim.keymap.set('n', '<C-S-d>', function () vim.diagnostic.jump({ count = -1, float = true }) end)
        vim.keymap.set('n', '<C-d>', function () vim.diagnostic.jump({ count = 1, float = true }) end)
        -- vim.keymap.set('n', '<M-S-d>', function () vim.diagnostic.jump({ count = -1, float = true, severity = vim.diagnostic.severity.ERROR }) end)
        vim.keymap.set('n', '<M-d>', function () vim.diagnostic.jump({ count = 1, float = true, severity = vim.diagnostic.severity.ERROR }) end)
        vim.api.nvim_create_autocmd('LspAttach', {
            desc = 'LSP actions',
            callback = function(event)
                local opts = { buffer = event.buf }
                vim.keymap.set('n', '<C-Space>', function () vim.lsp.buf.hover({ border = myborder }) end, opts)
                vim.keymap.set('n', '<M-C-CR>', function () vim.lsp.buf.references() end, opts)
                vim.keymap.set({ 'n', 'x' }, '<leader>cf', function () vim.lsp.buf.format({ async = true }) end, opts)
                vim.keymap.set('n', '<leader>ca', function () vim.lsp.buf.code_action() end, opts)
            end
        })

        local lsp_capabilities = require('cmp_nvim_lsp').default_capabilities()

        vim.diagnostic.config({
            float = { border = myborder, },
        })

        vim.lsp.enable("texlab")

        vim.lsp.enable("ts_ls")

        vim.lsp.enable("pyright")

        -- vim.lsp.enable("clojure_lsp")
        -- vim.lsp.enable("nil_ls")

        vim.lsp.config("nixd", {
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
        vim.lsp.enable("nixd")

        vim.lsp.enable("r_language_server")

        -- vim.lsp.enable("hls")

        -- vim.lsp.config("clangd", {
        --     autostart = true,
        --     -- capabilities = lsp_capabilities,
        --     -- handlers = handlers,
        -- })
        vim.lsp.enable("clangd")

        vim.lsp.enable("bashls")

        vim.lsp.enable("asm_lsp")

        vim.lsp.config("tinymist", {
            offset_encoding = "utf-8",
            autostart = true,
        })
        vim.lsp.enable("tinymist")

        -- lspconfig.rust_analyzer.setup({
        --     autostart = true,
        --     capabilities = lsp_capabilities,
        --     -- handlers = handlers,
        --     settings = {
        --         ['rust-analyzer'] = {
        --             check = {
        --                 command = "clippy"
        --             },
        --             diagnostics = {
        --                 enable = true
        --             },
        --         },
        --     },
        -- })
        vim.lsp.enable("rust_analyzer")

        -- local rt = require("rust-tools")
        -- rt.setup({
        --     server = {
        --         on_attach = function(_, bufnr)
        --             -- Hover actions
        --             vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
        --             -- Code action groups
        --             vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
        --         end,
        --     },
        -- })
        -- rt.inlay_hints.enable()

        vim.lsp.config("lua_ls", {
            autostart = true,
            capabilities = lsp_capabilities,
            -- handlers = handlers,
            settings = {
                Lua = {
                    diagnostics = {
                        globals = { 'vim' }
                    }
                }
            }
        })
        vim.lsp.enable("lua_ls")

        vim.api.nvim_create_user_command('LA', 'LspStart', {})
        vim.api.nvim_create_user_command('LD', 'LspStop', {})
    end
}

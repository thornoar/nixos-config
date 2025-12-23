-- $treesitter
require("nvim-treesitter.configs").setup({
    auto_install = false,
    modules = {},
    sync_install = true,
    ignore_install = {},
    ensure_installed = {
        "c", "lua", "python",
        "vimdoc", "vim", "hjson",
        "java", "markdown_inline",
        "typst", "html", "css",
        -- "yaml", "latex"
    },
    highlight = { enable = true },
    indent = { enable = false },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = "<C-space>",
            node_incremental = "<C-space>",
            scope_incremental = "<C-s>",
            node_decremental = "<M-space>",
        },
    },
    textobjects = {
        select = {
            enable = true,
            lookahead = true,
            keymaps = {
                ["af"] = "@function.outer",
                ["if"] = "@function.inner",
                ["ac"] = "@class.outer",
                ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
                ["as"] = { query = "@scope", query_group = "locals", desc = "Select language scope" },
            },
            selection_modes = {
                ["@parameter.outer"] = "v",
                ["@function.outer"] = "V",
                ["@class.outer"] = "<C-v>",
            },
            include_surrounding_whitespace = false,
        },
    },
})

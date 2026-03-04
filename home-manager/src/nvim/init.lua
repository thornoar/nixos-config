vim.loader.enable()

vim.g.mapleader = ";"
vim.g.maplocalleader = ";"
vim.keymap.set("n", "ec", ":e $NIXOS_CONFIG/home-manager/src/nvim/init.lua<CR>")

-- $install
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system {
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable",
        lazypath,
    }
end
vim.opt.rtp:prepend(lazypath)

local lazy_config = {
    defaults = {
        lazy = false,
        version = "*",
    },
    rocks = {
        enabled = false,
    },
    ui = {
        size = { width = 0.8, height = 0.8 },
        border = "single",
        -- title_pos = "right",
        icons = {
            cmd = "> ",
            config = "C ",
            debug = "D ",
            event = "E ",
            favorite = "F ",
            ft = "FT ",
            init = "I ",
            import = "",
            keys = "K ",
            lazy = "L ",
            loaded = "+",
            not_loaded = "-",
            plugin = "<- ",
            runtime = "RT ",
            require = "! ",
            source = "<- ",
            start = "",
            task = "✔ ",
            list = {
                "+",
                "->",
                "*",
                "-",
            },
        },
    },
}

require("lazy").setup({
    "tpope/vim-rhubarb",
    "tpope/vim-surround",
    "tpope/vim-repeat",
    "tpope/vim-fugitive",
    "sagarrakshe/toggle-bool",
    "farmergreg/vim-lastplace",
    "sirver/ultisnips",
    "neovimhaskell/haskell-vim",
    "ap/vim-css-color",
    "nanozuki/tabby.nvim",
    "hjson/vim-hjson",
    "dkarter/bullets.vim",
    "chrisbra/csv.vim",
    "JuliaEditorSupport/julia-vim",
    "mbbill/undotree",
    "whonore/Coqtail",
    "bfrg/vim-c-cpp-modern",
    "nvim-tree/nvim-web-devicons",
    "kaarmu/typst.vim",
    "nvim-lualine/lualine.nvim",
    "navarasu/onedark.nvim",
    require("setup.gitsigns"),
    require("setup.urlview"),
    require("setup.comment"),
    require("setup.metals"),
    require("setup.focus"),
    require("setup.trouble"),
    require("setup.telescope"),
    require("setup.copilot"),
    require("setup.ultimate-autopair"),
    require("setup.ibl"),
    require("setup.treesitter"),
    require("setup.lsp"),
    require("setup.cmp"),
    require("setup.neodev"),
    require("setup.ollama"),
    require("setup.haskell-tools"),
    require("setup.twilight"),
    require("setup.nvim-biscuits"),
    require("setup.neoscroll"),
}, lazy_config)

require("setup.commands")
require("setup.keymaps")
require("setup.settings")
require("setup.coloring")

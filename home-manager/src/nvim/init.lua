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
    "numToStr/Comment.nvim",
    "ap/vim-css-color",
    "nanozuki/tabby.nvim",
    "lewis6991/gitsigns.nvim",
    "hjson/vim-hjson",
    "dkarter/bullets.vim",
    "chrisbra/csv.vim",
    "JuliaEditorSupport/julia-vim",
    "mbbill/undotree",
    "whonore/Coqtail",
    "bfrg/vim-c-cpp-modern",
    "nvim-tree/nvim-web-devicons",
    "axieax/urlview.nvim",
    {
        "scalameta/nvim-metals",
        ft = { "scala", "sbt", "java" },
        opts = function()
            local metals_config = require("metals").bare_config()
            metals_config.settings = {
                fallbackScalaVersion = "3.3.5";
                useGlobalExecutable = true,
            }

            return metals_config
        end,
        config = function(self, metals_config)
            local nvim_metals_group = vim.api.nvim_create_augroup("nvim-metals", { clear = true })
            vim.api.nvim_create_autocmd("FileType", {
                pattern = self.ft,
                callback = function()
                    require("metals").initialize_or_attach(metals_config)
                end,
                group = nvim_metals_group,
            })
        end
    },
    {
        "cdmill/focus.nvim",
        cmd = { "Focus", "Zen", "Narrow" },
        opts = {
            border = "none",
            window = {
                width = 80,
            },
            auto_zen = true,
            zen = {
                diagnostics = true,
            },
        }
    },
    {
        "folke/trouble.nvim",
        opts = {
            icons = {
                indent = {
                    top           = "│ ",
                    middle        = "├╴",
                    last          = "└╴",
                    fold_open     = " +",
                    fold_closed   = "- ",
                    ws            = " ",
                    file          = "",
                },
                folder_closed   = "",
                folder_open     = "",
                kinds = {
                    Array         = "A ",
                    Boolean       = "B ",
                    Class         = "",
                    Constant      = "",
                    Constructor   = "C ",
                    Enum          = "E ",
                    EnumMember    = "",
                    Event         = "",
                    Field         = "",
                    File          = "",
                    Function      = "F ",
                    Interface     = "I ",
                    Key           = "K ",
                    Method        = "",
                    Module        = "M ",
                    Namespace     = "N ",
                    Null          = "",
                    Number        = "",
                    Object        = "",
                    Operator      = "O ",
                    Package       = "P ",
                    Property      = "",
                    String        = "S ",
                    Struct        = "",
                    TypeParameter = "TP ",
                    Variable      = "V ",
                },
            },
        },
        -- cmd = "Trouble",
        keys = {
            {
                "<C-M-d>",
                "<cmd>Trouble diagnostics toggle<cr>",
                desc = "Diagnostics (Trouble)",
            },
        },
        lazy = false,
    },
    {
        "nvim-lualine/lualine.nvim",
    },
    {
        "github/copilot.vim",
        cmd = "Copilot"
    },
    {
        "kaarmu/typst.vim",
    },
    {
        "nvim-telescope/telescope.nvim", tag = "0.1.6",
        dependencies = {
            "nvim-lua/plenary.nvim",
            { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
        },
    },
    {
        "davvid/telescope-git-grep.nvim",
        dependencies = { "nvim-telescope/telescope.nvim" }
    },
    {
        "altermo/ultimate-autopair.nvim",
        -- event = { "InsertEnter" },
        branch = "v0.6",
        opts = {
            space2 = { enable = true },
            tabout = { enable = true },
            fastwarp = {
                enable = true,
                enable_normal = true,
                enable_reverse = true,
                hopout = false,
                faster = false,
                map = "<M-/>",
            },
            internal_pairs = {-- *ultimate-autopair-pairs-default-pairs*
                {'[',']',fly=true,dosuround=false,newline=true,space=true},
                {'(',')',fly=true,dosuround=false,newline=true,space=true},
                {'<','>',fly=true,dosuround=false,newline=true,space=false, ft = {"html","markdown"}},
                {'{','}',fly=true,dosuround=false,newline=true,space=true},
                {'"','"',suround=false,multiline=false},
                {'`','`', nft={'tex'},multiline=false},
                {'``',"''",ft={'tex'}},
                {'```','```',newline=true,ft={'markdown'}},
                {'<!--','-->',ft={'markdown','html'},space=true},
                {'"""','"""',newline=true,ft={'python'}},
                {"'''","'''",newline=true,ft={'python'}},
            },
        },
    },
    {
        "navarasu/onedark.nvim",
        priority = 1000,
    },
    {
        "lukas-reineke/indent-blankline.nvim",
        -- version = "3.5.4",
        main = "ibl",
        opts = {},
    },
    {
        "nvim-treesitter/nvim-treesitter",
        dependencies = {
            "nvim-treesitter/nvim-treesitter-textobjects",
        },
        build = ":TSUpdate",
    },
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            "hrsh7th/cmp-nvim-lsp",
        },
    },
    -- { "simrat39/rust-tools.nvim" },
    {
        "hrsh7th/nvim-cmp",
    },
    {
        "quangnguyen30192/cmp-nvim-ultisnips",
        config = function()
            require("cmp_nvim_ultisnips").setup({})
        end,
    },
    { 'hrsh7th/cmp-buffer', },
    { 'hrsh7th/cmp-path', },
    { 'octaltree/cmp-look' },
    { 'folke/neodev.nvim', opts = {} },
    {
        "nomnivore/ollama.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim",
        },
        cmd = { "Ollama", "OllamaModel" },
        keys = { },
        opts = {
            model = "codegemma:7b",
        },
    },
    {
        "olimorris/codecompanion.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "nvim-treesitter/nvim-treesitter",
        },
    },
    {
        "mrcjkb/haskell-tools.nvim",
        commit = "52f1438",
        -- version = "^6", -- Recommended
        lazy = false, -- This plugin is already lazy
    },
    {
        "folke/twilight.nvim",
        opts = {
            -- your configuration comes here
            -- or leave it empty to use the default settings
            -- refer to the configuration section below
        }
    },
    {
        "code-biscuits/nvim-biscuits",
        requires = {
            "nvim-treesitter/nvim-treesitter",
            -- run = ":TSUpdate"
        },
    },
    {
        "karb94/neoscroll.nvim",
        opts = {}
    },
}, lazy_config)

require("setup.codecompanion")
require("setup.neodev")
-- require("setup.oil")
require("setup.commands")
-- require("setup.coq-lsp")
require("setup.keymaps")
require("setup.telescope")
require("setup.comment")
require("setup.gitsigns")
require("setup.treesitter")
require("setup.ibl")
require("setup.lsp")
require("setup.settings")
require("setup.coloring")
require("setup.twilight")
require("setup.nvim-biscuits")
require("setup.urlview")
require("setup.neoscroll")
-- require("setup.nvim-autopairs")
-- require("setup.render-markdown")
-- require("setup.noice")
-- require("setup.nvim-subfiles")

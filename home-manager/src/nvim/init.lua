vim.loader.enable()

local km = vim.keymap
vim.g.mapleader = ";"
vim.g.maplocalleader = ";"
km.set("n", "ec", ":e $NIXOS_CONFIG/home-manager/src/nvim/init.lua<CR>")

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
require("lazy").setup({
    defaults = { lazy = true },
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
    -- "nvim-tree/nvim-web-devicons",
    "axieax/urlview.nvim",
    -- "smithbm2316/centerpad.nvim",
    {
        "thornoar/coq-lsp.nvim",
        branch = "add-configuration",
    },
    -- {
    --     "folke/noice.nvim",
    --     event = "VeryLazy",
    --     opts = {
    --         -- add any options here
    --     },
    --     dependencies = {
    --         -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
    --         "MunifTanjim/nui.nvim",
    --         -- OPTIONAL:
    --         --   `nvim-notify` is only needed, if you want to use the notification view.
    --         --   If not available, we use `mini` as the fallback
    --         -- "rcarriga/nvim-notify",
    --     }
    -- },
    {
        "scalameta/nvim-metals",
        ft = { "scala", "sbt", "java" },
        opts = function()
            local metals_config = require("metals").bare_config()
            metals_config.settings = {
                fallbackScalaVersion = "3.3.5";
                -- verboseCompilation = true,
                -- inlayHints = {
                --     byNameParameters = { enable = true },
                --     hintsInPatternMatch = { enable = true },
                --     implicitArguments = { enable = true },
                --     implicitConversions = { enable = true },
                --     inferredTypes = { enable = true },
                --     typeParameters = { enable = true },
                --     javaProperties = {
                --         '-Dmetals.showMessage=off',
                --     },
                -- },
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
    -- {
    --     'stevearc/oil.nvim',
    --     ---@module 'oil'
    --     ---@type oil.SetupOpts
    --     opts = {},
    --     -- Optional dependencies
    --     -- dependencies = { { "echasnovski/mini.icons", opts = {} } },
    --     -- dependencies = { "nvim-tree/nvim-web-devicons" }, -- use if prefer nvim-web-devicons
    -- },

    -- {
    --     "lervag/vimtex",
    --     lazy = false,     -- we don't want to lazy load VimTeX
    --     -- tag = "v2.15", -- uncomment to pin to a specific release
    --     init = function()
    --         -- VimTeX configuration goes here, e.g.
    --         vim.g.vimtex_view_method = "zathura"
    --     end
    -- },
    {
        "cdmill/focus.nvim",
        cmd = { "Focus", "Zen", "Narrow" },
        opts = {
            border = "none",
            window = {
                width = 80,
                -- options = {
                --     border = "none",
                -- },
            },
            auto_zen = true,
            zen = {
                diagnostics = true,
            },
        }
    },
    {
        "folke/trouble.nvim",
        opts = {}, -- for default options, refer to the configuration section for custom setup.
        cmd = "Trouble",
        keys = {
            {
                "<C-M-d>",
                "<cmd>Trouble diagnostics toggle<cr>",
                desc = "Diagnostics (Trouble)",
            },
            -- {
            --     "<>",
            --     "<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
            --     desc = "Buffer Diagnostics (Trouble)",
            -- },
        },
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
        ft = "typst",
        lazy = false,
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
        event = { "InsertEnter","CmdlineEnter" },
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
        internal_pairs={-- *ultimate-autopair-pairs-default-pairs*
            {'[',']',fly=true,dosuround=true,newline=true,space=true},
            {'(',')',fly=true,dosuround=true,newline=true,space=true},
            {'<','>',fly=true,dosuround=true,newline=true,space=false, ft = {"html","markdown"}},
            {'{','}',fly=true,dosuround=true,newline=true,space=true},
            {'"','"',suround=true,multiline=false},
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
        version = "3.5.4",
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
    { "simrat39/rust-tools.nvim" },
    {
        "hrsh7th/nvim-cmp",
        dependencies = {
            "L3MON4D3/LuaSnip",
        },
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
        ---@type Ollama.Config
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
        "stevearc/dressing.nvim",
        opts = {},
    },
    {
        "kdheepak/lazygit.nvim",
        -- optional for floating window border decoration
        dependencies = {
            "nvim-lua/plenary.nvim",
            "nvim-telescope/telescope.nvim",
        },
        config = function()
            require("telescope").load_extension("lazygit")
        end,
    },
    {
        "mrcjkb/haskell-tools.nvim",
        version = "^6", -- Recommended
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
    -- 'thornoar/nvim-subfiles',
}, {})

require("setup.codecompanion")
require("setup.neodev")
-- require("setup.oil")
require("setup.commands")
require("setup.coq-lsp")
require("setup.keymaps")
require("setup.telescope")
require("setup.comment")
require("setup.gitsigns")
require("setup.treesitter")
require("setup.ibl")
require("setup.lsp")
require("setup.settings")
require("setup.coloring")
require("setup.spell")
require("setup.twilight")
require("setup.nvim-biscuits")
require("setup.urlview")
-- require("setup.noice")
-- require("setup.nvim-subfiles")

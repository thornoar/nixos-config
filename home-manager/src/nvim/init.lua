vim.loader.enable()

local km = vim.keymap
vim.g.mapleader = ';'
km.set('n', 'ec', ':e $NIXOS_CONFIG/home-manager/src/nvim/init.lua<CR>')

-- $install
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system {
        'git',
        'clone',
        '--filter=blob:none',
        'https://github.com/folke/lazy.nvim.git',
        '--branch=stable',
        lazypath,
    }
end
vim.opt.rtp:prepend(lazypath)
require('lazy').setup({
    defaults = { lazy = true },
    'tpope/vim-rhubarb',
    'tpope/vim-surround',
    'tpope/vim-repeat',
    'tpope/vim-fugitive',
    'sagarrakshe/toggle-bool',
    'farmergreg/vim-lastplace',
    'sirver/ultisnips',
    'neovimhaskell/haskell-vim',
    'numToStr/Comment.nvim',
    'ap/vim-css-color',
    'nanozuki/tabby.nvim',
    'lewis6991/gitsigns.nvim',
    'hjson/vim-hjson',
    'dkarter/bullets.vim',
    'chrisbra/csv.vim',
    'JuliaEditorSupport/julia-vim',
    {
        "folke/trouble.nvim",
        opts = {}, -- for default options, refer to the configuration section for custom setup.
        cmd = "Trouble",
        keys = {
            {
                "<M-S-d>",
                "<cmd>Trouble diagnostics toggle<cr>",
                desc = "Diagnostics (Trouble)",
            },
            {
                "<M-d>",
                "<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
                desc = "Buffer Diagnostics (Trouble)",
            },
        },
    },
    {
        'nvim-lualine/lualine.nvim',
    },
    {
        'github/copilot.vim',
        cmd = 'Copilot'
    },
    'mbbill/undotree',
    { 'akinsho/toggleterm.nvim', version = '*', config = true },
    {
        'kdheepak/lazygit.nvim',
        dependencies = {
            'nvim-lua/plenary.nvim',
        },
        keys = {
            { '<M-g>', ':LazyGit<CR>', desc = 'LazyGit' }
        },
    },
    {
        'kaarmu/typst.vim',
        ft = 'typst',
        lazy = false,
    },
    {
        'nvim-telescope/telescope.nvim', tag = '0.1.6',
        dependencies = { 'nvim-lua/plenary.nvim' }
    },
    {
        'davvid/telescope-git-grep.nvim',
        dependencies = { "nvim-telescope/telescope.nvim" }
    },
    {
        'altermo/ultimate-autopair.nvim',
        event = { 'InsertEnter','CmdlineEnter' },
        branch = 'v0.6',
        opts = {
            space2 = { enable = true },
            tabout = { enable = true },
            fastwarp = {
                enable = true,
                enable_normal = true,
                enable_reverse = true,
                hopout = false,
                faster = false,
                map = '<M-/>',
        },
        internal_pairs={-- *ultimate-autopair-pairs-default-pairs*
            {'[',']',fly=true,dosuround=true,newline=true,space=true},
            {'(',')',fly=true,dosuround=true,newline=true,space=true},
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
        'navarasu/onedark.nvim',
        priority = 1000,
    },
    {
        'lukas-reineke/indent-blankline.nvim',
        version = '3.5.4',
        main = 'ibl',
        opts = {},
    },
    {
        'nvim-treesitter/nvim-treesitter',
        dependencies = {
            'nvim-treesitter/nvim-treesitter-textobjects',
        },
        build = ':TSUpdate',
    },
    {
        'neovim/nvim-lspconfig',
        dependencies = {
            'hrsh7th/cmp-nvim-lsp',
        },
    },
    { 'simrat39/rust-tools.nvim' },
    {
        'hrsh7th/nvim-cmp',
        dependencies = {
            'L3MON4D3/LuaSnip',
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
    { 'https://github.com/octaltree/cmp-look' },
    { 'folke/neodev.nvim', opts = {} },
    -- 'thornoar/nvim-subfiles',
}, {})

require("setup.neodev")
require("setup.commands")
require("setup.keymaps")
require("setup.telescope")
require("setup.comment")
require("setup.gitsigns")
require("setup.treesitter")
require("setup.toggleterm")
require("setup.ibl")
require("setup.lsp")
require("setup.settings")
require("setup.coloring")

package.path = package.path .. ';'..os.getenv('PROJECTS')..'/nvim-subfiles/lua/?.lua'
require('nvim-subfiles').setup({
    bindings = { },
    opts = {
        jump_to_file = false,
    }
})

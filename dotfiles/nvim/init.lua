--[[--
 
   ▄███████▄                  ▄██████▄      ▄██████▄      ▄██████▄      ▄██████▄      ▄██████▄
 ▄█████████▀▀               ▄█▀████▀███▄  ▄██████████▄  ▄██████████▄  ▄██████████▄  ▄███▀████▀█▄
 ███████▀      ▄▄  ▄▄  ▄▄   █▄▄███▄▄████  ███ ████ ███  ███ ████ ███  ███ ████ ███  ████▄▄███▄▄█
 ███████▄      ▀▀  ▀▀  ▀▀   ████████████  ████████████  ████████████  ████████████  ████████████
 ▀█████████▄▄               ██▀██▀▀██▀██  ██▀██▀▀██▀██  ██▀██▀▀██▀██  ██▀██▀▀██▀██  ██▀██▀▀██▀██
   ▀███████▀                ▀   ▀  ▀   ▀  ▀   ▀  ▀   ▀  ▀   ▀  ▀   ▀  ▀   ▀  ▀   ▀  ▀   ▀  ▀   ▀

   ▄███████▄                  ▄██████▄      ▄██████▄      ▄██████▄      ▄██████▄      ▄██████▄
 ▄█████████▀▀               ▄█▀████▀███▄  ▄██ ████ ██▄  ▄██ ████ ██▄  ▄██ ████ ██▄  ▄███▀████▀█▄
 ███████▀      ▄▄  ▄▄  ▄▄   █▄▄███▄▄████  ████████████  ████████████  ████████████  ████▄▄███▄▄█
 ███████▄      ▀▀  ▀▀  ▀▀   ████████████  ████████████  ████████████  ████████████  ████████████
 ▀█████████▄▄               ██▀██▀▀██▀██  ██▀██▀▀██▀██  ██▀██▀▀██▀██  ██▀██▀▀██▀██  ██▀██▀▀██▀██
   ▀███████▀                ▀   ▀  ▀   ▀  ▀   ▀  ▀   ▀  ▀   ▀  ▀   ▀  ▀   ▀  ▀   ▀  ▀   ▀  ▀   ▀

--]]--

-- // VARIABLES // --

require('globals')

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.g.mapleader = ';'

km.set('n', 'ec', ':e ~/.config/nvim/init.lua<CR>')

-- // PLUGINS // --

local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system {
		'git',
		'clone',
		'--filter=blob:none',
		'https://github.com/folke/lazy.nvim.git',
		'--branch=stable', -- latest stable release
		lazypath,
	}
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup({
	'tpope/vim-fugitive',
	'tpope/vim-rhubarb',
	'tpope/vim-surround',
	'nanozuki/tabby.nvim',
	'lervag/vimtex',
	'farmergreg/vim-lastplace',
	-- 'Xe/lolcode.vim',
	'sirver/ultisnips',
	'neovimhaskell/haskell-vim',
	'nvim-lualine/lualine.nvim',
	'archibate/lualine-time',
	'dkarter/bullets.vim',
	-- 'cljoly/telescope-repo.nvim',
	-- 'folke/zen-mode.nvim',
	'numToStr/Comment.nvim',
    'lewis6991/gitsigns.nvim',
	-- 'serenevoid/kiwi.nvim',
    'nvim-tree/nvim-web-devicons',

	-- {
	-- 	'windwp/nvim-autopairs',
	-- 	event = 'InsertEnter',
	-- 	opts = {}
	-- },

    {
        'altermo/ultimate-autopair.nvim',
        event={'InsertEnter','CmdlineEnter'},
        branch='v0.6', --recomended as each new version will have breaking changes
        opts={
            --Config goes here
        },
    },

	{
		'navarasu/onedark.nvim',
		priority = 1000,
	},

	{
		'lukas-reineke/indent-blankline.nvim',
		main = 'ibl',
		opts = {},
	},

	{
		'nvim-telescope/telescope-file-browser.nvim',
		dependencies = { 'nvim-telescope/telescope.nvim', 'nvim-lua/plenary.nvim' }
	},

	{
		'AckslD/nvim-neoclip.lua',
		config = function() require('neoclip').setup() end,
	},

	{ 'nvim-telescope/telescope.nvim', branch = '0.1.x', dependencies = { 'nvim-lua/plenary.nvim' } },

	{
		'nvim-treesitter/nvim-treesitter',
		dependencies = {
			'nvim-treesitter/nvim-treesitter-textobjects',
		},
		build = ':TSUpdate',
	},

   -- 'thornoar/nvim-subfiles',
}, {})

-- // SETUP // --

require('gitsigns').setup()

require('commands')
require('keymaps')

require('telescope_setup')
require('remainder')

require('settings')

-- package.path = package.path .. ';'..home..'/programming/nvim-subfiles/lua/?.lua'
-- require('nvim-subfiles').setup({
--   ['subfile'] = 'SF',
--   ['subfigure'] = 'CF',
-- })
-- require('nvim-subfiles').setup({})

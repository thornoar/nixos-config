-- VARIABLES --
km = vim.keymap
km.set('n', 'ec', ':e $NIXOS_CONFIG/dotfiles/nvim/init.lua<CR>')
newcmd = function (name, command)
	vim.api.nvim_create_user_command(name, command, {})
end
P = function (v)
    print(vim.inspect(v))
    return v
end
RELOAD = function (...)
    return require("plenary.reload").reload_module(...)
end
R = function (name)
    RELOAD(name)
    return require(name)
end
home = os.getenv('PROJECTS')
testdir = home..'/sandbox'
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.g.mapleader = ';'

-- INSTALL --
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
	-- 'archibate/lualine-time',
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

require('gitsigns').setup()

-- COMMANDS --
local emptycompile = 'echo \"not set to compile\"';
local defaultoutputname = 'output'
local compilecmd = {
	['asy'] = 'asy -nosafe -noV',
	['r'] = 'Rscript',
	['py'] = 'python',
	['c'] = 'gcc',
	['cpp'] = 'g++ -o cpp.out',
    ['rust'] = 'rustc -o rust.out',
	['hs'] = 'runhaskell',
	['tex'] = 'latexmk -g -pdf',
	['lua'] = 'lua',
	['lol'] = 'lci',
	['java'] = 'javac',
    ['pdf'] = 'zathura',
    ['nix'] = 'nix eval --file',
}
local compilefunc = {
	['asy'] = function (name) return ('!asy -noV -nosafe ' .. name) end,
	['r'] = function (name) return ('!Rscript ' .. name) end,
	['python'] = function (name) return ('!python ' .. name) end,
	['c'] = function (name) return ('!gcc ' .. name .. ' && ./a.out') end,
	['cpp'] = function (name) return ('!g++ -Wall ' .. name .. ' -o cpp.out && ./cpp.out') end,
    ['rust'] = function (name) return ('!rustc ' .. name .. ' -o rust.out && ./rust.out') end,
	['haskell'] = function (name) return ('!runhaskell ' .. name) end,
	['tex'] = function (name) return ('!latexmk -g -pdf ' .. name) end,
	['lua'] = function (name) return ('!lua ' .. name) end,
	['lolcode'] = function (name) return ('!lci ' .. name) end,
	['java'] = function (name) return ('!javac ' .. name .. ' && java Main') end,
    ['pdf'] = function (name) return ('!nohup zathura ' .. name .. '&') end,
    ['nix'] = function (name) return ('!nix eval --file ' .. name) end,
}
newcmd("AS", function() autosave = not autosave end)
newcmd('C', function () vim.cmd('tabclose') end)
newcmd('Compile', function () 
	local ccmd = compilefunc[vim.bo.filetype]
	vim.cmd(not ccmd and emptycompile or ccmd('%:t'))
end)
newcmd('CompileSilent', function () 
	local ccmd = compilefunc[vim.bo.filetype]
	vim.cmd(not ccmd and '' or 'silent '..ccmd('%:t'))
end)
vim.api.nvim_create_user_command('BC', function (ext)
	local cmd = compilecmd[ext['args']]
	if not cmd then
		print('invalid file extension provided')
	else
		vim.cmd('!bulkcompile '..ext['args']..' '..'\"'..cmd..'\"')
	end
end, { nargs='?' })
local autocompile = false
local autocompilepattern = { '*.asy', '*.cpp', '*.tex', '*.hs' }
newcmd('AC', function ()
	autocompile = not autocompile
	print('autocompile: '..tostring(autocompile))
end)
vim.api.nvim_create_autocmd({ 'TextChanged', 'TextChangedI' }, {
	pattern = autocompilepattern,
	callback = function ()
		if autocompile then vim.cmd('CompileSilent') end
	end
})
newcmd('ViewPdf', function ()
	if io.open(vim.fn.expand('%:r')..'.pdf', 'r') ~= nil then
		vim.cmd('silent !zathura %:r.pdf&')
	elseif io.open(defaultoutputname..'.pdf', 'r') ~= nil then
		vim.cmd('silent !$READER '..defaultoutputname..'.pdf&')
	else
		vim.cmd('silent !pdfviewall')
	end
end)
newcmd('E', function () vim.bo.keymap = '' end)
newcmd('R', function () vim.bo.keymap = 'russian-jcuken' end)
newcmd('J', function () vim.bo.keymap = 'kana' end)
newcmd('G', function () vim.bo.keymap = 'german-qwertz' end)
newcmd('L', function () vim.cmd('Lazy') end)
newcmd('S', function () vim.wo.spell = not vim.wo.spell end)
newcmd('NS', function () vim.cmd('set nospell') end)
-- Autocommands
autosave = true
autosavepattern = { '*.tex', '*.asy', '*.md', '*.lua', '*.cpp', '*.py', '*.hs', '*.txt', '*.lol', '*.r', '*.snippets', '*.java', '*.nix' }
vim.api.nvim_create_autocmd({ 'TextChanged', 'TextChangedI', 'TextChangedP' }, {
	pattern = autosavepattern,
	callback = function()
		if autosave then vim.cmd('silent write') end
	end
})
vim.api.nvim_create_autocmd({'BufRead', 'BufNewFile'}, {
	pattern = {'*.md', '*.txt'},
	command = 'setlocal spell! spelllang=en_us'
})


-- KEYMAPS --
-- $text keymaps
km.set('v', '<S-Left>', 'ygv<Esc>')
km.set('n', '<S-Left>', 'yy')
km.set('v', '<C-S-Left>', '\"+y')
km.set('n', '<C-S-Left>', '\"+yy')
km.set({'n', 'v'}, '<S-Right>', 'p')
km.set({'n', 'v'}, '<C-S-Right>', '\"+p')
km.set('v', '<C-S-Down>', ":m '>+1<CR>gv=gv")
km.set('v', '<C-S-Up>', ":m '<-2<CR>gv=gv")
km.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })
km.set('v', 'z', '<esc>')
km.set('v', '<S-Up>', '<Up>')
km.set('v', '<S-Down>', '<Down>')
km.set('n', '<S-Down>', '<S-v>j')
km.set('n', '<S-Up>', '<S-v>k')
km.set('v', '<leader>a', ':s/\\d\\+/\\=(submatch(0)+1)/g')
km.set('n', '<leader>cw', ":%s/\\<<C-r><C-w>\\>/")
km.set('n', 'cw', 'ciw')
km.set('n', 'dw', 'diw')
km.set('x', '<leader>p', '\"_dP')
km.set('n', '<leader>f', 'zf%')
km.set('n', '<C-End>', 'k<S-v>jj<S-j>')
km.set('v', '>', '>gv')
km.set('v', '<', '<gv')
km.set('n', '>', '>>')
km.set('n', '<', '<<')
-- $insert keymaps
km.set('n', 'x', 'i')
km.set('i', '<M-z>', '<C-n>')
km.set('i', '<C-z>', '<C-o>:R<CR>')
km.set('i', '<C-x>', '<C-o>:E<CR>')
km.set('i', '<M-s>', '<C-o>$;')
km.set('i', '<C-q>', '<Esc>[s1z=A')
km.set('i', '<C-w>', function () vim.cmd('silent write') end)
-- $navigation keymaps
-- km.set('n', '<Up>', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
-- km.set('n', '<Down>', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
km.set('n', '<Up>', 'gk')
km.set('n', '<Down>', 'gj')
km.set('i', '<Up>', '<C-o>gk')
km.set('i', '<Down>', '<C-o>gj')
km.set({'n', 'v'}, '<M-Up>', '10k')
km.set({'n', 'v'}, '<M-Down>', '10j')
km.set({'n', 'v'}, '<M-Left>', 'b')
km.set({'n', 'v'}, '<M-Right>', 'w')
km.set('i', '<M-up>', '<C-o>10k')
km.set('i', '<M-down>', '<C-o>10j')
km.set('i', '<M-left>', '<C-o>b')
km.set('i', '<M-right>', '<C-o>w')
km.set({'n', 'v'}, '<M-a>', '%')
km.set('n', 'n', 'nzz')
km.set('n', 'N', 'Nzz')   
km.set('n', '<C-M-left>', '<C-^>')
km.set('n', '<C-M-right>', 'gf')
-- $window keymaps
km.set('n', '<C-left>', '<C-w>h')
km.set('n', '<C-Down>', '<C-w>j')
km.set('n', '<C-Up>', '<C-w>k')
km.set('n', '<C-right>', '<C-w>l')
-- km.set('i', '<C-Left>', '<C-o><C-w>h')
-- km.set('i', '<C-Down>', '<C-o><C-w>j')
-- km.set('i', '<C-Up>', '<C-o><C-w>k')
-- km.set('i', '<C-Right>', '<C-o><C-w>l')
km.set('n', '<C-w><Left>', '<C-w>H')
km.set('n', '<C-w><Right>', '<C-w>L')
km.set('n', '<C-w><Up>', '<C-w>K')
km.set('n', '<C-w><Down>', '<C-w>J')
km.set('n', '<C-M-o>', '<C-w>+')
km.set('n', '<C-M-l>', '<C-w>-')
km.set('n', '<C-M-i>', '<C-w>>')
km.set('n', '<C-M-u>', '<C-w><')
km.set('n', '<C-M-k>', '<C-w>=')
-- $command keymaps
km.set('n', 'Z', function () vim.cmd('quit') end)
km.set('n', '<leader>tr', function () vim.cmd('silent !alacritty -e ranger&') end)
km.set('n', '<C-a>', function () vim.cmd('silent !alacritty&') end)
km.set('n', '<leader>l', function () vim.cmd('tabnew $NIXOS_CONFIG/home-ramak.nix') end)
km.set('n', '<leader>k', function () vim.cmd('edit $NIXOS_CONFIG/home-ramak.nix') end)
km.set('n', '<leader>L', function () vim.cmd('tabnew $NIXOS_CONFIG/dotfiles/nvim/init.lua') end)
km.set('n', '<leader>K', function () vim.cmd('edit $NIXOS_CONFIG/dotfiles/nvim/init.lua') end)
km.set('n', '<leader>o', ':Compile<CR>')
km.set('n', '<leader>x', function () vim.cmd('CompileSilent') end)
km.set({'n','i'}, '<C-s>', function () vim.cmd('CompileSilent') end)
km.set('n', '<leader>vp', function () vim.cmd('ViewPdf') end)
km.set('n', '<leader>ee', function () vim.cmd('sp $NIXOS_CONFIG/dotfiles/nvim/UltiSnips/%:e.snippets') end)
km.set('n', 'd[', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
km.set('n', 'd]', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
km.set('n', '<leader>de', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
km.set('n', '<leader>dq', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })
-- $telescope keymaps
km.set('n', '<C-x>', require('telescope.builtin').buffers, { desc = '[ ] Find existing buffers' })
km.set('n', '<C-_>', function()
	require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
		previewer = false,
		theme = 'ivy',
	})
end)
km.set('n', '<M-/>', require('telescope.builtin').live_grep)
km.set('n', '<leader>cd', require('telescope.builtin').builtin)
km.set('n', '<leader>:', require('telescope.builtin').commands)
km.set('n', '<M-S-Right>', require('telescope').extensions.neoclip['a'])
km.set('n', '<C-x>', function () require('telescope').extensions.file_browser.file_browser({ path = vim.loop.cwd() }) end)
km.set('n', '<C-f>', require('telescope.builtin').find_files)
km.set('n', '<C-d>', require('telescope.builtin').oldfiles)
km.set('n', '<M-CR>', require('telescope.builtin').help_tags)
km.set('n', '<leader>s', require('telescope.builtin').grep_string)
km.set('n', '<leader>q', require('telescope.builtin').quickfix)
km.set('n', '<leader>cs', require('telescope.builtin').colorscheme)
km.set('n', '<C-j>', require('telescope.builtin').jumplist)
km.set('n', '<C-z>', 'u')

-- TELESCOPE --
local state = require("telescope.state")
local action_state = require("telescope.actions.state")
local slow_scroll = function(prompt_bufnr, direction)
	local previewer = action_state.get_current_picker(prompt_bufnr).previewer
	local status = state.get_status(prompt_bufnr)

	-- Check if we actually have a previewer and a preview window
	if type(previewer) ~= "table" or previewer.scroll_fn == nil or status.preview_win == nil then
		return
	end

	previewer:scroll_fn(1 * direction)
end
local fb_actions = require "telescope._extensions.file_browser.actions"
require('telescope').setup {
	extensions = {
		file_browser = {
			theme = 'ivy',
			path = vim.loop.cwd(),
			cwd = vim.loop.cwd(),
			cwd_to_path = true,
			grouped = true,
			files = true,
			add_dirs = true,
			depth = 1,
			auto_depth = false,
			select_buffer = false,
			hidden = { file_browser = false, folder_browser = false },
			respect_gitignore = vim.fn.executable "fd" == 1,
			no_ignore = false,
			follow_symlinks = true,
			browse_files = require("telescope._extensions.file_browser.finders").browse_files,
			browse_folders = require("telescope._extensions.file_browser.finders").browse_folders,
			hide_parent_dir = false,
			collapse_dirs = false,
			prompt_path = false,
			quiet = false,
			dir_icon = "",
			dir_icon_hl = "Default",
			display_stat = { date = true, size = true, mode = true },
			hijack_netrw = true,
			use_fd = true,
			git_status = true,
			mappings = {
				["i"] = {
					["<C-a>"] = fb_actions.create,
					["<S-CR>"] = fb_actions.create_from_prompt,
					["<C-r>"] = fb_actions.rename,
					["<C-x>"] = fb_actions.move,
					["<C-v>"] = fb_actions.copy,
					["<C-d>"] = fb_actions.remove,
					["<C-s>"] = fb_actions.open,
					["<C-w>"] = fb_actions.goto_home_dir,
					["<C-CR>"] = fb_actions.goto_cwd,
					["<C-e>"] = fb_actions.change_cwd,
					["<C-f>"] = fb_actions.toggle_browser,
					["<A-s>"] = fb_actions.toggle_hidden,
					["<A-a>"] = fb_actions.toggle_all,
					["<S-Left>"] = fb_actions.goto_parent_dir,
					-- ["<C-t>"] = require('telescope.actions').select_tab,
				},
				["n"] = {
					["a"] = fb_actions.create,
					["r"] = fb_actions.rename,
					["x"] = fb_actions.move,
					["c"] = fb_actions.copy,
					["d"] = fb_actions.remove,
					["s"] = fb_actions.open,
					["g"] = fb_actions.goto_parent_dir,
					["w"] = fb_actions.goto_home_dir,
					["<CR>"] = fb_actions.goto_cwd,
					["e"] = fb_actions.change_cwd,
					["f"] = fb_actions.toggle_browser,
					["h"] = fb_actions.toggle_hidden,
					["t"] = fb_actions.toggle_all,
				},
			},
		},
    },
	defaults = {
		mappings = {
			i = {
                ['<C-Right>'] = require('telescope.actions').select_tab,
                ['<C-Up>'] = require('telescope.actions').select_horizontal,
                ['<C-Left>'] = require('telescope.actions').select_vertical,
				['<M-Up>'] = function (bufnr) slow_scroll(bufnr, -2) end,
				['<M-Down>'] = function (bufnr) slow_scroll(bufnr, 2) end,
				['<S-Right>'] = function (bufnr) vim.cmd('call feedkeys("\\<CR>")') end 
			},
			n = {
				["<C-c>"] = function (bufnr) vim.cmd('call feedkeys("i")') end
			}
		},
	},
}
pcall(require'telescope'.load_extension, 'neoclip')
require'telescope'.load_extension 'file_browser'

-- REMAINDER --
-- $Comment setup
require('Comment').setup({
    padding = true,
    sticky = true,
    toggler = {
        line = '<C-del>',
        block = '<S-del>',
    },
    opleader = {
        line = '<C-del>',
        block = '<S-del>',
    },
    extra = {
        above = 'gcO',
        below = 'gco',
        eol = 'gcA',
    },
    mappings = {
        basic = true,
        extra = true,
    },
    pre_hook = nil,
    post_hook = nil,
})
local ft = require('Comment.ft')
ft.set('asy', { '//%s', '/*%s*/' })
-- $tabby setup
local theme = {
	fill = 'TabLineFill',
	head = 'TabLine',
	current_tab = { fg = '#000000', bg = '#89a870', style = 'italic' },--'TabLineSel',
	tab = 'TabLine',
	win = 'TabLine',
	tail = 'TabLine',
}
require('tabby.tabline').set(function(line)
	return {
		line.tabs().foreach(function(tab)
			local hl = tab.is_current() and theme.current_tab or theme.tab
			return {
				line.sep('', hl, theme.fill),
				-- tab.is_current() and '' or '󰆣',
				tab.name(),
				line.sep('', hl, theme.fill),
				hl = hl,
				margin = ' ',
			}
		end),
	}
end)
-- $treesitter setup
require('nvim-treesitter.configs').setup {
	modules = {},
	sync_install = true,
	ignore_install = {},
	ensure_installed = { 'cpp', 'lua', 'python', 'vimdoc', 'vim' },
	highlight = { enable = true },
	indent = { enable = false },
	incremental_selection = {
		enable = true,
		keymaps = {
			init_selection = '<c-space>',
			node_incremental = '<c-space>',
			scope_incremental = '<c-s>',
			node_decremental = '<M-space>',
		},
	},
}
-- $lualine setup
local function keymap()
	if vim.opt.iminsert:get() > 0 and vim.b.keymap_name then
		return vim.b.keymap_name
	end
	return 'en'
end
require('lualine').setup{
	options = {
		icons_enabled = true,
		-- theme = 'onedark',
		component_separators = '|',
		section_separators = '',
	},
	sections = {
		lualine_a = {'mode'},
		lualine_b = {'branch', 'diff', 'diagnostics'},
		lualine_c = {'filename', keymap},
		lualine_x = {'filetype'},
		lualine_y = {'progress'},
		lualine_z = {'location'},
	},
}
require('onedark').setup  {
	style = 'dark',
	transparent = true,  -- Show/hide background
	term_colors = true, -- Change terminal color as per the selected theme style
	ending_tildes = false, -- Show the end-of-buffer tildes. By default they are hidden
	cmp_itemkind_reverse = false, -- reverse item kind highlights in cmp menu
	toggle_style_key = "<leader>wt",
	toggle_style_list = {'dark', 'darker', 'cool', 'deep', 'warm', 'warmer', 'light'},
	code_style = {
		comments = 'italic',
		keywords = 'none',
		functions = 'none',
		strings = 'none',
		variables = 'none'
	},
	-- lualine = {
	-- 	transparent = false,
	-- },
	colors = {},
	highlights = {},
	diagnostics = {
		darker = true,
		undercurl = true,
		background = true,
	},
}
require 'ibl'.setup({
	indent = {
		char = '┊',
	},
	whitespace = {
		remove_blankline_trail = true,
	},
	-- char = ,
	-- show_trailing_blankline_indent = false,
})

-- SETTINGS --

vim.o.swapfile = false
vim.o.wrap = true
vim.o.linebreak = true
vim.o.list = false
vim.o.breakat = '   '
vim.opt.autochdir=true
vim.o.shell = '/bin/sh'
vim.wo.number = true
-- vim.wo.relativenumber = true
vim.o.mouse = 'a'
vim.o.breakindent = true
vim.o.undofile = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.wo.signcolumn = 'yes'
vim.o.updatetime = 1000
vim.o.timeout = false
vim.o.completeopt = 'menuone,noselect'
vim.o.termguicolors = true
vim.o.cmdheight = 1
vim.o.ruler = false
vim.o.termguicolors = true
vim.o.scrolloff = 5
vim.o.colorcolumn = 20
vim.o.tabstop = 4
vim.o.expandtab = true
vim.cmd('set shiftwidth=4 smarttab')
vim.o.shiftwidth = 4
vim.o.compatible = false
vim.o.hlsearch = false
vim.o.synmaxcol = 0
-- vim.o.cursorline = true
-- vim.o.guicursor=''


local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
	callback = function()
		vim.highlight.on_yank()
	end,
	group = highlight_group,
	pattern = '*',
})

vim.api.nvim_create_autocmd('VimLeave', {
	pattern = '*',
	callback = function () vim.opt.guicursor = { 'a:ver25' } end
})

vim.g.haskell_classic_highlighting = 1

vim.g.vimtex_delim_timeout = 60

vim.g.vimtex_complete_enabled = 1
vim.g.vimtex_quickfix_enabled = 0
vim.g.vimtex_view_method = 'zathura'
vim.g.latex_view_general_viewer = 'zathura'
vim.g.vimtex_compiler_progname = 'nvr'
vim.g.vimtex_view_general_options = '-reuse-instance -forward-search @tex @line @pdf'
-- vim.g.vimtex_view_automatic = 1
vim.g.vimtex_mappings_prefix = '\\'
-- vim.g.vimtex_compiler_method = 'latexmk'
vim.cmd([[let g:vimtex_compiler_latexmk = {'continuous': 1, 'aux_dir': '.aux', 'options': ['-verbose', '-synctex=1', '-interaction=nonstopmode', '-file-line-error']}]])
-- vim.o.conceallevel = 1
-- vim.g.tex_conceal = 'abdmg'
-- vim.cmd('hi Conceal ctermbg=none')

vim.g.UltiSnipsExpandTrigger='<tab>'
vim.g.UltiSnipsJumpForwardTrigger='<C-Right>'
vim.g.UltiSnipsJumpBackwardTrigger='<C-Left>'
vim.g.UltiSnipsEditSplit='horizontal'

-- $markdown settings
--vim.o.vim_markdown_folding_disabled = 1
vim.o.vim_markdown_folding_level = 6
vim.o.vim_markdown_folding_style_pythonic = 1

vim.cmd.colorscheme 'onedark'
vim.cmd(
[[
    highlight Function guifg=burlywood
    highlight Number guifg=lightsteelblue
    highlight Include guifg=orchid
    highlight Type guifg=lightseagreen
    highlight Constant guifg=palevioletred gui=italic cterm=italic
    highlight Operator guifg=aquamarine
    highlight Keyword guifg=plum
]])

package.path = package.path .. ';'..home..'/programming/nvim-subfiles/lua/?.lua'
-- package.path = package.path .. ';/home/ramak/projects/programming/nvim-subfiles/lua/?.lua'
require('nvim-subfiles').setup({
  ['subfile'] = 'SF',
  ['subfigure'] = 'F',
})
-- require('nvim-subfiles').setup({})

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
-- vim.g.loaded_netrw = 1
-- vim.g.loaded_netrwPlugin = 1
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
	-- 'tpope/vim-fugitive',
    'tpope/vim-rhubarb',
	'tpope/vim-surround',
    'sagarrakshe/toggle-bool',
	'nanozuki/tabby.nvim',
	'lervag/vimtex',
	'farmergreg/vim-lastplace',
	-- 'Xe/lolcode.vim',
	'sirver/ultisnips',
	'neovimhaskell/haskell-vim',
	'hjson/vim-hjson',
	'nvim-lualine/lualine.nvim',
	'dkarter/bullets.vim',
	-- 'cljoly/telescope-repo.nvim',
	'numToStr/Comment.nvim',
	'ap/vim-css-color',
    -- 'ixru/nvim-markdown',
    -- 'ThePrimeagen/harpoon',
	'junegunn/fzf',
	'junegunn/fzf.vim',
    {
        'lewis6991/gitsigns.nvim',
        opts = {
            on_attach = function(bufnr)
                vim.keymap.set('n', '<leader>gp', require('gitsigns').prev_hunk, { buffer = bufnr, desc = '[G]o to [P]revious Hunk' })
                vim.keymap.set('n', '<leader>gn', require('gitsigns').next_hunk, { buffer = bufnr, desc = '[G]o to [N]ext Hunk' })
                vim.keymap.set('n', '<leader>ph', require('gitsigns').preview_hunk, { buffer = bufnr, desc = '[P]review [H]unk' })
            end,
        },
    },
    {
        'altermo/ultimate-autopair.nvim',
        event={'InsertEnter','CmdlineEnter'},
        branch='v0.6', --recomended as each new version will have breaking changes
        opts={
            space2 = { enable = true },
            tabout = { enable = true },
            fastwarp = {
                enable = true,
                enable_normal = true,
                enable_reverse = true,
                hopout = false,
                faster = false,
				map = '<C-/>',
				rmap = '<C-S-/>',
            },
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
		'nvim-treesitter/nvim-treesitter',
		dependencies = {
			'nvim-treesitter/nvim-treesitter-textobjects',
		},
		build = ':TSUpdate',
	},
    -- 'thornoar/nvim-subfiles',
}, {}) require('gitsigns').setup()

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
	-- ['tex'] = function (name) return ('!latexmk -g -pdf ' .. name) end,
	['tex'] = function (name) return ('VimtexCompile') end,
	['lua'] = function (name) return ('!lua ' .. name) end,
	['lolcode'] = function (name) return ('!lci ' .. name) end,
	['java'] = function (name) return ('!javac ' .. name .. ' && java Main') end,
    ['pdf'] = function (name) return ('!nohup zathura ' .. name .. '&') end,
    ['nix'] = function (name) return ('!nix eval --file ' .. name) end,
}
newcmd("AS", function() 
	autosave = not autosave
	print("autosave is " .. (autosave and "enabled" or "disabled"))
end)
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
newcmd('D', function () vim.bo.keymap = 'german-qwertz' end)
newcmd('L', function () vim.cmd('Lazy') end)
newcmd('S', function () vim.wo.spell = not vim.wo.spell end)
newcmd('NS', function () vim.cmd('set nospell') end)
newcmd('G', function () vim.cmd('GitFiles') end)
newcmd('X', function () vim.cmd('Files') end)
newcmd('C', function () vim.cmd('Jumps') end)
-- Autocommands
autosave = true
autosavepattern = { '*.tex', '*.asy', '*.md', '*.lua', '*.cpp', '*.py', '*.hs', '*.txt', '*.lol', '*.r', '*.snippets', '*.java', '*.nix', '*.hjson', '*.vim' }
vim.api.nvim_create_autocmd({ 'TextChanged', 'TextChangedI', 'TextChangedP' }, {
	pattern = '*',
	callback = function()
		if autosave then vim.cmd('silent write') end
	end
})
vim.api.nvim_create_autocmd({'BufRead', 'BufNewFile'}, {
	pattern = {'*.md', '*.txt'},
	command = 'setlocal spell! spelllang=en_us'
})

vim.cmd([[
command! -nargs=? -range Dec2hex call s:Dec2hex(<line1>, <line2>, '<args>')
function! s:Dec2hex(line1, line2, arg) range
    if empty(a:arg)
        if histget(':', -1) =~# "^'<,'>" && visualmode() !=# 'V'
            let cmd = 's/\%V\<\d\+\>/\=printf("0x%x",submatch(0)+0)/g'
        else
            let cmd = 's/\<\d\+\>/\=printf("0x%x",submatch(0)+0)/g'
        endif
        try
            execute a:line1 . ',' . a:line2 . cmd
            catch
                echo 'Error: No decimal number found'
        endtry
    else
        echo printf('%x', a:arg + 0)
    endif
endfunction
]])

-- KEYMAPS --
-- $text keymaps
km.set('n', 'w1', 'mL')
km.set('n', 'w2', 'mN')
km.set('n', 'w3', 'mM')
km.set('n', 'w4', 'mO')
km.set('n', '<C-1>', '\'L')
km.set('n', '<C-2>', '\'N')
km.set('n', '<C-3>', '\'M')
km.set('n', '<C-4>', '\'O')
km.set('n', '<S-End>', 'o<Esc>')
km.set('n', '<S-Home>', 'ddk')

km.set('n', '<C-S-Left>', '<<')
km.set('n', '<C-S-Right>', '>>')
km.set('v', '<C-S-Left>', '<gv')
km.set('v', '<C-S-Right>', '>gv')

km.set('v', '<S-Left>', 'ygv<Esc>')
km.set('n', '<S-Left>', 'yy')
km.set({'n', 'v'}, '<S-Right>', 'p')
km.set('i', '<S-Right>', '<esc>pa')

km.set('n', '<M-CR>', 'md*ggn')
km.set('n', '<M-]>', '\'d')
km.set('v', '<C-S-Down>', ":m '>+1<CR>gv=gv")
km.set('v', '<C-S-Up>', ":m '<-2<CR>gv=gv")
km.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })
km.set('v', 'z', '<esc>')
km.set('v', '<S-Up>', '<Up>')
km.set('v', '<S-Down>', '<Down>')
km.set('n', '<S-Down>', '<S-v>j')
km.set('n', '<S-Up>', '<S-v>k')
km.set('v', '<leader>a', ':s/\\d\\+/\\=(submatch(0)+1)/g')
km.set('n', '<leader>cw', ":%s/\\w\\@<!\\<<C-r><C-w>\\>\\w\\@!/")
km.set('n', 'cw', 'ciw')
km.set('n', 'dw', 'diw')
km.set('x', '<leader>p', '\"_dP')
km.set('n', '<leader>f', 'zf%')
km.set('n', '<C-End>', 'k<S-v>jj<S-j>')
km.set('v', '<M-End>', 'J')
km.set('n', '<C-z>', 'u')
-- $insert keymaps
km.set('n', 'x', 'i')
km.set('i', '<C-x>', '<C-n>')
km.set('i', '<M-s>', '<C-o>$;')
km.set('i', '<C-z>', '<Esc>[s1z=A')
km.set('i', '<C-w>', function () vim.cmd('silent write') end)
-- $navigation keymaps
km.set('n', '<Up>', 'gk')
km.set('n', '<Down>', 'gj')
km.set('i', '<Up>', '<C-o>gk')
km.set('i', '<Down>', '<C-o>gj')
km.set({'n', 'v'}, '<M-Up>', '10k')
km.set('v', '<S-M-Up>', '10k')
km.set({'n', 'v'}, '<M-Down>', '10j')
km.set('v', '<S-M-Down>', '10j')
km.set({'n', 'v'}, '<M-Left>', 'b')
km.set({'n', 'v'}, '<M-Right>', 'w')
km.set('i', '<M-up>', '<C-o>10k')
km.set('i', '<M-down>', '<C-o>10j')
km.set('i', '<M-left>', '<esc>gea')
km.set('i', '<M-right>', '<esc>Ea')
km.set({'n', 'v'}, '<M-a>', '%')
km.set('n', 'n', 'nzz')
km.set('n', 'N', 'Nzz')   
km.set('n', '<C-M-left>', '<C-^>')
km.set('n', '<C-M-right>', 'gf')
-- $window keymaps
km.set('n', '<C-Left>', '<C-w>h')
km.set('n', '<C-Down>', '<C-w>j')
km.set('n', '<C-Up>', '<C-w>k')
km.set('n', '<C-Right>', '<C-w>l')
km.set('i', '<C-Left>', '<C-o><C-w>h')
km.set('i', '<C-Down>', '<C-o><C-w>j')
km.set('i', '<C-Up>', '<C-o><C-w>k')
km.set('i', '<C-Right>', '<C-o><C-w>l')
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
km.set('n', '<C-c>', function () vim.cmd('quit') end)
km.set('n', '<C-d>', function () vim.cmd('silent !$TERMINAL -e zsh -c \'source $NIXOS_CONFIG/dotfiles/br.sh; $FILEMANAGER; zsh\'&') end)
km.set('n', '<C-a>', function () vim.cmd('silent !$TERMINAL&') end)
km.set('n', '<leader>k', function () vim.cmd('edit $NIXOS_CONFIG/home-ramak/main.nix') end)
km.set('n', '<leader>K', function () vim.cmd('tabnew $NIXOS_CONFIG/home-ramak/main.nix') end)
km.set('n', '<leader>l', function () vim.cmd('edit $NIXOS_CONFIG/dotfiles/nvim/init.lua') end)
km.set('n', '<leader>L', function () vim.cmd('tabnew $NIXOS_CONFIG/dotfiles/nvim/init.lua') end)
km.set('n', '<leader>n', function () vim.cmd('edit $NIXOS_CONFIG/local/home-local.nix') end)
km.set('n', '<leader>N', function () vim.cmd('edit $NIXOS_CONFIG/local/system-local.nix') end)
km.set('n', '<leader>o', ':Compile<CR>')
km.set('n', '<leader>x', function () vim.cmd('CompileSilent') end)
km.set({'n','i'}, '<M-s>', function () vim.cmd('CompileSilent') end)
km.set('n', '<leader>vp', function () vim.cmd('ViewPdf') end)
km.set('n', '<leader>ee', function () vim.cmd('sp $NIXOS_CONFIG/dotfiles/nvim/UltiSnips/%:e.snippets') end)
km.set('n', 'd[', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
km.set('n', 'd]', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
km.set('n', '<leader>de', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
km.set('n', '<leader>dq', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })
-- $new keybinds
km.set('n', '<C-s>', function () vim.cmd('silent !$TERMINAL -e lazygit&') end)
km.set('n', '<C-f>', function () vim.cmd('Files') end)
km.set('n', '<C-e>', function () vim.cmd('Buf') end)
km.set('n', '<C-x>', function () vim.cmd('GitFiles') end)
km.set('n', 'X', function () vim.cmd('ToggleBool') end)
vim.cmd([[let g:fzf_action = {'ctrl-s': 'tab split', 'ctrl-x': 'vertical split'}]])

-- km.set('i', 'M-;', '<End>;<CR>')
km.set('i', '<M-a>', '<C-o>$;')

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
ft.set('hjson', { '#%s' })
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
                ['@parameter.outer'] = 'v', -- charwise
                ['@function.outer'] = 'V', -- linewise
                ['@class.outer'] = '<c-v>', -- blockwise
            },
            include_surrounding_whitespace = true,
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
require('onedark').setup  {
	style = 'dark',
	transparent = true,
	term_colors = true,
	ending_tildes = false,
	cmp_itemkind_reverse = false,
	toggle_style_key = "<leader>wt",
	toggle_style_list = {'dark', 'darker', 'cool', 'deep', 'warm', 'warmer', 'light'},
	code_style = {
		comments = 'italic',
		keywords = 'none',
		functions = 'none',
		strings = 'none',
		variables = 'none'
	},
	lualine = {
		transparent = false,
	},
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
vim.o.expandtab = true
vim.o.compatible = false
vim.o.hlsearch = false
vim.o.synmaxcol = 0
-- vim.o.cursorline = true
vim.o.tabstop = 4
vim.o.shiftwidth = 4
-- vim.o.binary = true
-- vim.o.eol = false
vim.cmd('set shiftwidth=4 smarttab')
vim.cmd('set clipboard+=unnamedplus')

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
vim.cmd([[let g:vimtex_compiler_latexmk = {'continuous': 0, 'aux_dir': '.aux', 'options': ['-verbose', '-synctex=1', '-interaction=nonstopmode', '-file-line-error']}]])
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

package.path = package.path .. ';'..home..'/nvim-subfiles/lua/?.lua'
require('nvim-subfiles').setup({
  ['subfile'] = 'SF',
  ['subfigure'] = 'F',
}, { })

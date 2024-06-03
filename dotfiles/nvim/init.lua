vim.loader.enable()

-- VARIABLES --
vim.keymap.set('n', 'ec', ':e $NIXOS_CONFIG/dotfiles/nvim/init.lua<CR>')
home = os.getenv('PROJECTS')
testdir = home..'/sandbox'
vim.g.mapleader = ';'

-- INSTALL --
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
    'sagarrakshe/toggle-bool',
	'farmergreg/vim-lastplace',
	'sirver/ultisnips',
	'neovimhaskell/haskell-vim',
	'nvim-lualine/lualine.nvim',
	'numToStr/Comment.nvim',
	'ap/vim-css-color',
	'junegunn/fzf',
	'junegunn/fzf.vim',
    'nanozuki/tabby.nvim',
    'lewis6991/gitsigns.nvim',
    'github/copilot.vim',
    'mbbill/undotree',
    {
        'kaarmu/typst.vim',
        ft = 'typst',
        lazy = false,
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
        'gen740/SmoothCursor.nvim',
        config = function ()
            require('smoothcursor').setup({
                cursor = '>',
                fancy = {
                    enable = true,
                    head = { cursor = '>', texthl = 'SmoothCursor' },
                    body = {
                        { cursor = '|', texthl = 'SmoothCursorGreen' },
                        { cursor = '|', texthl = 'SmoothCursorGreen' },
                        { cursor = '|', texthl = 'SmoothCursorGreen' },
                        { cursor = '|', texthl = 'SmoothCursorGreen' },
                        { cursor = '|', texthl = 'SmoothCursorGreen' },
                    }
                },
            })
        end
    }
    -- 'thornoar/nvim-subfiles',
}, {})

-- COMMANDS --
local compilefunc = {
	['asy'] = function (name) return ('!asy -noV -nosafe ' .. name) end,
	['r'] = function (name) return ('!Rscript ' .. name) end,
    ['julia'] = function (name) return ('!julia ' .. name) end,
	['python'] = function (name) return ('!python ' .. name) end,
	['c'] = function (name) return ('!gcc ' .. name .. ' && ./a.out') end,
	['cpp'] = function (name) return ('!g++ -Wall ' .. name .. ' -o cpp.out && ./cpp.out') end,
    ['rust'] = function (name) return ('!rustc ' .. name .. ' -o rust.out && ./rust.out') end,
	['haskell'] = function (name) return ('!runhaskell ' .. name) end,
	['tex'] = function (name) return ('!latexmk -g -pdf -synctex=1 -verbose -auxdir=./.aux ./main.tex') end,
	['typst'] = function (name) return ('write') end,
	-- ['tex'] = function (name) return ('!latexmk -g -pdf ' .. name) end,
	-- ['tex'] = function (name) return ('VimtexCompile') end,
	['lua'] = function (name) return ('!lua ' .. name) end,
	['lolcode'] = function (name) return ('!lci ' .. name) end,
	['java'] = function (name) return ('!javac ' .. name .. ' && java Main') end,
    ['pdf'] = function (name) return ('!nohup zathura ' .. name .. '&') end,
    ['nix'] = function (name) return ('!nix eval --file ' .. name) end,
}

vim.api.nvim_create_user_command('Compile', function () 
    if vim.bo.modified then vim.cmd('write') end
	local ccmd = compilefunc[vim.bo.filetype]
	vim.cmd(not ccmd and 'echo \'not set to compile\'' or ccmd('%:t'))
end, {})

vim.api.nvim_create_user_command('CompileSilent', function () 
    if vim.bo.modified then vim.cmd('write') end
	local ccmd = compilefunc[vim.bo.filetype]
	vim.cmd(not ccmd and '' or 'silent '..ccmd('%:t'))
end, {})

local defaultoutputname = 'out'
vim.api.nvim_create_user_command('V', function (args)
    ext, flags = (args and args['args'] or 'pdf'):match"^(%S+)%s+(.+)"
    if not ext then ext = (args and args['args'] or 'pdf') end
    if not flags then flags = '' else flags = ' '..flags end
	if io.open(vim.fn.expand('%:r')..'.'..ext, 'r') ~= nil then
		vim.cmd('silent !xdg-open %:r.'..ext..'&')
	elseif io.open(defaultoutputname..'.'..ext, 'r') ~= nil then
		vim.cmd('silent !xdg-open '..defaultoutputname..'.'..ext..'&')
	else
		vim.cmd('silent !blkcmd '..ext..' xdg-open'..flags..'&')
	end
end, { nargs = '?' })

vim.api.nvim_create_user_command('E', function () vim.bo.keymap = '' end, {})
vim.api.nvim_create_user_command('R', function () vim.bo.keymap = 'russian-jcuken' end, {})
vim.api.nvim_create_user_command('J', function () vim.bo.keymap = 'kana' end, {})
vim.api.nvim_create_user_command('D', function () vim.bo.keymap = 'german-qwertz' end, {})
vim.api.nvim_create_user_command('L', function () vim.cmd('Lazy') end, {})
vim.api.nvim_create_user_command('S', function () vim.wo.spell = not vim.wo.spell end, {})
vim.api.nvim_create_user_command('W', function () vim.cmd('wa') end, {})

-- Autocommands
vim.api.nvim_create_autocmd({ 'BufRead', 'BufNewFile' }, {
	pattern = { '*.md' },
	command = 'setlocal spell! spelllang=en_us'
})

local autosave = true
vim.api.nvim_create_user_command("AS", function() 
	autosave = not autosave
	print("autosave is " .. (autosave and "enabled" or "disabled"))
end, {})
local autosavepattern = { '*.tex', '*.asy', '*.md', '*.lua', '*.cpp', '*.py', '*.hs', '*.txt', '*.r', '*.snippets', '*.nix', '*.hjson', '*.vim', '*.sh', '*.html', '*.css', '*.c', '*.jl' }
vim.api.nvim_create_autocmd({ 'TextChanged', 'TextChangedI', 'TextChangedP' }, {
	-- pattern = '*.*',
    pattern = autosavepattern,
	callback = function()
		if autosave then vim.cmd('silent write') end
	end
})

-- KEYMAPS --
-- $text keymaps
vim.keymap.set('n', 'w1', 'mL')
vim.keymap.set('n', 'w2', 'mN')
vim.keymap.set('n', 'w3', 'mM')
vim.keymap.set('n', 'w4', 'mO')
vim.keymap.set('n', '<C-1>', '\'L')
vim.keymap.set('n', '<C-2>', '\'N')
vim.keymap.set('n', '<C-3>', '\'M')
vim.keymap.set('n', '<C-4>', '\'O')
vim.keymap.set('n', '<S-End>', 'o<Esc>')
vim.keymap.set('n', '<S-Home>', 'ddk')
vim.keymap.set('n', '<C-S-Left>', '<<')
vim.keymap.set('n', '<C-S-Right>', '>>')
vim.keymap.set('v', '<C-S-Left>', '<gv')
vim.keymap.set('v', '<C-S-Right>', '>gv')
vim.keymap.set('v', '<S-Left>', 'ygv<Esc>')
vim.keymap.set('n', '<S-Left>', 'yy')
vim.keymap.set({'n', 'v'}, '<S-Right>', 'p')
vim.keymap.set('i', '<S-Right>', '<esc>pa')
vim.keymap.set('n', '<M-CR>', 'md*ggn')
vim.keymap.set('n', '<M-]>', '\'d')
vim.keymap.set('v', '<C-S-Down>', ':m \'>+1<CR>gv=gv')
vim.keymap.set('v', '<C-S-Up>', ':m \'<-2<CR>gv=gv')
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })
vim.keymap.set('v', 'z', '<esc>')
vim.keymap.set('v', '<S-Up>', '<Up>')
vim.keymap.set('v', '<S-Down>', '<Down>')
vim.keymap.set('n', '<S-Down>', '<S-v>j')
vim.keymap.set('n', '<S-Up>', '<S-v>k')
vim.keymap.set('v', '<leader>a', ':s/\\d\\+/\\=(submatch(0)+1)/g')
vim.keymap.set('n', '<leader>cw', ':%s/\\w\\@<!\\<<C-r><C-w>\\>\\w\\@!/')
vim.keymap.set('n', 'cw', 'ciw')
vim.keymap.set('n', 'dw', 'diw')
vim.keymap.set({'v', 'x'}, '<leader>p', '\"_dP')
vim.keymap.set('n', '<leader>f', 'zf%')
vim.keymap.set('n', '<C-End>', 'k<S-v>jj<S-j>')
vim.keymap.set('v', '<M-End>', 'J')
vim.keymap.set('n', '<C-z>', 'u')
vim.keymap.set('n', '<C-M-z>', ':UndotreeToggle<CR>')
vim.keymap.set('n', '<M-z>', '<C-r>')
vim.keymap.set('n', 'daa', 'F,dt)')
vim.keymap.set('n', '<C-space>', 'yy<C-del>p')
-- $insert keymaps
vim.keymap.set('i', '<C-Space>', ' ')
vim.keymap.set('i', '<C-x>', '<C-n>')
vim.keymap.set('i', '<C-d>', '<Esc>')
vim.keymap.set('i', '<M-a>', '<C-o>$;')
vim.keymap.set('i', '<C-z>', '<Esc>[s1z=A')
vim.keymap.set('n', 'x', 'i')
vim.keymap.set('n', 'X', 'I')
-- $navigation keymaps
vim.keymap.set('n', '<Up>', 'gk')
vim.keymap.set('n', '<Down>', 'gj')
vim.keymap.set('i', '<Up>', '<C-o>gk')
vim.keymap.set('i', '<Down>', '<C-o>gj')
vim.keymap.set({'n', 'v'}, '<M-Up>', '5k')
vim.keymap.set('v', '<S-M-Up>', '5k')
vim.keymap.set({'n', 'v'}, '<M-Down>', '5j')
vim.keymap.set('v', '<S-M-Down>', '5j')
vim.keymap.set({'n', 'v'}, '<M-Left>', '5h')
vim.keymap.set({'n', 'v'}, '<M-Right>', '5l')
vim.keymap.set('i', '<M-up>', '<C-o>5k')
vim.keymap.set('i', '<M-down>', '<C-o>5j')
vim.keymap.set('i', '<M-left>', '<C-o>5h')
vim.keymap.set('i', '<M-right>', '<C-o>5l')
vim.keymap.set({'n', 'v'}, '<M-a>', '%')
vim.keymap.set('n', 'n', 'nzz')
vim.keymap.set('n', 'N', 'Nzz')   
vim.keymap.set('n', '<C-M-left>', '<C-^>')
vim.keymap.set('n', '<C-M-right>', 'gf')
-- $window keymaps
vim.keymap.set('n', '<C-Left>', '<C-w>h')
vim.keymap.set('n', '<C-Down>', '<C-w>j')
vim.keymap.set('n', '<C-Up>', '<C-w>k')
vim.keymap.set('n', '<C-Right>', '<C-w>l')
vim.keymap.set('i', '<C-Left>', '<C-o><C-w>h')
vim.keymap.set('i', '<C-Down>', '<C-o><C-w>j')
vim.keymap.set('i', '<C-Up>', '<C-o><C-w>k')
vim.keymap.set('i', '<C-Right>', '<C-o><C-w>l')
vim.keymap.set('n', '<C-w><Left>', '<C-w>H')
vim.keymap.set('n', '<C-w><Right>', '<C-w>L')
vim.keymap.set('n', '<C-w><Up>', '<C-w>K')
vim.keymap.set('n', '<C-w><Down>', '<C-w>J')
vim.keymap.set('n', '<C-M-o>', '<C-w>+')
vim.keymap.set('n', '<C-M-l>', '<C-w>-')
vim.keymap.set('n', '<C-M-i>', '<C-w>>')
vim.keymap.set('n', '<C-M-u>', '<C-w><')
vim.keymap.set('n', '<C-M-k>', '<C-w>=')
-- $command keymaps
vim.keymap.set('n', '<C-c>', function () vim.cmd('wq') end)
vim.keymap.set('n', '<C-a>', function () vim.cmd('silent !$TERMINAL&') end)
vim.keymap.set('n', '<C-f>', function () vim.cmd('silent !$TERMINAL -e zsh -c \'source $NIXOS_CONFIG/dotfiles/br.sh; $FILEMANAGER; zsh\'&') end)
vim.keymap.set('n', '<leader>k', function () vim.cmd('edit $NIXOS_CONFIG/home-ramak/main.nix') end)
vim.keymap.set('n', '<leader>K', function () vim.cmd('tabnew $NIXOS_CONFIG/home-ramak/main.nix') end)
vim.keymap.set('n', '<leader>l', function () vim.cmd('edit $NIXOS_CONFIG/dotfiles/nvim/init.lua') end)
vim.keymap.set('n', '<leader>L', function () vim.cmd('tabnew $NIXOS_CONFIG/dotfiles/nvim/init.lua') end)
vim.keymap.set('n', '<leader>n', function () vim.cmd('edit $NIXOS_LOCAL/home-local.nix') end)
vim.keymap.set('n', '<leader>N', function () vim.cmd('edit $NIXOS_LOCAL/system-local.nix') end)
vim.keymap.set('n', '<leader>o', ':Compile<CR>')
vim.keymap.set({'n', 'i'}, '<C-s>', function () vim.cmd('CompileSilent') end)
vim.keymap.set('n', '<leader>vp', function () vim.cmd('V pdf') end)
vim.keymap.set('n', '<leader>vs', function () vim.cmd('V svg') end)
vim.keymap.set('n', '<leader>vg', function () vim.cmd('V png') end)
vim.keymap.set('n', '<leader>ee', function () vim.cmd('sp $NIXOS_CONFIG/dotfiles/nvim/UltiSnips/%:e.snippets') end)
vim.keymap.set('n', '<leader>cd', function () vim.cmd('Copilot disable') end)
vim.keymap.set('n', 'Z', function () vim.cmd('ToggleBool') end)
vim.keymap.set('n', '<M-s>', function () vim.cmd('silent Gitsigns preview_hunk_inline') end)
vim.keymap.set('n', '<C-x>', function () vim.cmd('GitFiles') end)
vim.keymap.set('n', '<C-d>', function () vim.cmd('Files') end)
vim.keymap.set('n', '<C-e>', function () vim.cmd('Buf') end)
vim.cmd([[let g:fzf_action = {'ctrl-s': 'tab split', 'ctrl-x': 'vertical split', 'ctrl-d': 'horizontal split'}]])

-- REMAINDER 
-- $Comment setup
require('Comment').setup({
    padding = true,
    sticky = true,
    toggler = {
        line = '\\\\',
        block = '||',
    },
    opleader = {
        line = '\\',
        block = '|',
    },
    extra = {
        above = '<C-\\>O',
        below = '<C-\\>o',
        eol = '<C-S-a>',
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
-- $gitsigns setup
require('gitsigns').setup {
    numhl = true,
    current_line_blame = true,
    current_line_blame_opts = {
        delay = 1000,
        ignore_whitespace = true,
    },
    max_file_length = 10000,
    on_attach = function(bufnr)
        local gitsigns = require('gitsigns')

        local function map (mode, l, r, opts)
            opts = opts or {}
            opts.buffer = bufnr
            vim.keymap.set(mode, l, r, opts)
        end

        -- Navigation
        map('n', ']c', function ()
            if vim.wo.diff then
                vim.cmd.normal({']c', bang = true})
            else
                gitsigns.next_hunk()
            end
        end)

        map('n', '[c', function ()
            if vim.wo.diff then
                vim.cmd.normal({'[c', bang = true})
            else
                gitsigns.prev_hunk()
            end
        end)

        -- Actions
        map('n', '<leader>hs', gitsigns.stage_hunk)
        map('n', '<leader>hr', gitsigns.reset_hunk)
        map('v', '<leader>hs', function() gitsigns.stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
        map('v', '<leader>hr', function() gitsigns.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
        map('n', '<leader>hS', gitsigns.stage_buffer)
        map('n', '<leader>hu', gitsigns.undo_stage_hunk)
        map('n', '<leader>hR', gitsigns.reset_buffer)
        map('n', '<leader>hp', gitsigns.preview_hunk)
        map('n', '<leader>hb', function() gitsigns.blame_line{full=true} end)
        map('n', '<leader>tb', gitsigns.toggle_current_line_blame)
        map('n', '<leader>hd', gitsigns.diffthis)
        map('n', '<leader>hD', function() gitsigns.diffthis('~') end)
        map('n', '<leader>td', gitsigns.toggle_deleted)

        -- Text object
        map({'o', 'x'}, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
    end,
}
-- $treesitter setup
require('nvim-treesitter.configs').setup {
	modules = {},
	sync_install = true,
	ignore_install = {},
	ensure_installed = { 'cpp', 'lua', 'python', 'vimdoc', 'vim', 'hjson', 'java' },
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
                ['af'] = '@function.outer',
                ['if'] = '@function.inner',
                ['ac'] = '@class.outer',
                ['ic'] = { query = '@class.inner', desc = 'Select inner part of a class region' },
                ['as'] = { query = '@scope', query_group = 'locals', desc = 'Select language scope' },
            },
            selection_modes = {
                ['@parameter.outer'] = 'v', -- charwise
                ['@function.outer'] = 'V', -- linewise
                ['@class.outer'] = '<c-v>', -- blockwise
            },
            include_surrounding_whitespace = false,
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
	toggle_style_key = '<leader>wt',
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
require('ibl').setup({
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
vim.o.incsearch = false
vim.o.synmaxcol = 0
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.cmd('let tex_flavor=\"latex\"')
vim.cmd('set shiftwidth=4 smarttab')
vim.cmd('set clipboard+=unnamedplus')
vim.cmd('let g:omni_sql_no_default_maps = 1')
vim.cmd('autocmd BufEnter * set formatoptions-=cro')
vim.cmd('autocmd BufEnter * setlocal formatoptions-=cro')

vim.g.neovide_transparency = 0.9

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
vim.g.vimtex_mappings_prefix = '\\'
vim.cmd([[let g:vimtex_compiler_latexmk = {'continuous': 0, 'aux_dir': '.aux', 'options': ['-verbose', '-synctex=1', '-interaction=nonstopmode', '-file-line-error']}]])

vim.g.UltiSnipsExpandTrigger='<tab>'
vim.g.UltiSnipsJumpForwardTrigger='<C-Right>'
vim.g.UltiSnipsJumpBackwardTrigger='<C-Left>'
vim.g.UltiSnipsEditSplit='horizontal'

-- $markdown settings
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
    bindings = {
    },
    opts = {
        jump_to_file = false,
    }
})

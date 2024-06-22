vim.loader.enable()

local km = vim.keymap
vim.g.mapleader = ';'
km.set('n', 'ec', ':e $NIXOS_CONFIG/dotfiles/nvim/init.lua<CR>')

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
	'tpope/vim-repeat',
    'sagarrakshe/toggle-bool',
	'farmergreg/vim-lastplace',
	'sirver/ultisnips',
	'neovimhaskell/haskell-vim',
	'nvim-lualine/lualine.nvim',
	'numToStr/Comment.nvim',
	'ap/vim-css-color',
    'nanozuki/tabby.nvim',
    'lewis6991/gitsigns.nvim',
    'hjson/vim-hjson',
	'dkarter/bullets.vim',
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
        "nvim-telescope/telescope-file-browser.nvim",
        dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
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
        'neovim/nvim-lspconfig',
        dependencies = {
            'williamboman/mason.nvim',
            'williamboman/mason-lspconfig.nvim',
            'hrsh7th/cmp-nvim-lsp',
        },
        -- command = 'Lsp',
        -- config = function ()
        -- end
    },
    {
        'hrsh7th/nvim-cmp',
        dependencies = {
            'L3MON4D3/LuaSnip',
        },
        -- command = 'Lsp',
        -- config = function ()
        -- end
    },
    {
        "quangnguyen30192/cmp-nvim-ultisnips",
        config = function()
            require("cmp_nvim_ultisnips").setup({})
        end,
    },
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
	['typst'] = function (name) return ('') end,
	['lua'] = function (name) return ('!lua ' .. name) end,
	['java'] = function (name) return ('!javac ' .. name .. ' && java Main') end,
    ['pdf'] = function (name) return ('!nohup zathura ' .. name .. '&') end,
    ['nix'] = function (name) return ('!nix eval --file ' .. name) end,
}
local daemonfunc = {
    ['typst'] = function (name) return 'TypstWatch' end,
}

local compile = function (daemon, silent)
    return function ()
        local compilecmd = (daemon and daemonfunc or compilefunc)[vim.bo.filetype]
        if not compilecmd then
            print('not set to compile')
        else
            if vim.bo.modified then vim.cmd('write') end
            vim.cmd((silent and 'silent ' or '') .. compilecmd('%:t'))
        end
    end
end
vim.api.nvim_create_user_command('Compile', compile(false, false), {})
km.set('n', '<leader>o', ':Compile<CR>')
km.set({'n', 'i'}, '<C-s>', compile(false, true))
km.set({'n', 'i'}, '<C-M-s>', compile(true, false))

local defaultoutputname = 'out'
local view_output = function (args)
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
end
vim.api.nvim_create_user_command('View', view_output, { nargs = '?' })
km.set('n', '<leader>vp', function () view_output({ ['args'] = 'pdf' }) end)
km.set('n', '<leader>vs', function () view_output({ ['args'] = 'svg' }) end)
km.set('n', '<leader>vg', function () view_output({ ['args'] = 'png' }) end)

vim.api.nvim_create_user_command('E', function () vim.bo.keymap = '' end, {})
vim.api.nvim_create_user_command('R', function () vim.bo.keymap = 'russian-jcuken' end, {})
vim.api.nvim_create_user_command('D', function () vim.bo.keymap = 'german-qwertz' end, {})
vim.api.nvim_create_user_command('J', function () vim.bo.keymap = 'kana' end, {})
vim.api.nvim_create_user_command('S', function () vim.wo.spell = not vim.wo.spell end, {})
vim.api.nvim_create_user_command('L', 'Lazy', {})

-- Autocommands
vim.api.nvim_create_autocmd({ 'BufRead', 'BufNewFile' }, {
	pattern = { '*.md', '*.typ', '*.tex' },
	command = 'setlocal spell! spelllang=en_us'
})

local autosave = true
vim.api.nvim_create_user_command('AS', function() 
	autosave = not autosave
	print('autosave is ' .. (autosave and 'enabled' or 'disabled'))
end, {})
local autosavepattern = { '*.asy', '*.md', '*.lua', '*.cpp', '*.py', '*.hs', '*.txt', '*.r', '*.snippets', '*.nix', '*.hjson', '*.vim', '*.sh', '*.html', '*.css', '*.c', '*.jl', '*.yml' }
vim.api.nvim_create_autocmd({ 'TextChanged', 'TextChangedI', 'TextChangedP' }, {
    pattern = autosavepattern,
	callback = function()
		if autosave and
        not vim.bo[vim.api.nvim_win_get_buf(0)].readonly and
        vim.bo[vim.api.nvim_win_get_buf(0)].buftype == '' then
            vim.cmd('silent write')
        end
	end
})

-- KEYMAPS --
-- $text keymaps
km.set('n', 'w1', 'mL')
km.set('n', 'w2', 'mN')
km.set('n', 'w3', 'mM')
km.set('n', 'w4', 'mO')
km.set('n', 'e1', '\'L')
km.set('n', 'e2', '\'N')
km.set('n', 'e3', '\'M')
km.set('n', 'e4', '\'O')
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
km.set('v', '<C-S-Down>', ':m \'>+1<CR>gv=gv')
km.set('v', '<C-S-Up>', ':m \'<-2<CR>gv=gv')
km.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })
km.set('v', 'z', '<esc>')
km.set('v', '<S-Up>', '<Up>')
km.set('v', '<S-Down>', '<Down>')
km.set('n', '<S-Down>', '<S-v>j')
km.set('n', '<S-Up>', '<S-v>k')
km.set('v', '<leader>a', ':s/\\d\\+/\\=(submatch(0)+1)/g')
km.set('n', '<leader>cw', ':%s/\\w\\@<!\\<<C-r><C-w>\\>\\w\\@!/')
km.set('n', 'cw', 'ciw')
km.set('n', 'dw', 'diw')
km.set('n', 'vw', 'viw')
km.set({'v', 'x'}, '<leader>p', '\"_dP')
km.set('n', '<leader>f', 'zf%')
km.set('n', '<C-End>', 'k<S-v>jj<S-j>')
km.set('v', '<M-End>', 'J')
km.set('n', '<C-z>', 'u')
km.set('n', '<C-M-z>', ':UndotreeToggle<CR>')
km.set('n', '<M-z>', '<C-r>')
km.set('n', 'daa', 'F,dt)')
km.set('n', '<C-space>', 'yy<C-del>p')
-- $insert keymaps
km.set('i', '<C-Space>', ' ')
km.set('i', '<C-x>', '<C-n>')
km.set('i', '<M-a>', '<C-o>$;')
km.set('i', '<C-z>', '<Esc>[s1z=A')
km.set('n', 'x', 'i')
km.set('n', 'X', 'I')
km.set('i', '<M-e>', '<C-o>:E<CR>')
km.set('i', '<M-r>', '<C-o>:R<CR>')
km.set('i', '<M-g>', '<C-o>:D<CR>')
km.set('i', '<M-j>', '<C-o>:J<CR>')
-- $navigation keymaps
km.set('n', '<Up>', 'gk')
km.set('n', '<Down>', 'gj')
-- km.set('i', '<Up>', '<C-o>gk')
-- km.set('i', '<Down>', '<C-o>gj')
km.set({'n', 'v'}, '<M-Up>', '5k')
km.set('v', '<S-M-Up>', '5k')
km.set({'n', 'v'}, '<M-Down>', '5j')
km.set('v', '<S-M-Down>', '5j')
km.set({'n', 'v'}, '<M-Left>', '5h')
km.set({'n', 'v'}, '<M-Right>', '5l')
km.set('i', '<M-up>', '<C-o>5k')
km.set('i', '<M-down>', '<C-o>5j')
km.set('i', '<M-left>', '<C-o>5h')
km.set('i', '<M-right>', '<C-o>5l')
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
km.set('n', '<C-c>', function()
    if (#vim.api.nvim_list_wins() < 2) then
        for _, ui in pairs(vim.api.nvim_list_uis()) do
            if ui.chan and not ui.stdout_tty then
            vim.fn.chanclose(ui.chan)
            end
        end
    else
        local function is_no_name_buf(buf)
            return 
        end
        local buf = vim.api.nvim_win_get_buf(0)
        if vim.bo[buf].readonly or (vim.api.nvim_buf_is_loaded(buf) and vim.api.nvim_buf_get_name(buf) == '') then
            vim.cmd('quit!')
        else
            vim.cmd('wq')
        end
    end
end, { noremap = true })
km.set('n', '<C-M-a>', function () vim.cmd('silent !$TERMINAL --title \'Terminal\'&') end)
km.set('n', '<C-M-x>', function () vim.cmd('silent !$TERMINAL --title \'Viewer\' -e zsh -c \'nvimserver; br\'&') end)
km.set('n', '<leader>k', function () vim.cmd('edit $NIXOS_CONFIG/home-manager/main.nix') end)
km.set('n', '<leader>K', function () vim.cmd('tabnew $NIXOS_CONFIG/home-manager/main.nix') end)
km.set('n', '<leader>l', function () vim.cmd('edit $NIXOS_CONFIG/dotfiles/nvim/init.lua') end)
km.set('n', '<leader>L', function () vim.cmd('tabnew $NIXOS_CONFIG/dotfiles/nvim/init.lua') end)
km.set('n', '<leader>n', function () vim.cmd('edit $NIXOS_LOCAL/home-local.nix') end)
km.set('n', '<leader>N', function () vim.cmd('edit $NIXOS_LOCAL/system-local.nix') end)
km.set('n', '<leader>ee', function ()
    ft = vim.bo.filetype
    vim.cmd('sp $NIXOS_CONFIG/dotfiles/nvim/UltiSnips/' .. ft .. '.snippets')
end)
km.set('n', '<leader>cd', function () vim.cmd('Copilot disable') end)
km.set('n', '<leader>ce', function () vim.cmd('Copilot enable') end)
km.set('n', 'Z', function () vim.cmd('ToggleBool') end)
km.set('n', '<M-s>', function () vim.cmd('silent Gitsigns preview_hunk_inline') end)

-- $telescope keymaps
local telescope = require('telescope.builtin')
local utils = require('telescope.utils')
km.set('n', '<M-?>', telescope.live_grep)
km.set('n', '<M-/>', telescope.current_buffer_fuzzy_find)
km.set('n', '<leader>:', telescope.commands)
km.set('n', '<leader>j', telescope.jumplist)
km.set('n', '<C-h>', telescope.help_tags)
km.set('n', '<C-_>', telescope.search_history)
km.set('n', '<C-q>', telescope.builtin)
km.set('n', '<C-g>', telescope.git_files)
km.set('n', '<C-x>', telescope.buffers)
km.set('n', '<C-d>', telescope.oldfiles)
km.set('n', '<C-f>', function () telescope.find_files({ cwd = utils.buffer_dir() }) end)
km.set('n', '<C-p>', function () telescope.find_files({ cwd = "~/projects" }) end)
vim.api.nvim_create_user_command('Files', function ()
    require('telescope').extensions.file_browser.file_browser()
end, {})
local state = require('telescope.state')
local action_state = require('telescope.actions.state')
local slow_scroll = function(prompt_bufnr, direction)
    local previewer = action_state.get_current_picker(prompt_bufnr).previewer
    local status = state.get_status(prompt_bufnr)
    -- Check if we actually have a previewer and a preview window
    if type(previewer) ~= 'table' or previewer.scroll_fn == nil or status.preview_win == nil then
        return
    end
    previewer:scroll_fn(1 * direction)
end
local fbactions = require('telescope._extensions.file_browser.actions')
require('telescope').setup({
    defaults = {
        mappings = {
            i = {
                ['<C-Down>'] = function(bufnr) slow_scroll(bufnr, 1) end,
                ['<C-Up>'] = function(bufnr) slow_scroll(bufnr, -1) end,
            },
        },
    },
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
			respect_gitignore = vim.fn.executable 'fd' == 1,
			no_ignore = false,
			follow_symlinks = true,
			browse_files = require('telescope._extensions.file_browser.finders').browse_files,
			browse_folders = require('telescope._extensions.file_browser.finders').browse_folders,
			hide_parent_dir = false,
			collapse_dirs = false,
			prompt_path = false,
			quiet = false,
			-- dir_icon = '',
			dir_icon = 'D',
			dir_icon_hl = 'Default',
			display_stat = { date = true, size = true, mode = true },
			hijack_netrw = true,
			use_fd = true,
			git_status = true,
			mappings = {
				['i'] = {
					['<C-a>'] = fbactions.create,
					['<S-CR>'] = fbactions.create_from_prompt,
					['<C-r>'] = fbactions.rename,
					['<C-x>'] = fbactions.move,
					['<C-v>'] = fbactions.copy,
					['<C-d>'] = fbactions.remove,
					['<C-s>'] = fbactions.open,
					['<C-w>'] = fbactions.goto_home_dir,
					['<C-CR>'] = fbactions.goto_cwd,
					['<C-e>'] = fbactions.change_cwd,
					['<C-f>'] = fbactions.toggle_browser,
					['<A-s>'] = fbactions.toggle_hidden,
					['<A-a>'] = fbactions.toggle_all,
					['<S-Left>'] = fbactions.goto_parent_dir,
				},
				['n'] = {
					['a'] = fbactions.create,
					['r'] = fbactions.rename,
					['x'] = fbactions.move,
					['c'] = fbactions.copy,
					['d'] = fbactions.remove,
					['s'] = fbactions.open,
					['g'] = fbactions.goto_parent_dir,
					['w'] = fbactions.goto_home_dir,
					['<CR>'] = fbactions.goto_cwd,
					['e'] = fbactions.change_cwd,
					['f'] = fbactions.toggle_browser,
					['h'] = fbactions.toggle_hidden,
					['t'] = fbactions.toggle_all,
				},
			},
        },
    }
})

-- REMAINDER --
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
            km.set(mode, l, r, opts)
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
-- $toggleterm setup
require('toggleterm').setup{
    size = function(term)
        if term.direction == 'horizontal' then
            return 20
        elseif term.direction == 'vertical' then
            return vim.o.columns * 0.4
        end
    end,
    open_mapping = [[<C-a>]],
    hide_numbers = true,
    autochdir = true,
    terminal_mappings = true,
    direction = 'float',
    shell = vim.o.shell,
    border = 'single'
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
-- $ibl setup
require('ibl').setup({
	indent = {
		char = '┊',
	},
	whitespace = {
		remove_blankline_trail = true,
	},
})
-- $lsp setup
km.set('n', 'gl', '<cmd>lua vim.diagnostic.open_float()<cr>')
km.set('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<cr>')
km.set('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<cr>') 
vim.api.nvim_create_autocmd('LspAttach', {
    desc = 'LSP actions',
    callback = function(event)
        local opts = { buffer = event.buf }
        km.set('n', 'K', '<cmd>lua vim.lsp.buf.hover()<cr>', opts)
        km.set('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', opts)
        km.set('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', opts)
        km.set('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<cr>', opts)
        km.set('n', 'go', '<cmd>lua vim.lsp.buf.type_definition()<cr>', opts)
        km.set('n', 'gr', '<cmd>lua vim.lsp.buf.references()<cr>', opts)
        km.set('n', 'gs', '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts)
        km.set('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<cr>', opts)
        km.set({'n', 'x'}, '<F3>', '<cmd>lua vim.lsp.buf.format({async = true})<cr>', opts)
        km.set('n', '<F4>', '<cmd>lua vim.lsp.buf.code_action()<cr>', opts)
    end
})
local lsp_capabilities = require('cmp_nvim_lsp').default_capabilities()
local default_setup = function(server)
    require('lspconfig')[server].setup({
        autostart = false,
        capabilities = lsp_capabilities,
    })
end
vim.api.nvim_create_user_command('LSP', 'LspStart', {})
-- vim.api.nvim_create_autocmd('BufWinEnter', {
--     command = 'LspStart'
-- })
require('mason').setup({})
require('mason-lspconfig').setup({
    ensure_installed = { 'pyright', 'bashls', 'rnix' },
    handlers = {
        default_setup,
    },
})
local luasnip = require('luasnip')
local cmp_ultisnips_mappings = require("cmp_nvim_ultisnips.mappings")
local cmp = require('cmp')
cmp.setup({
    sources = {
        { name = 'nvim_lsp' },
        { name = 'bashls' },
        { name = 'pyright' },
        { name = 'ultisnips' },
    },
    mapping = cmp.mapping.preset.insert({
        ['<Down>'] = cmp.mapping(function (fallback)
            cmp.close()
            fallback()
        end, { 'i' }),
        ['<Up>'] = cmp.mapping(function (fallback)
            cmp.close()
            fallback()
        end, { 'i' }),
        ['<C-Down>'] = cmp.mapping.select_next_item({}),
        ['<C-Up>'] = cmp.mapping.select_prev_item({}),
        ['<CR>'] = cmp.mapping.confirm({ select = false }),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-Right>'] = cmp.mapping(function(fallback)
            if luasnip.locally_jumpable(1) then
                luasnip.jump(1)
            else
                cmp_ultisnips_mappings.expand_or_jump_forwards(fallback)
            end
        end, { 'i', 's' }),
        ['<C-Left>'] = cmp.mapping(function(fallback)
            if luasnip.locally_jumpable(-1) then
                luasnip.jump(-1)
            else
                cmp_ultisnips_mappings.jump_backwards(fallback)
            end
        end, { 'i', 's' }),
    }),
    snippet = {
        expand = function(args)
            -- luasnip.lsp_expand(args.body)
            vim.fn["UltiSnips#Anon"](args.body)
        end,
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
vim.o.incsearch = true
vim.o.synmaxcol = 0
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.clipboard = 'unnamedplus'
vim.o.undofile = true
vim.o.cursorline = false

vim.cmd([[
let tex_flavor="latex"
set shiftwidth=4 smarttab
let g:omni_sql_no_default_maps = 1
autocmd BufEnter * set formatoptions-=cro
autocmd BufEnter * setlocal formatoptions-=cro
]])

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

-- $UltiSnips setup
-- vim.g.UltiSnipsExpandTrigger='<C-Right>'
-- vim.g.UltiSnipsJumpForwardTrigger='<C-Right>'
-- vim.g.UltiSnipsJumpBackwardTrigger='<C-Left>'
vim.g.UltiSnipsEditSplit='horizontal'

-- $markdown setup
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

package.path = package.path .. ';'..os.getenv('PROJECTS')..'/nvim-subfiles/lua/?.lua'
require('nvim-subfiles').setup({
    bindings = {
    },
    opts = {
        jump_to_file = false,
    }
})

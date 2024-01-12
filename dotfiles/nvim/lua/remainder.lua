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
		lualine_x = {'cdate', 'ctime', 'filetype'},
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

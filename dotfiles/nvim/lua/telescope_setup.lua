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

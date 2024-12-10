-- $telescope
local telescope = require('telescope.builtin')
-- local theme = require("telescope.themes").get_dropdown({})
vim.keymap.set('n', '<M-/>', function () telescope.live_grep({ search_dirs = { vim.fn.expand('%') } }) end)
vim.keymap.set('n', '<C-/>', function () telescope.grep_string{ search_dirs = { vim.fn.expand('%') }, shorten_path = true, word_match = "-w", only_sort_text = true, search = '' } end)
vim.keymap.set('n', '<C-M-/>', function () require('git_grep').live_grep() end)
vim.keymap.set('n', '<M-e>', function () telescope.jumplist() end)
vim.keymap.set('n', '<leader>/', function () telescope.search_history() end)
vim.keymap.set('n', '<leader>?', function () telescope.command_history() end)
vim.keymap.set('n', '<leader>:', function () telescope.commands() end)
vim.keymap.set('n', '<M-h>', function () telescope.help_tags() end)
vim.keymap.set('n', '<M-q>', function () telescope.builtin() end)
vim.keymap.set('n', '<M-g>', function () telescope.git_files() end)
vim.keymap.set('n', '<M-f>', function () telescope.find_files() end)
vim.keymap.set('n', '<M-x>', function () telescope.buffers({ previewer = true }) end)
local state = require('telescope.state')
local action_state = require('telescope.actions.state')
local slow_scroll = function(prompt_bufnr, direction)
    local previewer = action_state.get_current_picker(prompt_bufnr).previewer
    local status = state.get_status(prompt_bufnr)
    if type(previewer) ~= 'table' or previewer.scroll_fn == nil or status.preview_win == nil then
        return
    end
    previewer:scroll_fn(1 * direction)
end
vim.api.nvim_create_user_command('CS', telescope.colorscheme, {})
require('telescope').setup({
    defaults = {
        layout_config = {
          width = { padding = 0 },
          height = { padding = 0 },
        },
        color_devicons = false,
        mappings = {
            i = {
                ['<C-Down>'] = function(bufnr) slow_scroll(bufnr, 1) end,
                ['<C-Up>'] = function(bufnr) slow_scroll(bufnr, -1) end,
            },
        },
    },
    extensions = {
        git_grep = {
            cwd = '%:h:p',
            regex = 'extended',
            skip_binary_files = true,
            use_git_root = true
        }
    },
    pickers = {
        colorscheme = {
            enable_preview = true,
        },
    },
})
require('telescope').load_extension('git_grep')

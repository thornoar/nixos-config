-- $telescope
local telescope = require('telescope.builtin')
vim.keymap.set('n', '<M-/>', function () telescope.live_grep({ search_dirs = { vim.fn.expand('%') } }) end)
vim.keymap.set('n', '<C-/>', function () telescope.grep_string{ search_dirs = { vim.fn.expand('%') }, shorten_path = true, word_match = "-w", only_sort_text = true, search = '' } end)
vim.keymap.set('n', '<C-M-/>', function () require('git_grep').live_grep() end)
vim.keymap.set('n', '<M-e>', telescope.jumplist)
vim.keymap.set('n', '<leader>/', telescope.search_history)
vim.keymap.set('n', '<leader>?', telescope.command_history)
vim.keymap.set('n', '<leader>:', telescope.commands)
vim.keymap.set('n', '<M-h>', telescope.help_tags)
vim.keymap.set('n', '<M-q>', telescope.builtin)
vim.keymap.set('n', '<M-g>', telescope.git_files)
vim.keymap.set('n', '<M-f>', telescope.find_files)
vim.keymap.set('n', '<M-x>', function ()
    telescope.buffers({ previewer = true })
end)
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
        git_files = { theme = "ivy" },
        find_files = { theme = "ivy" },
        commands = { theme = "ivy" },
        command_history = { theme = "ivy" },
        search_history = { theme = "ivy" },
        help_tags = { theme = "ivy" },
        builtin = { theme = "ivy" },
        jumplist = { theme = "ivy" },
        live_grep = { theme = "ivy" },
        grep_string = { theme = "ivy" },
        colorscheme = {
            enable_preview = true,
        },
    },
})
require('telescope').load_extension('git_grep')

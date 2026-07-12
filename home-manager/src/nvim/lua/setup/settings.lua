-- $settings
vim.o.foldmethod = "marker"
vim.o.swapfile = false
vim.o.wrap = false
vim.o.linebreak = true
vim.o.list = false
vim.o.breakat = "   "
vim.opt.autochdir=true
vim.o.shell = "/usr/bin/env zsh"
vim.wo.number = true
vim.wo.relativenumber = false
vim.o.mouse = ""
vim.o.breakindent = true
vim.o.undofile = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.wo.signcolumn = "yes"
vim.o.updatetime = 1000
vim.o.timeout = false
vim.o.completeopt = "menuone,noselect"
vim.o.termguicolors = true
vim.o.cmdheight = 1
vim.o.ruler = false
vim.o.termguicolors = true
vim.o.scrolloff = 10
-- vim.o.colorcolumn = "20"
vim.o.expandtab = true
vim.o.compatible = false
vim.o.hlsearch = false
vim.o.incsearch = true
vim.o.synmaxcol = 0
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.clipboard = "unnamedplus"
vim.o.undofile = false
vim.o.cursorline = false
-- vim.g.neovide_transparency = 0.9
vim.o.laststatus = 3
vim.o.fixendofline = false
vim.o.fixeol = false

vim.cmd([[
    let tex_flavor="latex"
    " set shiftwidth=4 smarttab
    let g:omni_sql_no_default_maps = 1
    autocmd BufEnter * set formatoptions-=cro
    autocmd BufEnter * setlocal formatoptions-=cro
]])

local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
    callback = function()
        vim.highlight.on_yank()
    end,
    group = highlight_group,
    pattern = "*",
})

vim.g.typst_embedded_languages = { "haskell", "c", "java" }

local autosave = true

vim.api.nvim_create_autocmd({ 'BufEnter' }, {
    pattern = "*.*",
    callback = function ()
        local ft = vim.bo.filetype

        -- Comment settings

        if ft == "asy" then
            vim.o.commentstring = "//%s"
        end
        if ft == "hjson" then
            vim.o.commentstring = "#%s"
        end
        if ft == "tex" then
            vim.o.commentstring = "%%s"
            vim.o.textwidth = 70
        end

        -- Tab settings

        if
            ft == "haskell" or
            ft == "scala" or
            ft == "typst" or
            ft == "markdown" or
			ft == "tex" or
			ft == "nix" or
			ft == "coq" or
			ft == "clojure" or
			ft == "mlscript" or
			ft == "amy"
        then
            vim.o.tabstop = 2
            vim.o.shiftwidth = 2
            vim.o.softtabstop = 2
            vim.opt.spell = false
            vim.o.wrap = false
        else
            vim.o.tabstop = 4
            vim.o.shiftwidth = 4
            vim.o.softtabstop = 4
            vim.opt.spell = false
            vim.o.wrap = false
        end

        -- Spell settings

        if
            ft == "tex" or
			ft == "markdown" or
			ft == "typst" or
			ft == "text"
        then
            vim.o.wrap = true
            vim.opt.spell = true
            vim.opt.spelllang = { "en", "ru" }
        end

        -- Autosave settings

        if
            ft == "tex" or
			ft == "typst"
        then
            autosave = false
        end
    end
})

vim.api.nvim_create_user_command('AutoSave', function()
    autosave = not autosave
    print('autosave is ' .. (autosave and 'enabled' or 'disabled'))
end, {})
-- local autosavepattern = {
--     '*.asy', '*.md', '*.lua', '*.cpp', '*.py', '*.hs', '*.txt',
--     '*.r', '*.snippets', '*.nix', '*.hjson', '*.vim', '*.sh',
--     '*.html', '*.css', '*.c', '*.jl', '*.yml', '*.conf', '*.rs', '*.cabal'
-- }
vim.api.nvim_create_autocmd({ 'TextChanged', 'TextChangedI', 'TextChangedP' }, {
    pattern = '*.*',
    callback = function()
        if autosave and
        not vim.bo[vim.api.nvim_win_get_buf(0)].readonly and
        vim.bo[vim.api.nvim_win_get_buf(0)].buftype == '' then
            vim.cmd('silent write')
        end
    end
})

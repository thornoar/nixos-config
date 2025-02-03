-- $settings
vim.o.swapfile = false
vim.o.wrap = false
vim.o.linebreak = true
vim.o.list = false
vim.o.breakat = '   '
vim.opt.autochdir=true
vim.o.shell = '/usr/bin/env zsh'
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
-- vim.o.colorcolumn = "20"
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
vim.g.neovide_transparency = 0.9
vim.o.laststatus = 3

-- vim.g.python3_host_prog = "/etc/profiles/per-user/ramak/bin/python"

vim.g.vimtex_complete_enabled = 1
vim.g.vimtex_quickfix_enabled = 0
vim.g.vimtex_view_method = 'zathura'
-- vim.g.latex_view_general_viewer = 'zathura'
vim.g.vimtex_compiler_progname = 'nvr'
vim.g.vimtex_view_general_options = '-reuse-instance -forward-search @tex @line @pdf'
vim.g.vimtex_mappings_prefix = '\\'
-- vim.cmd([[let g:vimtex_compiler_latexmk = {'callback': 1, 'continuous': 1, 'aux_dir': '.aux', 'executable': 'latexmk', 'options': ['-verbose', '-synctex=1', '-interaction=nonstopmode', '-file-line-error']}]])

vim.cmd([[
    let tex_flavor="latex"
    " set shiftwidth=4 smarttab
    let g:omni_sql_no_default_maps = 1
    autocmd BufEnter * set formatoptions-=cro
    autocmd BufEnter * setlocal formatoptions-=cro
]])

local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
    callback = function()
        vim.highlight.on_yank()
    end,
    group = highlight_group,
    pattern = '*',
})

vim.api.nvim_create_autocmd('vimLeave', {
    pattern = '*',
    callback = function () vim.opt.guicursor = { 'a:ver25' } end
})

vim.g.typst_embedded_languages = {'haskell', 'c', 'java'}

vim.g.UltiSnipsEditSplit='horizontal'

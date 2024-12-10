vim.loader.enable()

local km = vim.keymap
vim.g.mapleader = ';'
km.set('n', 'ec', ':e $NIXOS_CONFIG/home-manager/src/nvim/init.lua<CR>')

-- $install
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
    'tpope/vim-fugitive',
    'sagarrakshe/toggle-bool',
    'farmergreg/vim-lastplace',
    'sirver/ultisnips',
    'neovimhaskell/haskell-vim',
    'numToStr/Comment.nvim',
    'ap/vim-css-color',
    'nanozuki/tabby.nvim',
    'lewis6991/gitsigns.nvim',
    'hjson/vim-hjson',
    'dkarter/bullets.vim',
    'chrisbra/csv.vim',
    'JuliaEditorSupport/julia-vim',
    {
        "folke/trouble.nvim",
        opts = {}, -- for default options, refer to the configuration section for custom setup.
        cmd = "Trouble",
        keys = {
            {
                "<M-S-d>",
                "<cmd>Trouble diagnostics toggle<cr>",
                desc = "Diagnostics (Trouble)",
            },
            {
                "<M-d>",
                "<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
                desc = "Buffer Diagnostics (Trouble)",
            },
        },
    },
    {
        'nvim-lualine/lualine.nvim',
    },
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
        'davvid/telescope-git-grep.nvim',
        dependencies = { "nvim-telescope/telescope.nvim" }
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
        internal_pairs={-- *ultimate-autopair-pairs-default-pairs*
            {'[',']',fly=true,dosuround=true,newline=true,space=true},
            {'(',')',fly=true,dosuround=true,newline=true,space=true},
            {'{','}',fly=true,dosuround=true,newline=true,space=true},
            {'"','"',suround=true,multiline=false},
            {'`','`', nft={'tex'},multiline=false},
            {'``',"''",ft={'tex'}},
            {'```','```',newline=true,ft={'markdown'}},
            {'<!--','-->',ft={'markdown','html'},space=true},
            {'"""','"""',newline=true,ft={'python'}},
            {"'''","'''",newline=true,ft={'python'}},
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
            'hrsh7th/cmp-nvim-lsp',
        },
    },
    { 'simrat39/rust-tools.nvim' },
    {
        'hrsh7th/nvim-cmp',
        dependencies = {
            'L3MON4D3/LuaSnip',
        },
    },
    {
        "quangnguyen30192/cmp-nvim-ultisnips",
        config = function()
            require("cmp_nvim_ultisnips").setup({})
        end,
    },
    { 'hrsh7th/cmp-buffer', },
    { 'hrsh7th/cmp-path', },
    { 'https://github.com/octaltree/cmp-look' },
    { 'folke/neodev.nvim', opts = {} },
    -- 'thornoar/nvim-subfiles',
}, {})

require("setup.neodev")
require("setup.commands")
require("setup.keymaps")
require("setup.telescope")
require("setup.comment")
require("setup.gitsigns")
require("setup.treesitter")
require("setup.toggleterm")

-- $ibl
require('ibl').setup({
    indent = {
        -- char = '┊',
        char = '|',
    },
    whitespace = {
        remove_blankline_trail = true,
    },
})

-- $lsp
vim.diagnostic.config({
    virtual_text = false
})
local symbols = { Error = "E", Info = "I", Hint = "H", Warn = "W" }
for name, icon in pairs(symbols) do
    local hl = "DiagnosticSign" .. name
    vim.fn.sign_define(hl, { text = icon, numhl = hl, texthl = hl })
end
km.set('n', '<C-S-d>', function () vim.diagnostic.goto_prev() end)
km.set('n', '<C-d>', function () vim.diagnostic.goto_next() end)
vim.api.nvim_create_autocmd('LspAttach', {
    desc = 'LSP actions',
    callback = function(event)
        local opts = { buffer = event.buf }
        km.set('n', '<C-Space>', function () vim.lsp.buf.hover() end, opts)
        -- km.set('n', '<C-M-CR>', function () vim.lsp.buf.references() end, opts)
        km.set('n', '<M-S-CR>', function () vim.lsp.buf.references() end, opts)
        km.set({ 'n', 'x' }, '<leader>cf', function () vim.lsp.buf.format({ async = true }) end, opts)
        km.set('n', '<leader>ca', function () vim.lsp.buf.code_action() end, opts)
    end
})

local lsp_capabilities = require('cmp_nvim_lsp').default_capabilities()
local hl_name = "FloatBorder"
-- local border = { '+', '-', '+', '|', '+', '-', '+', '|' }
local border = {
    { "╭", hl_name },
    { "─", hl_name },
    { "╮", hl_name },
    { "│", hl_name },
    { "╯", hl_name },
    { "─", hl_name },
    { "╰", hl_name },
    { "│", hl_name },
}
local handlers =  {
    ["textDocument/hover"] =  vim.lsp.with(vim.lsp.handlers.hover, { border = border }),
    ["textDocument/signatureHelp"] =  vim.lsp.with(vim.lsp.handlers.signature_help, { border = border }),
}

local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')
local util = require 'lspconfig.util'
local lspbasicconfig = {
    autostart = true,
    capabilities = lsp_capabilities,
    handlers = handlers,
    root_dir = function (_)
        return vim.loop.cwd()
    end,
}

if not configs.asy_ls then
    configs.asy_ls = {
        default_config = {
            cmd = {'asy', '-lsp'},
            filetypes = {'asy'},
            root_dir = function(fname)
                return util.find_git_ancestor(fname)
            end,
            single_file_support = true,
            settings = {},
        },
    }
end
lspconfig.asy_ls.setup(lspbasicconfig)

vim.diagnostic.config({
    float = { border = "rounded", },
})

lspconfig.texlab.setup(lspbasicconfig)
lspconfig.ts_ls.setup(lspbasicconfig)
lspconfig.pyright.setup(lspbasicconfig)
lspconfig.clojure_lsp.setup(lspbasicconfig)
-- lspconfig.nil_ls.setup(lspbasicconfig)
lspconfig.nixd.setup({
    handlers = handlers,
    capabilities = lsp_capabilities,
    cmd = { "nixd" },
    settings = {
        nixd = {
            nixpkgs = {
                expr = "import <nixpkgs { }>",
            },
            formatting = {
                command = { "alejandra" },
            },
            options = {
                nixos = {
                    expr = "(builtins.getFlake \"/home/ramak/projects/nixos-config\").nixosConfigurations.master.options",
                },
            },
        },
    },
})
lspconfig.r_language_server.setup(lspbasicconfig)
lspconfig.hls.setup(lspbasicconfig)
lspconfig.clangd.setup(lspbasicconfig)
lspconfig.bashls.setup(lspbasicconfig)

lspconfig.tinymist.setup({
    -- settings = {
    --     exportPdf = "never",
    -- },
    autostart = true,
    capabilities = lsp_capabilities,
    handlers = handlers,
    root_dir = function (_)
        return vim.loop.cwd()
    end,
})

lspconfig.rust_analyzer.setup({
    autostart = true,
    capabilities = lsp_capabilities,
    handlers = handlers,
    settings = {
        ['rust-analyzer'] = {
            check = {
                command = "clippy"
            },
            diagnostics = {
                enable = true
            }
        }
    }
})
local rt = require("rust-tools")
rt.setup({
    server = {
        on_attach = function(_, bufnr)
            -- Hover actions
            vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
            -- Code action groups
            vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
        end,
    },
})

lspconfig.lua_ls.setup({
    autostart = true,
    capabilities = lsp_capabilities,
    handlers = handlers,
    root_dir = function (_)
        return vim.loop.cwd()
    end,
    settings = {
        Lua = {
            diagnostics = {
                globals = { 'vim' }
            }
        }
    }
})

vim.api.nvim_create_user_command('LA', 'LspStart', {})
vim.api.nvim_create_user_command('LD', 'LspStop', {})
local luasnip = require('luasnip')
local cmp_ultisnips_mappings = require("cmp_nvim_ultisnips.mappings")
local cmp = require('cmp')
cmp.setup({
    sources = {
        { name = 'nvim_lsp' },
        { name = 'path' },
        { name = 'buffer' },
        { name = 'ultisnips' },
        {
            name = 'look',
            keyword_length = 2,
            option = {
                convert_case = true,
                loud = true,
                dict = os.getenv('WORDLIST')
            }
        },
        -- {
        --     name = "html-css",
        --     option = {
        --         enable_on = {
        --             'html'
        --         },
        --     },
        -- },
    },
    window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
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
        ['<C-x>'] = cmp.mapping.select_next_item({}),
        ['<C-Down>'] = cmp.mapping.select_next_item({}),
        ['<C-a>'] = cmp.mapping.select_prev_item({}),
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

-- vim.g.python3_host_prog = "/etc/profiles/per-user/ramak/bin/python"

-- vim.g.vimtex_complete_enabled = 1
-- vim.g.vimtex_quickfix_enabled = 0
-- -- vim.g.vimtex_view_method = 'zathura'
-- -- vim.g.latex_view_general_viewer = 'zathura'
-- -- vim.g.vimtex_compiler_progname = 'nvr'
-- vim.g.vimtex_view_general_options = '-reuse-instance -forward-search @tex @line @pdf'
-- vim.g.vimtex_mappings_prefix = '\\'
-- vim.cmd([[let g:vimtex_compiler_latexmk = {'continuous': 0, 'aux_dir': '.aux', 'options': ['-verbose', '-synctex=1', '-interaction=nonstopmode', '-file-line-error']}]])

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

package.path = package.path .. ';'..os.getenv('PROJECTS')..'/nvim-subfiles/lua/?.lua'
require('nvim-subfiles').setup({
    bindings = { },
    opts = {
        jump_to_file = false,
    }
})

local colors = require("colors")

require('tabby.tabline').set(function(line)
    return {
        line.tabs().foreach(function(tab)
            local hl = tab.is_current() and { fg = colors.colorMagenta0, bg = colors.bgColor0 } or { fg = colors.colorWhite3, bg = colors.bgColor0 }
            return {
                line.sep("", hl, { bg = colors.bgColor0 }),
                tab.name(),
                line.sep("", hl, { bg = colors.bgColor0 }),
                hl = hl,
                margin = ' ',
            }
        end),
        hl = { bg = colors.bgColor0 },
    }
end)

require('onedark').setup  {
    style = 'dark',
    colors = {
        bg0 = colors.bgColor0,
        bg1 = colors.bgColor1,
        bg2 = colors.bgColor2,
        bg3 = colors.bgColor3,
    },
    transparent = true,
    term_colors = true,
    ending_tildes = false,
    cmp_itemkind_reverse = false,
    toggle_style_key = '<leader>wt',
    toggle_style_list = { 'dark', 'darker', 'cool', 'deep', 'warm', 'warmer', 'light' },
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
    highlights = {},
}

vim.cmd.colorscheme 'onedark'

vim.cmd( [[
    highlight Function guifg=burlywood
    highlight Number guifg=lightsteelblue
    highlight Include guifg=orchid
    highlight Type guifg=lightseagreen
    highlight Constant guifg=palevioletred gui=italic cterm=italic
    highlight Operator guifg=aquamarine
    highlight Keyword guifg=plum
]])

local function keymap()
    if vim.opt.iminsert:get() > 0 and vim.b.keymap_name then
        return vim.b.keymap_name
    end
    return 'en'
end
require('lualine').setup{
    options = {
        icons_enabled = true,
        theme = {
            normal = {
                a = { fg = colors.bgColor0, bg = colors.colorMagenta1, gui = 'bold' },
                b = { fg = colors.colorWhite3, bg = colors.bgColor0 },
                c = { fg = colors.colorWhite3, bg = colors.bgColor0 },
            },
            command = { a = { fg = colors.bgColor0, bg = colors.colorYellow1, gui = 'bold' } },
            insert = { a = { fg = colors.bgColor0, bg = colors.colorRed1, gui = 'bold' } },
            visual = { a = { fg = colors.bgColor0, bg = colors.colorMagenta0, gui = 'bold' } },
            terminal = { a = { fg = colors.bgColor0, bg = colors.colorCyan, gui = 'bold' } },
            replace = { a = { fg = colors.bgColor0, bg = colors.colorMagenta1, gui = 'bold' } },
            inactive = {
                a = { fg = colors.bgColor0, bg = colors.bgColor0, gui = 'bold' },
                b = { fg = colors.bgColor0, bg = colors.bgColor0 },
                c = { fg = colors.bgColor0, bg = colors.bgColor0 },
            },
        },
        component_separators = '|',
        section_separators = "",
    },
    sections = {
        lualine_a = { 'mode' },
        lualine_b = { 'branch', 'diff', 'diagnostics' },
        lualine_c = { 'filename', keymap },
        lualine_x = { 'filetype' },
        lualine_y = { 'progress' },
        lualine_z = { 'location' },
    },
}

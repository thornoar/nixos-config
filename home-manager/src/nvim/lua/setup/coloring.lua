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

vim.cmd([[
    highlight Function guifg=burlywood
    highlight Number guifg=lightsteelblue
    highlight Include guifg=orchid
    highlight Type guifg=lightseagreen
    highlight Constant guifg=palevioletred gui=italic cterm=italic
    highlight Operator guifg=aquamarine
    highlight Keyword guifg=plum
]])
vim.api.nvim_set_hl(0, "WinSeparator", { fg = colors.colorMagenta1, bg = colors.bgColor0 })

local function keymap()
    if vim.opt.iminsert:get() > 0 and vim.b.keymap_name then
        return vim.b.keymap_name
    end
    return 'en'
end
require('lualine').setup{
    options = {
        icons_enabled = false,
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
        lualine_b = { 'branch', 'diff', {
                'diagnostics',
                symbols = {error = 'E ', warn = 'W ', info = 'I ', hint = 'H '},
                update_in_insert = true, -- Update diagnostics in insert mode.
            }
        },
        lualine_c = { 'filename', keymap },
        lualine_x = { 'filetype' },
        lualine_y = { 'progress' },
        lualine_z = { 'location' },
    },
}

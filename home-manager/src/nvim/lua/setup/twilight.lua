local colors = require("colors")
require("twilight").setup({
    dimming = {
        alpha = 1.0, -- amount of dimming
        -- we try to get the foreground from the highlight groups or fallback color
        color = { colors.commentColor },
        -- color = { "Normal", "#ffffff" },
        -- term_bg = colors.bgColor3, -- if guibg=NONE, this will be used to calculate text color
        inactive = true, -- when true, other windows will be fully dimmed (unless they contain the same buffer)
    },
    context = 10,
    treesitter = true,
    expand = {
        "function",
        "method",
        "table",
        "if_statement",
    },
    exclude = {}, -- exclude these filetypes
})

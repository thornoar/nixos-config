-- $comment
require('Comment').setup({
    ignore = nil,
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

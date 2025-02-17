package.path = package.path .. ';'..os.getenv('PROJECTS')..'/nvim-subfiles/lua/?.lua'
require('nvim-subfiles').setup({
    bindings = { },
    opts = {
        jump_to_file = false,
    }
})

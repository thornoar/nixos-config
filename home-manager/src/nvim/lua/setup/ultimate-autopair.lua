return {
    "altermo/ultimate-autopair.nvim",
    -- event = { "InsertEnter" },
    branch = "v0.6",
    opts = {
        space2 = { enable = true },
        tabout = { enable = true },
        fastwarp = {
            enable = true,
            enable_normal = true,
            enable_reverse = true,
            hopout = false,
            faster = false,
            map = "<M-/>",
        },
        internal_pairs = {-- *ultimate-autopair-pairs-default-pairs*
            {'[',']',fly=true,dosuround=false,newline=true,space=true},
            {'(',')',fly=true,dosuround=false,newline=true,space=true},
            {'<','>',fly=true,dosuround=false,newline=true,space=false, ft = {"html","markdown"}},
            {'{','}',fly=true,dosuround=false,newline=true,space=true},
            {'"','"',suround=false,multiline=false},
            {'`','`', nft={'tex'},multiline=false},
            {'``',"''",ft={'tex'}},
            {'```','```',newline=true,ft={'markdown'}},
            {'<!--','-->',ft={'markdown','html'},space=true},
            {'"""','"""',newline=true,ft={'python'}},
            {"'''","'''",newline=true,ft={'python'}},
        },
    }
}

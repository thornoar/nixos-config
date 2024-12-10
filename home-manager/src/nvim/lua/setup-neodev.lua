-- $neodev
require("neodev").setup({
    override = function(root_dir, library)
        if root_dir:find(os.getenv("NIXOS_CONFIG") .. "/home-manager/src/nvim", 1, true) == 1 then
            library.enabled = true
            library.plugins = true
        end
    end,
})

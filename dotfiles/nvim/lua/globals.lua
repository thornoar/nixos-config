newcmd = function (name, command)
	vim.api.nvim_create_user_command(name, command, {})
end

km = vim.keymap

P = function (v)
    print(vim.inspect(v))
    return v
end

RELOAD = function (...)
    return require("plenary.reload").reload_module(...)
end

R = function (name)
    RELOAD(name)
    return require(name)
end

home = '$PROJECTS'
testdir = home..'/sandbox'

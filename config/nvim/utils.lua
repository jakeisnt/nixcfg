-- from https://github.com/icyphox/dotfiles/blob/c17e7b07be8a85f9c58feaf7f6bd9aa220647c3e/config/nvim/lua/utils.lua
local M = {}
local cmd = vim.cmd

function M.create_augroup(autocmds, name)
    cmd('augroup ' .. name)
    cmd('autocmd!')
    for _, autocmd in ipairs(autocmds) do
        cmd('autocmd ' .. table.concat(autocmd, ' '))
    end
    cmd('augroup END')
end

return M

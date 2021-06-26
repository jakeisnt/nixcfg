local gl = require 'galaxyline'
local hsl = require('lush').hsl
local condition = require('galaxyline.condition')
local gls = gl.section
gl.short_line_list = {
    'startify', 'undotree', 'fugitive', 'fugitiveblame', 'startuptime',
    'NvimTree', 'vista', 'dbui', 'packer'
}

local clrs = {
    nord0 = hsl(220, 16, 22),
    nord1 = hsl(222, 16, 28),
    nord2 = hsl(220, 17, 32),
    nord3 = hsl(220, 16, 36),
    nord3_bright = hsl(220, 17, 46),
    nord4 = hsl(219, 28, 88),
    nord5 = hsl(218, 27, 92),
    nord6 = hsl(218, 27, 94),
    nord7 = hsl(179, 25, 65),
    nord8 = hsl(193, 43, 67),
    nord9 = hsl(210, 34, 63),
    nord10 = hsl(213, 32, 52),
    nord11 = hsl(354, 42, 56),
    nord12 = hsl(14, 51, 63),
    nord13 = hsl(40, 71, 73),
    nord14 = hsl(92, 28, 65),
    nord15 = hsl(311, 20, 63)
}

local colors = {
    bg = clrs.nord0.da(10).hex,
    blue = clrs.nord9.hex,
    cyan = clrs.nord8.hex,
    darkblue = clrs.nord10.hex,
    fg = clrs.nord4.hex,
    green = clrs.nord14.hex,
    magenta = clrs.nord15.hex,
    orange = clrs.nord12.hex,
    red = clrs.nord11.hex,
    violet = clrs.nord15.da(10).hex,
    yellow = clrs.nord13.hex
}

local mode_color = {
    n = colors.red,
    i = colors.green,
    v = colors.blue,
    [''] = colors.blue,
    V = colors.blue,
    c = colors.magenta,
    no = colors.red,
    s = colors.orange,
    S = colors.orange,
    [''] = colors.orange,
    ic = colors.yellow,
    R = colors.violet,
    Rv = colors.violet,
    cv = colors.red,
    ce = colors.red,
    r = colors.cyan,
    rm = colors.cyan,
    ['r?'] = colors.cyan,
    ['!'] = colors.red,
    t = colors.red
}

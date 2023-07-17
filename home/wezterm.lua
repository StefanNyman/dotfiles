local wezterm = require 'wezterm'

local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.automatically_reload_config = true
config.font = wezterm.font 'Fira Code'
config.color_scheme = 'GruvboxDark'
config.enable_tab_bar = false
config.audible_bell = "Disabled"
config.enable_scroll_bar = false
config.visual_bell = {
  fade_in_function = 'EaseIn',
  fade_in_duration_ms = 50,
  fade_out_function = 'EaseOut',
  fade_out_duration_ms = 50,
}
config.window_padding = {
  left = 0,
  right = 1,
  top = 0,
  bottom = 0,
}
config.window_decorations = "NONE"
config.default_prog = { '/usr/bin/fish', '-l'}


return config

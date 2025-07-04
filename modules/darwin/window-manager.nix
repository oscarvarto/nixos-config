{ config, pkgs, lib, ... }:

{
  services = {
    jankyborders = {
      enable = true;
      active_color = "0xff00ff00";
      inactive_color = "0xff494d64";
      width = 10.0;
    };

    yabai = {
      enable = true;
      enableScriptingAddition = true;
      config = {
        mouse_follows_focus = "on";
        focus_follows_mouse = "autofocus";
        display_arrangement_order = "horizontal";
        window_origin_display = "default";
        window_placement = "first_child";
        window_zoom_persist = "on";
        window_shadow = "off";
        window_animation_duration = "0.0";  # Faster window operations
        window_animation_easing = "ease_out_circ";
        window_opacity_duration = "0.2";
        active_window_opacity = "1.0";
        normal_window_opacity = "0.70";
        window_opacity = "off";
        insert_feedback_color = "0xffd75f5f";
        split_ratio = "0.70";
        split_type = "auto";
        auto_balance = "off";
        # Add window borders for easier identification
        window_border = "on";
        window_border_width = "4";
        active_window_border_color = "0xFF40FF00";
        normal_window_border_color = "0x00FFFFFF";
        # Adjust padding for different layouts
        external_bar = "all:0:0";
        top_padding = 20;
        bottom_padding = 20;
        left_padding = 20;
        right_padding = 20;
        window_gap = 20;
        layout = "bsp float";
        mouse_modifier = "fn";
        mouse_action1 = "move";
        mouse_action2 = "resize";
        mouse_drop_action = "swap";
      };
      extraConfig = ''
        yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
        sudo yabai --load-sa

        #### Ghostty ####
        yabai -m signal --add app='^Ghostty$' event=window_created action='yabai -m space --layout bsp'
        yabai -m signal --add app='^Ghostty$' event=window_destroyed action='yabai -m space --layout bsp'

        # yabai -m rule --add app="^Google Chrome$" space=^10
        # float system preferences
        yabai -m rule --add app="^System Settings$" manage=off
        yabai -m rule --add title="Zoom Workplace" manage=off
        yabai -m rule --add title="Zoom Meeting" manage=off

        # Emacs specific rules
        # yabai -m rule --add app="^Emacs$" manage=on space=^1

        borders &
      '';
    };
  };
}

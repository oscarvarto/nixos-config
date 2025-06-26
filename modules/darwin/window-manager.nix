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
         window_placement = "second_child";
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

    skhd = {
      enable = true;
      skhdConfig = ''
        # Example and documentation here: https://github.com/koekeishiya/yabai/blob/master/examples/skhdrc
        cmd + ctrl + shift - r : skhd -r
        cmd + alt - 1  : yabai -m space --focus 1
        cmd + alt - 2  : yabai -m space --focus 2
        cmd + alt - 3  : yabai -m space --focus 3
        cmd + alt - 4  : yabai -m space --focus 4
        cmd + alt - 5  : yabai -m space --focus 5
        cmd + alt - 6  : yabai -m space --focus 6
        cmd + alt - 7  : yabai -m space --focus 7
        cmd + alt - 8  : yabai -m space --focus 8
        cmd + alt - 9  : yabai -m space --focus 9
        cmd + alt - 0  : yabai -m space --focus 10

        # send window to desktop and follow focus
        shift + cmd + alt - 1  : yabai -m window --space  1; yabai -m space --focus  1
        shift + cmd + alt - 2  : yabai -m window --space  2; yabai -m space --focus  2
        shift + cmd + alt - 3  : yabai -m window --space  3; yabai -m space --focus  3
        shift + cmd + alt - 4  : yabai -m window --space  4; yabai -m space --focus  4
        shift + cmd + alt - 5  : yabai -m window --space  5; yabai -m space --focus  5
        shift + cmd + alt - 6  : yabai -m window --space  6; yabai -m space --focus  6
        shift + cmd + alt - 7  : yabai -m window --space  7; yabai -m space --focus  7
        shift + cmd + alt - 8  : yabai -m window --space  8; yabai -m space --focus  8
        shift + cmd + alt - 9  : yabai -m window --space  9; yabai -m space --focus  9
        shift + cmd + alt - 0  : yabai -m window --space 10; yabai -m space --focus 10

        # options: zoom-parent, zoom-fullscreen, native-fullscreen
        ctrl + alt + shift - f : yabai -m window --toggle native-fullscreen

        # • Status: launchctl list | grep yabai

        # yabai --restart-service
        ctrl + alt + shift - r : launchctl kickstart -k gui/$(id -u)/org.nixos.yabai

        # yabai --start-service
        ctrl + alt + shift - s : launchctl start gui/$(id -u)/org.nixos.yabai

        # yabai --stop-service
        ctrl + alt + shift - t : launchctl stop gui/$(id -u)/org.nixos.yabai
     '';
    };
  };
}

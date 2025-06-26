{ config, pkgs, lib, inputs, ... }:

# For now, let's provide a basic working neovim package until nixCats compatibility is resolved
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    
    # Use nightly neovim
    package = inputs.neovim-nightly-overlay.packages.${pkgs.stdenv.hostPlatform.system}.neovim;
    
    extraPackages = with pkgs; [
      # LSP servers
      lua-language-server
      stylua
      nixd
      alejandra
      
      # Development tools
      lazygit
      git
      ripgrep
      fd
      
      # Language support
      nodejs
      python3
    ];
  };
  
  # Copy the lua configuration to the appropriate location
  home.file.".config/nvim" = {
    source = ./nixCats;
    recursive = true;
  };
}

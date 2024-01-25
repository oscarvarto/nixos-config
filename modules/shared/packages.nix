{ pkgs }:

with pkgs; [
  # General packages for development and system management
  # act
  # alacritty
  aspell
  aspellDicts.en
  aspellDicts.es
  bash-completion
  bat
  cmake
  coreutils
  killall
  ktfmt
  neofetch
  nixfmt
  openssh
  pandoc
  sqlite
  shellcheck
  shfmt
  stylelint
  wget
  # zip

  # Encryption and security tools
  age
  # age-plugin-yubikey
  # gnupg # 2.4.1
  libfido2
  pinentry
  # yubikey-manager

  # Media-related packages
  dejavu_fonts
  ffmpeg
  fd
  font-awesome
  hack-font
  noto-fonts
  noto-fonts-emoji
  meslo-lgs-nf

  # Node.js development tools
  nodePackages.nodemon
  nodePackages.prettier
  nodePackages.npm # globally install npm
  nodejs

  # Text and terminal utilities
  htop
  hunspell
  hunspellDicts.es_MX
  iftop
  jetbrains-mono
  jq
  ripgrep
  tree
  # tmux
  unrar
  unzip
  zsh-powerlevel10k

  # Python packages
  python311
  python311Packages.virtualenv # globally install virtualenv
]

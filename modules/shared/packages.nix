{ pkgs }:

with pkgs; 
[
  # General packages for development and system management
  aspell
  aspellDicts.en
  bash-completion
  btop
  coreutils
  curl
  killall
  neofetch
  openssh
  sqlite
  wget
  zip

  # Encryption and security tools
  age
  age-plugin-yubikey
  gnupg
  libfido2

  # Cloud-related tools and SDKs
  docker
  docker-compose

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
  nodePackages.npm # globally install npm
  nodePackages.prettier
  nodejs

  # Text and terminal utilities
  htop
  hunspell
  iftop
  jetbrains-mono
  jq
  (ripgrep.override {withPCRE2 = true;})
  isync
  neovim
  nil
  nurl
  tree
  unrar
  unzip

  # Python packages
  python3
  virtualenv
]

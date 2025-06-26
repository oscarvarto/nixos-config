{ config, pkgs, lib, ... }:

let 
  name = "Oscar Vargas Torres";
  user = "oscarvarto";
  email = "contact@oscarvarto.mx";
in
{
  # Import modular configurations
  imports = [
    ./shell-config.nix
    ./git-ignores.nix
  ];

  git = {
    enable = true;
    ignores = [
      # Vim/Neovim temporary files
      "*.swp"
      "*.swo"
      "*~"
      ".*.swp"
      ".*.swo"
 
      # macOS specific
      ".DS_Store"
      ".DS_Store?"
      "._*"
      ".Spotlight-V100"
      ".Trashes"
      "ehthumbs.db"
      "Thumbs.db"
      ".AppleDouble"
      ".LSOverride"
      ".DocumentRevisions-V100"
      ".fseventsd"
      ".VolumeIcon.icns"
      ".com.apple.timemachine.donotpresent"
 
      # Linux specific
      "*~"
      ".fuse_hidden*"
      ".directory"
      ".Trash-*"
      ".nfs*"
 
      # IDE and Editor files
      # JetBrains IDEs (IntelliJ, IDEA, CLion, etc.)
      ".idea/"
      "*.iml"
      "*.ipr"
      "*.iws"
      ".idea_modules/"
      "atlassian-ide-plugin.xml"
 
      # VSCode
      ".vscode/"
      "!.vscode/settings.json"
      "!.vscode/tasks.json"
      "!.vscode/launch.json"
      "!.vscode/extensions.json"
      "*.code-workspace"
 
      # Emacs
      "*~"
      "\\#*\\#"
      "/.emacs.desktop"
      "/.emacs.desktop.lock"
      "*.elc"
      "auto-save-list"
      "tramp"
      ".\\#*"
      ".org-id-locations"
      "*_archive"
      "*_flymake.*"
      "/eshell/history"
      "/eshell/lastdir"
      "/elpa/"
      "*.rel"
      "/auto/"
      ".cask/"
      "dist/"
      "flycheck_*.el"
      "/server/"
      ".projectile"
 
      # Programming Languages
      # Java
      "*.class"
      "*.log"
      "*.ctxt"
      ".mtj.tmp/"
      "*.jar"
      "*.war"
      "*.nar"
      "*.ear"
      "*.zip"
      "*.tar.gz"
      "*.rar"
      "hs_err_pid*"
      "replay_pid*"
 
      # Scala
      "*.class"
      "*.log"
      "project/project/"
      "project/target/"
      "target/"
      ".bsp/"
      ".metals/"
      ".bloop/"
      "metals.sbt"
      ".scala-build/"
 
      # Clojure
      "pom.xml"
      "pom.xml.asc"
      "*.jar"
      "*.class"
      "/lib/"
      "/classes/"
      "/target/"
      "/checkouts/"
      ".lein-deps-sum"
      ".lein-repl-history"
      ".lein-plugins/"
      ".lein-failures"
      ".nrepl-port"
      ".cpcache/"
 
      # Python
      "__pycache__/"
      "*.py[cod]"
      "*$py.class"
      "*.so"
      ".Python"
      "build/"
      "develop-eggs/"
      "dist/"
      "downloads/"
      "eggs/"
      ".eggs/"
      "lib/"
      "lib64/"
      "parts/"
      "sdist/"
      "var/"
      "wheels/"
      "*.egg-info/"
      ".installed.cfg"
      "*.egg"
      "MANIFEST"
      "*.manifest"
      "*.spec"
      "pip-log.txt"
      "pip-delete-this-directory.txt"
      ".tox/"
      ".nox/"
      ".coverage"
      ".coverage.*"
      ".cache"
      "nosetests.xml"
      "coverage.xml"
      "*.cover"
      ".hypothesis/"
      ".pytest_cache/"
      "*.py,cover"
      ".env"
      ".venv"
      "env/"
      "venv/"
      "ENV/"
      "env.bak/"
      "venv.bak/"
 
      # C#/.NET
      "bin/"
      "obj/"
      "*.user"
      "*.suo"
      "*.userosscache"
      "*.sln.docstates"
      "[Dd]ebug/"
      "[Dd]ebugPublic/"
      "[Rr]elease/"
      "[Rr]eleases/"
      "x64/"
      "x86/"
      "[Ww][Ii][Nn]32/"
      "[Aa][Rr][Mm]/"
      "[Aa][Rr][Mm]64/"
      "bld/"
      "[Bb]in/"
      "[Oo]bj/"
      "[Ll]og/"
      "[Ll]ogs/"
      "*.tmp"
      "*.tmp_proj"
      "*_wpftmp.csproj"
      "*.log"
      "*.vspscc"
      "*.vssscc"
      ".builds"
      "*.pidb"
      "*.svclog"
      "*.scc"
 
      # Build and dependency management
      "node_modules/"
      "npm-debug.log*"
      "yarn-debug.log*"
      "yarn-error.log*"
      ".npm"
      ".eslintcache"
      ".next"
      ".nuxt"
      "dist"
      ".cache/"
      ".parcel-cache"
 
      # Environment and secrets
      ".env"
      ".env.local"
      ".env.development.local"
      ".env.test.local"
      ".env.production.local"
      "*.pem"
      "*.key"
      "*.p12"
      "*.jks"
 
      # Logs and temporary files
      "*.log"
      "logs"
      "*.tmp"
      "*.temp"
      "*.pid"
      "*.seed"
      "*.pid.lock"
 
      # Version control
      ".svn/"
      ".hg/"
      ".bzr/"
 
      # Archives
      "*.7z"
      "*.dmg"
      "*.gz"
      "*.iso"
      "*.jar"
      "*.rar"
      "*.tar"
      "*.zip"
    ];
    userName = name;
    # userEmail is handled by conditional includes based on directory
    lfs = {
      enable = true;
    };
    extraConfig = {
      init.defaultBranch = "main";
      core = {
        editor = "nvim";
        autocrlf = false;  # Better for macOS/Linux - preserves line endings as-is
        eol = "lf";        # Use LF line endings on macOS/Linux
        ignorecase = false; # Case-sensitive file names (better for cross-platform)
        # hooksPath removed - now configured conditionally
      };
      commit.gpgsign = false;
      diff.colorMoved = "zebra"; # https://spin.atomicobject.com/git-configurations-default/
      fetch.prune = true;
      pull.rebase = true;
      push.autoSetupRemote = true;
      rebase.autoStash = true;
      safe.directory = [
        "*"  # Trust all directories (most comprehensive)
        "/Users/${user}/nixos-config"
        "/nix/store/*"
        "/opt/homebrew/*"
      ];
      # Conditional includes for work directory
      includeIf."gitdir:/Users/${user}/ir/**".path = "/Users/${user}/.local/share/git/config-work";
      # Conditional include for Doom Emacs directory (with hooks)
      includeIf."gitdir:/Users/${user}/.emacs.d/".path = "/Users/${user}/.config/git/config-doom";
      # Include personal config as fallback for all other directories
      include.path = "/Users/${user}/.config/git/config-personal";
    };
  };

  ssh = {
    enable = true;
    includes = [
      (lib.mkIf pkgs.stdenv.hostPlatform.isLinux
        "/home/${user}/.ssh/config_external"
      )
      (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin
        "/Users/${user}/.ssh/config_external"
      )
    ];
    matchBlocks = {
      "github.com" = {
        identitiesOnly = true;
        identityFile = [
          (lib.mkIf pkgs.stdenv.hostPlatform.isLinux
            "/home/${user}/.ssh/id_ed25519"
          )
          (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin
            "/Users/${user}/.ssh/id_ed25519"
          )
        ];
      };
    };
  };
}

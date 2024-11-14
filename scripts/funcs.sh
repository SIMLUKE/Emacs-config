function install_dependencies() {
    PS3="Please select your OS : "
    options=("Arch" "Fedora" "Debian" "Ubuntu" "SKIP (will break lsp mode)")
    select commitPrefix in "${options[@]}"; do
        case $commitPrefix in
        "Fedora")
            sudo dnf install clang-tools-extra
            sudo dnf install pyright
            sudo dnf install cmake
            break
            ;;
        "Arch")
            sudo pacman -S pyright
            sudo pacman -S clang
            sudo pacman -S cmake
            break
            ;;
        "Debian")
            sudo apt install clangd
            sudo apt install cmake
            echo "install \"Pyright\" pls"
            break
            ;;
        "Ubuntu")
            sudo apt install clangd
            sudo apt install cmake
            echo "install \"Pyright\" pls"
            break
            ;;
        "SKIP (will break lsp mode)")
            echo "Skipped"
            break
            ;;
        *)
            echo "Invalid choice. Please select a valid option."
            ;;
        esac
    done
}

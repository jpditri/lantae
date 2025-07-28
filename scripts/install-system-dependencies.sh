#!/bin/bash
set -euo pipefail

# System Dependencies Installation Script
# Installs core system tools and dependencies for Lantae

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "${LOG_FILE:-install.log}"
}

show_progress() {
    echo -e "${BLUE}[DEPS] $1${NC}"
    log "DEPS: $1"
}

show_success() {
    echo -e "${GREEN}✓ [DEPS] $1${NC}"
    log "DEPS SUCCESS: $1"
}

error_exit() {
    echo -e "${RED}ERROR [DEPS]: $1${NC}" >&2
    log "DEPS ERROR: $1"
    exit 1
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Package manager detection and installation
install_package_manager() {
    case "${OS:-unknown}" in
        "macos")
            if ! command_exists brew; then
                show_progress "Installing Homebrew package manager..."
                /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" || error_exit "Failed to install Homebrew"
                
                # Add Homebrew to PATH for M1/M2 Macs
                if [[ -f "/opt/homebrew/bin/brew" ]]; then
                    echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zshrc
                    eval "$(/opt/homebrew/bin/brew shellenv)"
                fi
                show_success "Homebrew installed"
            else
                show_success "Homebrew already installed"
            fi
            ;;
        "debian")
            show_progress "Updating package lists..."
            sudo apt-get update -qq || error_exit "Failed to update package lists"
            show_success "Package lists updated"
            ;;
        "redhat")
            show_progress "Updating package manager..."
            if command_exists dnf; then
                sudo dnf check-update -q || true
            elif command_exists yum; then
                sudo yum check-update -q || true
            else
                error_exit "No supported package manager found (dnf/yum)"
            fi
            show_success "Package manager updated"
            ;;
        "arch")
            show_progress "Updating package database..."
            sudo pacman -Sy --noconfirm || error_exit "Failed to update package database"
            show_success "Package database updated"
            ;;
        *)
            error_exit "Unsupported OS: ${OS:-unknown}"
            ;;
    esac
}

# Install core system packages
install_core_packages() {
    local packages_to_install=()
    
    show_progress "Checking core system dependencies..."
    
    case "${OS:-unknown}" in
        "macos")
            # Check and collect missing packages for macOS
            ! command_exists git && packages_to_install+=("git")
            ! command_exists curl && packages_to_install+=("curl")
            ! command_exists wget && packages_to_install+=("wget")
            ! command_exists make && packages_to_install+=("make")
            ! command_exists gcc && packages_to_install+=("gcc")
            
            if [[ ${#packages_to_install[@]} -gt 0 ]]; then
                show_progress "Installing core packages: ${packages_to_install[*]}"
                brew install "${packages_to_install[@]}" || error_exit "Failed to install core packages"
            fi
            
            # Install Xcode command line tools if needed
            if ! xcode-select -p >/dev/null 2>&1; then
                show_progress "Installing Xcode command line tools..."
                xcode-select --install || show_progress "Xcode tools installation initiated (may require manual confirmation)"
            fi
            ;;
            
        "debian")
            # Essential packages for Debian/Ubuntu/Raspberry Pi
            local debian_packages=(
                "build-essential"
                "git"
                "curl"
                "wget"
                "software-properties-common"
                "apt-transport-https"
                "ca-certificates"
                "gnupg"
                "lsb-release"
                "libssl-dev"
                "libreadline-dev"
                "zlib1g-dev"
                "libyaml-dev"
                "libxml2-dev"
                "libxslt1-dev"
                "libffi-dev"
            )
            
            # Add Raspberry Pi specific packages if ARM architecture detected
            if [[ -f /proc/cpuinfo ]] && grep -q "ARM\|arm" /proc/cpuinfo; then
                show_progress "ARM architecture detected (possibly Raspberry Pi)"
                debian_packages+=(
                    "bc"                    # Basic calculator for memory checks
                    "libc6-dev"            # C library development files
                    "libgmp-dev"           # GNU Multiple Precision Arithmetic Library
                    "libncurses5-dev"      # Terminal handling library
                    "autoconf"             # Automatic configure script builder
                    "bison"                # Parser generator
                    "automake"             # Tool for generating Makefiles
                    "libtool"              # Generic library support script
                    "pkg-config"           # Package configuration helper
                )
                
                # For very limited memory systems, suggest alternatives
                if [[ -f /proc/meminfo ]]; then
                    local mem_gb
                    mem_gb=$(awk '/MemTotal/ {printf "%.1f", $2/1024/1024}' /proc/meminfo)
                    if (( $(echo "$mem_gb < 1.5" | bc -l 2>/dev/null || echo "1") )); then
                        show_progress "Low memory system detected (${mem_gb}GB)"
                        show_progress "Consider using system Ruby packages instead of compiling"
                        show_progress "Alternative: sudo apt install -y ruby ruby-dev ruby-bundler"
                    fi
                fi
            fi
            
            for package in "${debian_packages[@]}"; do
                if ! dpkg -l | grep -q "^ii  $package "; then
                    packages_to_install+=("$package")
                fi
            done
            
            if [[ ${#packages_to_install[@]} -gt 0 ]]; then
                show_progress "Installing packages: ${packages_to_install[*]}"
                sudo apt-get install -y "${packages_to_install[@]}" || error_exit "Failed to install packages"
            fi
            ;;
            
        "redhat")
            # Essential packages for CentOS/RHEL/Fedora
            local redhat_packages=(
                "git"
                "curl"
                "wget"
                "gcc"
                "gcc-c++"
                "make"
                "openssl-devel"
                "readline-devel"
                "zlib-devel"
                "libyaml-devel"
                "libxml2-devel"
                "libxslt-devel"
                "libffi-devel"
            )
            
            if command_exists dnf; then
                dnf group install -y "Development Tools" || true
                for package in "${redhat_packages[@]}"; do
                    if ! rpm -q "$package" >/dev/null 2>&1; then
                        packages_to_install+=("$package")
                    fi
                done
                
                if [[ ${#packages_to_install[@]} -gt 0 ]]; then
                    show_progress "Installing packages: ${packages_to_install[*]}"
                    sudo dnf install -y "${packages_to_install[@]}" || error_exit "Failed to install packages"
                fi
            else
                yum groupinstall -y "Development Tools" || true
                for package in "${redhat_packages[@]}"; do
                    if ! rpm -q "$package" >/dev/null 2>&1; then
                        packages_to_install+=("$package")
                    fi
                done
                
                if [[ ${#packages_to_install[@]} -gt 0 ]]; then
                    show_progress "Installing packages: ${packages_to_install[*]}"
                    sudo yum install -y "${packages_to_install[@]}" || error_exit "Failed to install packages"
                fi
            fi
            ;;
            
        "arch")
            # Essential packages for Arch Linux
            local arch_packages=(
                "base-devel"
                "git"
                "curl"
                "wget"
                "openssl"
                "readline"
                "zlib"
                "libyaml"
                "libxml2"
                "libxslt"
                "libffi"
            )
            
            for package in "${arch_packages[@]}"; do
                if ! pacman -Q "$package" >/dev/null 2>&1; then
                    packages_to_install+=("$package")
                fi
            done
            
            if [[ ${#packages_to_install[@]} -gt 0 ]]; then
                show_progress "Installing packages: ${packages_to_install[*]}"
                sudo pacman -S --noconfirm "${packages_to_install[@]}" || error_exit "Failed to install packages"
            fi
            ;;
    esac
    
    if [[ ${#packages_to_install[@]} -eq 0 ]]; then
        show_success "All core dependencies already installed"
    else
        show_success "Core dependencies installed: ${packages_to_install[*]}"
    fi
}

# Install SQLite (required for Lantae database)
install_sqlite() {
    show_progress "Checking SQLite installation..."
    
    if command_exists sqlite3; then
        show_success "SQLite already installed"
        return
    fi
    
    case "${OS:-unknown}" in
        "macos")
            brew install sqlite3 || error_exit "Failed to install SQLite"
            ;;
        "debian")
            sudo apt-get install -y sqlite3 libsqlite3-dev || error_exit "Failed to install SQLite"
            ;;
        "redhat")
            if command_exists dnf; then
                sudo dnf install -y sqlite sqlite-devel || error_exit "Failed to install SQLite"
            else
                sudo yum install -y sqlite sqlite-devel || error_exit "Failed to install SQLite"
            fi
            ;;
        "arch")
            sudo pacman -S --noconfirm sqlite || error_exit "Failed to install SQLite"
            ;;
    esac
    
    show_success "SQLite installed"
}

# Verify installations
verify_installation() {
    show_progress "Verifying system dependencies..."
    
    local required_commands=("git" "curl" "wget" "make" "sqlite3")
    local missing_commands=()
    
    for cmd in "${required_commands[@]}"; do
        if ! command_exists "$cmd"; then
            missing_commands+=("$cmd")
        fi
    done
    
    if [[ ${#missing_commands[@]} -gt 0 ]]; then
        error_exit "Missing required commands: ${missing_commands[*]}"
    fi
    
    show_success "All system dependencies verified"
}

# Main execution
main() {
    show_progress "Installing system dependencies for ${OS:-unknown}..."
    
    install_package_manager
    install_core_packages
    install_sqlite
    verify_installation
    
    show_success "System dependencies installation complete"
}

main "$@"
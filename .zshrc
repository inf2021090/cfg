# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

#EXPORTS
export LF_ICONS="nerd"

ZSH_THEME="fox"
#plugins=(git archlinux)

source $ZSH/oh-my-zsh.sh

source ~/.zsh/themes/fox.zsh-theme
#fox theme

export PATH="$PATH:$HOME/bin"

# Exports
export PATH=/usr/local/texlive/2024/bin/x86_64-linux:$PATH



# Load zsh-syntax-highlighting
source ~/.zsh/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Load zsh-autosuggestions
source ~/.zsh/custom/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

source ~/aliashes.sh

# rust
. "$HOME/.cargo/env" 

# go
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:~/go/bin


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/kafter/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/kafter/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/kafter/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/kafter/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


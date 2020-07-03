# version control info
autoload -Uz vcs_info
precmd() { vcs_info }

zstyle ':vcs_info:git:*' formats '[%b] '

# default prompt
setopt PROMPT_SUBST
PROMPT='%(?.%F{green}.%F{red})%n%f %B%F{240}%~%f%b ${vcs_info_msg_0_}> '

autoload -U colors && colors

# history
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.cache/zsh/history

# autocomplete case insensitive
autoload -U compinit
zstyle ':completion:*' menu select
#zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=*' 'l:|=* r:|=*'

# history autocomplete on arrow up
autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^[[A" history-beginning-search-backward-end
bindkey "^[[B" history-beginning-search-forward-end

# plugins
source $HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
source $HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
source $HOME/.zsh/zsh-nvm/zsh-nvm.plugin.zsh 2>/dev/null

fpath=($HOME/.zsh/zsh-autocompletions/src $fpath)
rm -f ~/.zcompdump; compinit

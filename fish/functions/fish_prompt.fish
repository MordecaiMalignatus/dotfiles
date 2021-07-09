function fish_prompt
    set -l last_command_status $status
    set -l cwd (prompt_pwd)
    set -l color (set_color $fish_color_normal)
    set -l color_normal (set_color $fish_color_normal)
    set -l color_error (set_color $fish_color_error)

    if test "$last_command_status" -ne 0
        set color "$color_error"
    end

    if test ! -z "$SSH_CLIENT"
        if test 0 -eq (id -u "$USER")
            set color "$color_error"
        end
        echo -sn "$color"(host_info "usr@")"$color_normal"
    end

    if test 0 -eq (id -u "$USER")
        echo -sn "$color_error\$"
    end

    if test "$PWD" = ~
        set color (set_color yellow)
    else if test "$PWD" = /
        set color (set_color yellow)
    end

    echo -sn "$color$cwd"

    if set branch_name (git_branch_name)
        set -l git_color
        set -l git_glyph "╍"

        if git_is_staged
            set git_color (set_color green)

            if git_is_dirty
                set git_glyph "$git_color$git_glyph$color_error$git_glyph"
                set git_color "$color_error"
            end

        else if git_is_dirty
            set git_color "$color_error"

        else if git_is_touched
            set git_color "$color_error"
        else
            set git_color (set_color cyan)
        end

        set -l git_ahead (git_ahead "+" "-" "+-")

        if test "$branch_name" = "master" -o "$branch_name" = "main"
            set branch_name
            if git_is_stashed
                set branch_name "{}"
            end
        else
            set -l left_par "("
            set -l right_par ")"

            if git_is_stashed
                set left_par "{"
                set right_par "}"
            end

            set branch_name " $git_color$left_par$color_normal$branch_name$git_color$right_par"
        end

        echo -sn " $git_color$git_glyph$branch_name$git_ahead"
    end
    echo -sn "$color_normal "
end

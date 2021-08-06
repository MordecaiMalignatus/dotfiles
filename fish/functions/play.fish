function play
        pushd ~/work-music
        if test "$argv[1]" = "pick"
                rake
        else
                rake play
        end
        popd
end

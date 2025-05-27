function make-private-repo
        git config core.sshCommand "ssh -i ~/.ssh/id_private"
        git config user.name "MordecaiMalignatus"
        git config user.email "mordecai@malignat.us"
end

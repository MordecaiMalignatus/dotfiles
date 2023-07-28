function trust-binary -a target -d "unset the apple quarantine on downloaded binaries"
    xattr -d com.apple.quarantine $argv[1]
end

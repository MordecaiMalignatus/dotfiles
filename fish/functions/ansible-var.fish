function ansible-var --argument var pattern
        ansible -i ~/projects/wf-vault-tools/ansible/hostdb.yaml -m debug -a "var=$var" $pattern \
        | awk "/\"$var\"\:/ {print \$2}" \
        | head -n1 \
        | tr -d '"'
end

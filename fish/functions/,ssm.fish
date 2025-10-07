function ,ssm --argument-names profile
        set -l region "eu-central-1"
        set -l profile "htg-platform"
        set -l ec2output (aws ec2 describe-instances --profile $profile --region eu-central-1 | jq '.Reservations[].Instances[]')
        echo $ec2output | jq -r '.PrivateIpAddress' > /tmp/ssm-ip-addresses
        echo $ec2output | jq -r '.InstanceId' > /tmp/ssm-instance-ids
        echo $ec2output | jq -r '.Tags[] | select ( .Key == "Name" ) | .Value' > /tmp/ssm-instance-names

        set -l target (paste /tmp/ssm-ip-addresses /tmp/ssm-instance-ids /tmp/ssm-instance-names | fzf )
        set -l target_instance (echo $target | awk '{print $2}')

        aws ssm start-session --target $target_instance --profile $profile --region $region
end

# Make "go install" go via the appropriate proxy.
set -gx GOPROXY 'https://:@artifactorybase.service.csnzoo.com/artifactory/api/go/go/'
set -gx GOPRIVATE 'github.csnzoo.com'

fish_add_path ~/projects/shed/scripts
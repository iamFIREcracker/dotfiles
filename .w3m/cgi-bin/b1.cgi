#!/usr/bin/env bash

set -euo pipefail

cat <<EOF
Content-type: text/html

<html>
<head>
<title>bunny1 wrapper</title>
</head>
<body>
<form action="http://localhost:9084/">
<input type='search' name='___' value=''>
</form>
</body>
</html>
EOF

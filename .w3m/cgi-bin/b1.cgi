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
<input type='text' name='___' value=''>
<input type='submit' value='try me'>
</form>
</body>
</html>
EOF

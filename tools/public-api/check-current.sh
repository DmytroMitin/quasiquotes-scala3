#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 4 || $# -gt 5 ]]; then
  echo "usage: $0 BASELINE CORE_DOCS_JAR FRONTEND_DOCS_JAR OUTPUT_DIR [report|current-minor]" >&2
  exit 64
fi

tool_root=$(cd "$(dirname "$0")" && pwd)
baseline=$(realpath "$1")
core_docs=$(realpath "$2")
frontend_docs=$(realpath "$3")
output_dir=$(realpath -m "$4")
mode=${5:-current-minor}

case "$mode" in
  report|current-minor) ;;
  *) echo "unsupported mode: $mode" >&2; exit 64 ;;
esac

mkdir -p "$output_dir"
python3 "$tool_root/generate-inventory.py" \
  "$core_docs" "$frontend_docs" "$output_dir/candidate.tsv"
python3 "$tool_root/diff-inventory.py" \
  "$baseline" "$output_dir/candidate.tsv" \
  "$output_dir/delta.tsv" "$output_dir/summary.txt" --mode "$mode"

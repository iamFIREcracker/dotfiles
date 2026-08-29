```
cp /nix/store/i12q6cmbp48i5g0407gdwrp2qnhwcd8k-nixos-unstable.2025-01-14/pkgs/by-name/ll/llama-cpp/package.nix ./llama-cpp-original.nix
```
make changes to llama-cpp.nix
```
nix-build -E 'with import <nixpkgs> {}; callPackage ./llama-cpp.nix { curlSupport = true; }'
```
```
result/bin/llama-server \
    -hf ggml-org/Qwen2.5-Coder-1.5B-Q8_0-GGUF \
    --port 8012 -ngl 99 -fa -ub 1024 -b 1024 -dt 0.1 \
    --ctx-size 0 --cache-reuse 256
# nope

result/bin/llama-cli --hf-repo ggml-org/Qwen2.5-Coder-1.5B-Q8_0-GGUF --hf-file qwen2.5-coder-1.5b-q8_0.gguf -p "The meaning to life and the universe is"
# yes

result/bin/llama-server \
    -hfr ggml-org/Qwen2.5-Coder-1.5B-Q8_0-GGUF \
    --port 8012 -ngl 99 -fa -ub 1024 -b 1024 -dt 0.1 \
    --ctx-size 0 --cache-reuse 256

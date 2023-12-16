#!/bin/bash

# 获取当前目录
current_dir=$(pwd)

# 调用 ocamlformat 格式化当前目录中的文件
ocamlformat -i "$current_dir"

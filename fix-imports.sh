#!/bin/bash

# 対象ディレクトリ
TARGET_DIR="test"

# OSによってsedの挙動が異なるための対応 (macOSのsedはバックアップ引数が必須)
if [[ "$OSTYPE" == "darwin"* ]]; then
  SED_CMD=(sed -i '')
else
  SED_CMD=(sed -i)
fi

echo "Checking imports in $TARGET_DIR..."

# 相対パスのインポート/エクスポートに .js を付与する
# 仕組み:
# 1. from ' または from " で始まるパスを探す
# 2. パスが ./ または ../ で始まっているか確認
# 3. パスの末尾に .js, .ts, .json 等の拡張子がない場合に .js を追加
find "$TARGET_DIR" -type f -name "*.ts" | xargs "${SED_CMD[@]}" -E \
  "s/(from\s+['\"](\.\.?\/[^'\"]+))(['\"])/\1.js\3/g"

# 二重に .js.js となってしまった箇所を修正 (念のためのクリーンアップ)
find "$TARGET_DIR" -type f -name "*.ts" | xargs "${SED_CMD[@]}" -E \
  "s/\.js\.js(['\"])/.js\1/g"

echo "Done! Please check the changes with 'git diff'."
